# ==============================================================================
# SCRIPT: Exploratory bibliometric analysis (robust + HTML wordcloud)
# File: R/analysis/exploratory_analysis.R
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(stringi)      # for accent removal (Latin-ASCII)
  library(tm)
  library(htmlwidgets)
})

# Optional packages: we try to load/install if missing
ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

ensure_pkg("wordcloud2")  # HTML wordcloud
# htmlwidgets is already loaded above, but keep safe
ensure_pkg("htmlwidgets")

# ----------------------------
# Helpers
# ----------------------------
cat_line <- function(...) cat(..., "\n", sep = "")
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

# Normalize text for robust matching (lower + remove accents + squish)
norm_txt <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- tolower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_squish(x)
  x
}

# Robust "contains any keyword" (fixed match, no regex surprises)
contains_any <- function(txt, keys_norm) {
  if (is.na(txt) || !nzchar(txt)) return(FALSE)
  any(stringr::str_detect(txt, stringr::fixed(keys_norm)))
}

# Try to detect project root
detect_project_root <- function() {
  # If running under RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(p) && nzchar(p)) return(dirname(dirname(p)))
  }
  # fallback: current wd
  getwd()
}

# ==============================================================================
# 0) Setup
# ==============================================================================
cat_line("=== ANÁLISE EXPLORATÓRIA BIBLIOMÉTRICA (ROBUST) ===\n")

project_root <- detect_project_root()
setwd(project_root)

cat_line("Diretório do projeto: ", getwd(), "\n")

ensure_dir("outputs/figures")
ensure_dir("outputs/tables")
ensure_dir("outputs/reports")

# ==============================================================================
# 1) Load data
# ==============================================================================
cat_line("--- PARTE 1: Carregamento de Dados ---")

silver_works <- "data/silver/works.parquet"
silver_authorships <- "data/silver/authorships.parquet"
silver_concepts <- "data/silver/concepts.parquet"
silver_references <- "data/silver/references.parquet"

if (!file.exists(silver_works)) {
  stop("❌ Não encontrei: ", silver_works, "\nRode primeiro o pipeline de extração.")
}

cat_line("✓ Carregando dados da camada Silver (Parquet)...")
works <- read_parquet(silver_works)

# Load optional tables if present
authorships <- if (file.exists(silver_authorships)) read_parquet(silver_authorships) else NULL
concepts <- if (file.exists(silver_concepts)) read_parquet(silver_concepts) else NULL
references <- if (file.exists(silver_references)) read_parquet(silver_references) else NULL

cat_line("✓ Dados carregados: ", nrow(works), " documentos\n")

# ==============================================================================
# 1.1) Schema compatibility / safety
# ==============================================================================
# Make sure essential columns exist
if (!"id" %in% names(works) && "work_id" %in% names(works)) {
  works <- works %>% rename(id = work_id)
}
if (!"work_id" %in% names(works) && "id" %in% names(works)) {
  works <- works %>% rename(work_id = id)
}

# Ensure title/display_name exist (for older scripts / compatibility)
if (!"title" %in% names(works)) works$title <- NA_character_
if (!"display_name" %in% names(works)) works$display_name <- NA_character_

# If one exists, backfill the other
works <- works %>%
  mutate(
    title = ifelse(is.na(title) | !nzchar(title), display_name, title),
    display_name = ifelse(is.na(display_name) | !nzchar(display_name), title, display_name)
  )

# Ensure abstract exists
if (!"abstract" %in% names(works)) works$abstract <- NA_character_

# Ensure year/citations exist
if (!"publication_year" %in% names(works)) works$publication_year <- NA_integer_
if (!"cited_by_count" %in% names(works)) works$cited_by_count <- NA_real_

# Coerce types safely
works <- works %>%
  mutate(
    publication_year = suppressWarnings(as.integer(publication_year)),
    cited_by_count = suppressWarnings(as.numeric(cited_by_count))
  )

# ==============================================================================
# 2) Robust thematic filtering
# ==============================================================================
cat_line("--- PARTE 2: Filtragem Temática (robusta) ---")

# Keywords: keep in PT + EN, but match will happen on normalized (no accents)
keywords <- c(
  "direito à alimentação",
  "direito humano à alimentação",
  "direito à alimentação adequada",
  "segurança alimentar",
  "segurança alimentar e nutricional",
  "insegurança alimentar",
  "soberania alimentar",
  "right to food",
  "human right to food",
  "food security",
  "food insecurity"
)

keywords_norm <- norm_txt(keywords)

# Build combined searchable text
works <- works %>%
  mutate(
    .text_raw = paste(
      coalesce(title, ""),
      coalesce(display_name, ""),
      coalesce(abstract, ""),
      sep = " "
    ),
    .text_norm = norm_txt(.text_raw)
  )

filtered <- works %>%
  filter(purrr::map_lgl(.text_norm, contains_any, keys_norm = keywords_norm))

cat_line("✓ Documentos após filtragem: ", nrow(filtered))
cat_line("  Taxa de retenção: ", round(nrow(filtered) / nrow(works) * 100, 1), "%\n")

# If filter returns zero, do diagnostics and continue with full dataset
if (nrow(filtered) == 0) {
  cat_line("⚠ Nenhum documento após filtragem. Vou gerar diagnóstico e seguir sem filtro.\n")

  # show examples of normalized text to help user adjust keywords
  examples <- works %>%
    filter(!is.na(.text_norm) & nzchar(.text_norm)) %>%
    slice_head(n = 8) %>%
    transmute(example = substr(.text_norm, 1, 220))

  readr::write_csv(examples, "outputs/reports/filter_diagnostic_examples.csv")
  cat_line("✓ Exemplos salvos em outputs/reports/filter_diagnostic_examples.csv")

  # Also compute candidate terms (top 50) from titles/abstracts
  text_diag <- works %>%
    transmute(txt = norm_txt(paste(coalesce(title, ""), coalesce(abstract, "")))) %>%
    filter(nzchar(txt)) %>%
    pull(txt)

  # Keep it light: just tokenise with stringr (no tidytext dependency)
  tokens <- unlist(strsplit(text_diag, "\\s+"))
  tokens <- tokens[nchar(tokens) >= 4]
  # remove very common stopwords (already normalized, so use ASCII)
  stop_basic <- c("para","pela","pelo","sobre","entre","como","mais","menos","aqui",
                  "da","de","do","das","dos","uma","um","uns","umas","que","com","sem",
                  "nos","nas","na","no","em","por","ao","aos","as","os","e","ou","se",
                  "sua","seu","suas","seus","tambem","sao","ser","foi","tem","tendo",
                  "direito","alimentacao") # you can remove this if you want to keep
  tokens <- tokens[!tokens %in% stop_basic]
  top_tokens <- sort(table(tokens), decreasing = TRUE)
  df_top_tokens <- tibble(
    token = names(top_tokens)[1:min(80, length(top_tokens))],
    freq = as.integer(top_tokens[1:min(80, length(top_tokens))])
  )
  readr::write_csv(df_top_tokens, "outputs/reports/filter_diagnostic_top_tokens.csv")
  cat_line("✓ Top tokens salvos em outputs/reports/filter_diagnostic_top_tokens.csv\n")

  # Continue with full dataset
  filtered <- works
  cat_line("✓ Prosseguindo com a base completa (sem filtro) para gerar outputs.\n")
}

# Save filtered set (for traceability)
readr::write_csv(
  filtered %>% select(work_id, doi, title, display_name, publication_year, cited_by_count, type, language, source),
  "outputs/tables/works_filtered_minimal.csv"
)

# Narrative review candidates (>=10 citations)
narrative <- filtered %>%
  filter(!is.na(cited_by_count) & cited_by_count >= 10) %>%
  arrange(desc(cited_by_count))

readr::write_csv(
  narrative %>% select(work_id, doi, title, publication_year, cited_by_count, source),
  "outputs/tables/narrative_candidates_citations_ge10.csv"
)

cat_line("✓ Documentos para revisão narrativa (≥10 citações): ", nrow(narrative), "\n")

# ==============================================================================
# 3) Visualizations
# ==============================================================================
cat_line("--- PARTE 3: Visualizações ---")

# 3.1 Production by year
df_year <- filtered %>%
  filter(!is.na(publication_year)) %>%
  count(publication_year, name = "n") %>%
  arrange(publication_year)

p_year_bar <- ggplot(df_year, aes(publication_year, n)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Produção por ano",
    x = "Ano",
    y = "Número de documentos"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/production_by_year_bar.png", p_year_bar, width = 8, height = 5, dpi = 150)

p_year_line <- ggplot(df_year, aes(publication_year, n)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Evolução temporal da produção",
    x = "Ano",
    y = "Número de documentos"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/production_by_year_line.png", p_year_line, width = 8, height = 5, dpi = 150)

# 3.2 Citations by year (if present)
df_cit_year <- filtered %>%
  filter(!is.na(publication_year)) %>%
  group_by(publication_year) %>%
  summarise(total_citations = sum(cited_by_count, na.rm = TRUE), .groups = "drop") %>%
  arrange(publication_year)

p_cit <- ggplot(df_cit_year, aes(publication_year, total_citations)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Citações totais por ano",
    x = "Ano",
    y = "Total de citações"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/figures/citations_by_year.png", p_cit, width = 8, height = 5, dpi = 150)

# 3.3 Types
if ("type" %in% names(filtered)) {
  df_type <- filtered %>%
    mutate(type = if_else(is.na(type) | !nzchar(type), "unknown", type)) %>%
    count(type, name = "n") %>%
    arrange(desc(n)) %>%
    slice_head(n = 20)

  p_type <- ggplot(df_type, aes(reorder(type, n), n)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Top tipos de documentos",
      x = NULL,
      y = "n"
    )

  ggsave("outputs/figures/top_document_types.png", p_type, width = 8, height = 6, dpi = 150)
  readr::write_csv(df_type, "outputs/tables/top_document_types.csv")
}

cat_line("✓ Figuras salvas em outputs/figures/\n")

# ==============================================================================
# 4) Wordcloud in HTML (no Cairo/PNG dependency)
# ==============================================================================
cat_line("--- PARTE 4: Nuvem de palavras (HTML, robusta) ---")

# Choose text source: prefer abstract, else title
text_for_wc <- filtered %>%
  mutate(
    .wc_text = case_when(
      !is.na(abstract) & nzchar(abstract) ~ abstract,
      !is.na(title) & nzchar(title) ~ title,
      TRUE ~ display_name
    ),
    .wc_text = norm_txt(.wc_text)
  ) %>%
  filter(nzchar(.wc_text)) %>%
  pull(.wc_text)

if (length(text_for_wc) == 0) {
  cat_line("⚠ Sem texto suficiente para wordcloud. Pulando.\n")
} else {
  corpus <- tm::VCorpus(tm::VectorSource(text_for_wc))
  corpus <- tm::tm_map(corpus, tm::removePunctuation)
  corpus <- tm::tm_map(corpus, tm::removeNumbers)
  corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords("portuguese"))
  corpus <- tm::tm_map(corpus, tm::stripWhitespace)
  corpus <- corpus[sapply(corpus, function(x) nchar(trimws(x$content)) > 0)]

  tdm <- tm::TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  word_freq <- sort(rowSums(m), decreasing = TRUE)

  df_words <- tibble(
    word = names(word_freq),
    freq = as.numeric(word_freq)
  ) %>%
    filter(nchar(word) >= 3) %>%
    slice_head(n = 250)

  readr::write_csv(df_words, "outputs/tables/word_frequency_top250.csv")

  wc <- wordcloud2::wordcloud2(df_words, size = 0.7)
  htmlwidgets::saveWidget(wc, "outputs/figures/wordcloud.html", selfcontained = TRUE)

  cat_line("✓ Wordcloud salva em outputs/figures/wordcloud.html")
  cat_line("  (Abra esse arquivo no VS Code / browser)\n")
}

cat_line("✓ Análise exploratória finalizada.")
