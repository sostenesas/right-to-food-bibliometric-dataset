# R/analysis/plot_outlet_themes_from_concepts.R
library(arrow)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# -------------------------------------------------
# Helpers (define BEFORE using)
# -------------------------------------------------
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  scale_x_discrete(labels = function(x) gsub(paste0(sep, ".*$"), "", x), ...)
}

# Robustly coerce a vector that may contain list-ish entries to character
as_chr_robust <- function(x) {
  if (is.list(x)) {
    return(vapply(
      x,
      function(v) {
        if (length(v) == 0 || all(is.na(v))) return(NA_character_)
        as.character(v[[1]])
      },
      character(1)
    ))
  }
  as.character(x)
}

# -------------------------------------------------
# 0) Load data
# -------------------------------------------------
works <- arrow::read_parquet("data/silver/works.parquet")
concepts <- arrow::read_parquet("data/silver/concepts.parquet")

# -------------------------------------------------
# 1) Detect venue column robustly
# -------------------------------------------------
venue_col <- dplyr::case_when(
  "host_venue_display_name" %in% names(works) ~ "host_venue_display_name",
  "source_name" %in% names(works) ~ "source_name",
  "source" %in% names(works) ~ "source",
  "journal" %in% names(works) ~ "journal",
  TRUE ~ NA_character_
)

if (is.na(venue_col)) {
  stop(
    "No venue/journal column found in works.parquet.\nAvailable columns:\n",
    paste(names(works), collapse = ", ")
  )
}

works2 <- works %>%
  transmute(
    work_id,
    publication_year = suppressWarnings(as.integer(publication_year)),
    venue_raw = .data[[venue_col]]
  ) %>%
  mutate(
    venue = as_chr_robust(venue_raw),
    venue = str_squish(venue),
    venue = na_if(venue, "")
  ) %>%
  select(-venue_raw)

# -------------------------------------------------
# 2) Map OpenAlex concepts to themes (edit patterns if you want)
# -------------------------------------------------
concepts2 <- concepts %>%
  mutate(
    concept_name_lc = tolower(concept_name),
    theme = case_when(
      str_detect(concept_name_lc,
                 "\\b(law|legal|jurisprudence|direito|human rights|judicial|courts|litigation|judicialization|judicialização)\\b") ~
        "Cross-law journals",

      str_detect(concept_name_lc,
                 "\\b(public health|saúde pública|saude publica|epidemiology|epidemiologia|health policy|saúde coletiva|saude coletiva|primary care|atenção primária|atencao primaria)\\b") ~
        "Public health outlets",

      str_detect(concept_name_lc,
                 "\\b(nutrition|nutrição|nutricao|diet|food|obesity|obesidade|malnutrition|desnutrição|desnutricao)\\b") ~
        "Nutrition journals",

      str_detect(concept_name_lc,
                 "\\b(public policy|policy|governance|development|desenvolvimento|economics|economia|inequality|desigualdade|welfare)\\b") ~
        "Interdisciplinary policy venues",

      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(theme)) %>%
  select(work_id, theme, concept_score)

# -------------------------------------------------
# 3) Dominant theme per work (by summed concept_score)
# -------------------------------------------------
work_theme_scores <- concepts2 %>%
  group_by(work_id, theme) %>%
  summarise(theme_score = sum(concept_score, na.rm = TRUE), .groups = "drop")

dominant_theme <- work_theme_scores %>%
  group_by(work_id) %>%
  slice_max(order_by = theme_score, n = 1, with_ties = FALSE) %>%
  ungroup()

works_classified <- works2 %>%
  left_join(dominant_theme, by = "work_id") %>%
  mutate(theme = if_else(is.na(theme), "Unclassified", theme))

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------
# 4) Plot A: overall theme distribution
# -------------------------------------------------
theme_counts <- works_classified %>% count(theme, sort = TRUE)

p_theme <- ggplot(theme_counts, aes(x = reorder(theme, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Themes based on OpenAlex concepts (dominant theme per work)",
    x = NULL,
    y = "Number of documents"
  ) +
  theme_minimal(base_size = 11)

ggsave("outputs/figures/fig_theme_distribution_concepts.png",
       p_theme, width = 8, height = 5, dpi = 300)

# -------------------------------------------------
# 5) Plot B: Top venues within each theme (ONLY if venues exist)
# -------------------------------------------------
TOP_N <- 10

top_venues <- works_classified %>%
  filter(!is.na(venue)) %>%
  count(theme, venue, sort = TRUE) %>%
  group_by(theme) %>%
  slice_head(n = TOP_N) %>%
  ungroup()

if (nrow(top_venues) == 0) {
  message(
    "No non-missing venues found in works$", venue_col,
    ". Skipping faceted top-venues plot."
  )
} else {
  p_venues <- ggplot(
    top_venues %>%
      group_by(theme) %>%
      mutate(venue = reorder_within(venue, n, theme)) %>%
      ungroup(),
    aes(x = venue, y = n)
  ) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ theme, scales = "free_y") +
    scale_x_reordered() +
    labs(
      title = sprintf("Top %d venues per theme (themes from OpenAlex concepts)", TOP_N),
      x = NULL,
      y = "Number of documents"
    ) +
    theme_minimal(base_size = 11)

  ggsave("outputs/figures/fig_top_venues_by_theme_concepts.png",
         p_venues, width = 10, height = 7, dpi = 300)
}

# -------------------------------------------------
# 6) Plot C: composition over time (share by year)
# -------------------------------------------------
theme_by_year <- works_classified %>%
  filter(!is.na(publication_year)) %>%
  count(publication_year, theme) %>%
  group_by(publication_year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

if (nrow(theme_by_year) == 0) {
  message("No rows for theme-by-year plot (missing publication_year?). Skipping.")
} else {
  p_time <- ggplot(theme_by_year, aes(x = publication_year, y = share, fill = theme)) +
    geom_col() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Theme composition over time (concept-based classification)",
      x = "Publication year",
      y = "Share of documents"
    ) +
    theme_minimal(base_size = 11)

  ggsave("outputs/figures/fig_theme_composition_by_year_concepts.png",
         p_time, width = 9, height = 5, dpi = 300)
}

cat("Saved:\n- outputs/figures/fig_theme_distribution_concepts.png\n",
    if (nrow(top_venues) > 0) "- outputs/figures/fig_top_venues_by_theme_concepts.png\n" else "",
    "- outputs/figures/fig_theme_composition_by_year_concepts.png\n", sep = "")