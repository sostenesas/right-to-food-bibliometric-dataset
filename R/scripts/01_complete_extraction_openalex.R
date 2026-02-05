# ==============================================================================
# 01_complete_extraction_openalex.R
# OpenAlex API → Bronze (JSONL raw) → Silver (Parquet/CSV) → Gold (aggregations)
# Exposes: run_complete_extraction()
# ==============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(arrow)
  library(readr)
  library(lubridate)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# ----------------------------
# Helpers
# ----------------------------
cat_line <- function(...) cat(..., "\n", sep = "")
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
timestamp_now <- function() format(Sys.time(), "%Y-%m-%d_%H%M%S")

# Parse a JSONL line into an R list (never simplify to vectors)
safe_fromJSON <- function(x) {
  tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
}

# Coerce anything to a single character scalar (never return json/list objects)
to_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.character(x)) return(x[[1]])
  if (inherits(x, "json")) return(as.character(x))
  if (is.list(x)) return(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")))
  out <- suppressWarnings(as.character(x))
  if (length(out) == 0) NA_character_ else out[[1]]
}

# IMPORTANT: write JSONL as raw JSON strings (one object per line)
# We force result to be a plain character line.
append_jsonl <- function(path, obj) {
  line <- as.character(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", digits = NA))
  write(line, file = path, append = TRUE)
}

# Normalize abstract into plain text (character)
normalize_abstract_text <- function(r) {
  # 1) Direct 'abstract' field (if present)
  a <- r$abstract %||% NULL
  if (!is.null(a)) return(to_chr(a))

  # 2) OpenAlex typical: abstract_inverted_index
  ai <- r$abstract_inverted_index %||% NULL
  if (is.null(ai)) return(NA_character_)
  if (!is.list(ai)) return(to_chr(ai))

  # Reconstruct text from inverted index (word -> positions)
  tryCatch({
    max_pos <- max(unlist(ai, use.names = FALSE)) + 1L
    out <- rep("", max_pos)

    for (w in names(ai)) {
      pos <- ai[[w]]
      if (length(pos) > 0) out[pos + 1L] <- w
    }

    txt <- stringr::str_squish(paste(out, collapse = " "))
    if (nchar(txt) == 0) NA_character_ else txt
  }, error = function(e) {
    to_chr(ai)
  })
}

# ----------------------------
# Configuration
# ----------------------------
BASE_URL <- "https://api.openalex.org/works"

DIR_BRONZE <- "data/bronze"
DIR_SILVER <- "data/silver"
DIR_GOLD   <- "data/gold"

ensure_dir(DIR_BRONZE)
ensure_dir(DIR_SILVER)
ensure_dir(DIR_GOLD)

OPENALEX_API_KEY <- Sys.getenv("OPENALEX_API_KEY", unset = "")

# ----------------------------
# Query builder
# - text terms go in search=
# - structured filters go in filter=
# ----------------------------
build_openalex_query <- function(use_language_filter = TRUE) {

  search_terms <- c(
    '"direito à alimentação"',
    '"direito humano à alimentação"',
    '"direito à alimentação adequada"',
    '"segurança alimentar"',
    '"segurança alimentar e nutricional"',
    '"insegurança alimentar"',
    '"right to food"',
    '"human right to food"',
    '"food security"',
    '"food insecurity"'
  )

  search_query <- paste0("(", paste(search_terms, collapse = " OR "), ")")

  filter_parts <- c(
    "authorships.institutions.country_code:BR",
    "from_publication_date:2014-01-01",
    "to_publication_date:2023-12-31"
  )

  if (isTRUE(use_language_filter)) {
    filter_parts <- c(filter_parts, "language:pt")
  }

  filter_query <- paste(filter_parts, collapse = ",")

  list(search = search_query, filter = filter_query)
}

# ----------------------------
# STEP 1: Bronze (collect JSONL raw)
# ----------------------------
collect_openalex_bronze <- function(out_jsonl,
                                   search_query,
                                   filter_query,
                                   polite_email = "",
                                   per_page = 200,
                                   delay_sec = 0.2,
                                   max_pages = Inf) {

  headers <- add_headers("User-Agent" = "right-to-food-bibliometric-dataset (R script)")
  if (nzchar(OPENALEX_API_KEY)) {
    headers <- add_headers(
      "User-Agent" = "right-to-food-bibliometric-dataset (R script)",
      "Authorization" = paste("Bearer", OPENALEX_API_KEY)
    )
  }

  if (file.exists(out_jsonl)) file.remove(out_jsonl)

  cursor <- "*"
  page <- 0
  total_records <- 0

  cat_line("\n--- STEP 1: BRONZE LAYER (OpenAlex API → JSONL) ---")
  cat_line("Starting data collection from OpenAlex...")
  cat_line("Search: ", search_query)
  cat_line("Filter: ", filter_query)
  cat_line("Output: ", out_jsonl, "\n")

  repeat {
    page <- page + 1
    if (page > max_pages) {
      cat_line("Reached max_pages limit: ", max_pages)
      break
    }

    q <- list(
      search = search_query,
      filter = filter_query,
      cursor = cursor,
      `per-page` = per_page
    )
    if (nzchar(polite_email)) q$mailto <- polite_email

    res <- GET(BASE_URL, headers, query = q)

    if (status_code(res) != 200) {
      txt <- content(res, as = "text", encoding = "UTF-8")
      stop("OpenAlex request failed (HTTP ", status_code(res), "): ", txt)
    }

    js <- jsonlite::fromJSON(content(res, as = "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)

    results <- js$results
    next_cursor <- js$meta$next_cursor
    n <- length(results)

    cat_line("Page ", page, "... results: ", n)

    if (n == 0) {
      cat_line("No more results.")
      break
    }

    # Save each record as raw JSON line
    for (i in seq_len(n)) append_jsonl(out_jsonl, results[[i]])
    total_records <- total_records + n

    if (is.null(next_cursor) || !nzchar(next_cursor)) {
      cat_line("No next_cursor. Stopping.")
      break
    }

    cursor <- next_cursor
    Sys.sleep(delay_sec)
  }

  cat_line("\n✓ Data collection complete!")
  cat_line("Total pages fetched: ", page)
  cat_line("Total records: ", total_records)
  cat_line("Output file: ", out_jsonl, "\n")

  if (total_records == 0) {
    stop(
      "OpenAlex returned 0 records.\n",
      "Try use_language_filter = FALSE (language filter may be too restrictive),\n",
      "or simplify the search terms."
    )
  }

  invisible(list(out_file = out_jsonl, pages = page, total_records = total_records))
}

# ----------------------------
# STEP 2: Silver (JSONL → normalized tables)
# ----------------------------
build_silver_from_jsonl <- function(bronze_jsonl,
                                   dir_silver = DIR_SILVER,
                                   write_csv = TRUE,
                                   keep_abstract_inverted_index_json = TRUE) {

  cat_line("\n--- STEP 2: SILVER LAYER (Normalized Tables) ---")
  cat_line("Building silver layer from: ", bronze_jsonl)

  if (!file.exists(bronze_jsonl)) stop("Bronze file not found: ", bronze_jsonl)

  lines <- readLines(bronze_jsonl, warn = FALSE)
  cat_line("Total lines: ", length(lines))
  if (length(lines) == 0) stop("Bronze JSONL has 0 lines: ", bronze_jsonl)

  records <- purrr::map(lines, safe_fromJSON) |> purrr::compact()
  cat_line("Total records parsed: ", length(records))
  if (length(records) == 0) stop("Parsed 0 JSON records from bronze JSONL.")

  # Works
  cat_line("\nBuilding works table...")
  works <- purrr::map_dfr(records, function(r) {
    ai_json <- if (isTRUE(keep_abstract_inverted_index_json)) {
      to_chr(r$abstract_inverted_index %||% NULL)
    } else {
      NA_character_
    }

    tibble(
      work_id = r$id %||% NA_character_,
      doi = r$doi %||% NA_character_,
      title = r$title %||% NA_character_,
      display_name = r$display_name %||% r$title %||% NA_character_,
      abstract = normalize_abstract_text(r),  # ALWAYS character
      abstract_inverted_index_json = ai_json, # optional (string JSON)
      publication_year = r$publication_year %||% NA_integer_,
      publication_date = r$publication_date %||% NA_character_,
      type = r$type %||% NA_character_,
      language = r$language %||% NA_character_,
      cited_by_count = r$cited_by_count %||% NA_integer_,
      is_oa = r$open_access$is_oa %||% NA,
      source = r$host_venue$display_name %||% NA_character_
    )
  })

  # Final type safety (extra hardening)
  works <- works %>%
    mutate(
      abstract = to_chr(abstract),
      abstract_inverted_index_json = to_chr(abstract_inverted_index_json)
    )

  cat_line("  Works: ", nrow(works))

  # Authorships
  cat_line("Building authorships table...")
  authorships <- purrr::map_dfr(records, function(r) {
    w <- r$id %||% NA_character_
    auths <- r$authorships
    if (is.null(auths) || length(auths) == 0) return(tibble())

    purrr::map_dfr(auths, function(a) {
      author_id <- a$author$id %||% NA_character_
      author_name <- a$author$display_name %||% NA_character_

      insts <- a$institutions
      if (is.null(insts) || length(insts) == 0) {
        tibble(
          work_id = w,
          author_id = author_id,
          author_name = author_name,
          institution_id = NA_character_,
          institution_name = NA_character_,
          institution_country_code = NA_character_
        )
      } else {
        purrr::map_dfr(insts, function(i) {
          tibble(
            work_id = w,
            author_id = author_id,
            author_name = author_name,
            institution_id = i$id %||% NA_character_,
            institution_name = i$display_name %||% NA_character_,
            institution_country_code = i$country_code %||% NA_character_
          )
        })
      }
    })
  })
  cat_line("  Authorships: ", nrow(authorships))

  # Concepts
  cat_line("Building concepts table...")
  concepts <- purrr::map_dfr(records, function(r) {
    w <- r$id %||% NA_character_
    cs <- r$concepts
    if (is.null(cs) || length(cs) == 0) return(tibble())
    purrr::map_dfr(cs, function(cn) {
      tibble(
        work_id = w,
        concept_id = cn$id %||% NA_character_,
        concept_name = cn$display_name %||% NA_character_,
        concept_level = cn$level %||% NA_integer_,
        concept_score = cn$score %||% NA_real_
      )
    })
  })
  cat_line("  Concepts: ", nrow(concepts))

  # References
  cat_line("Building references table...")
  references <- purrr::map_dfr(records, function(r) {
    w <- r$id %||% NA_character_
    refs <- r$referenced_works
    if (is.null(refs) || length(refs) == 0) return(tibble())
    tibble(
      work_id = w,
      referenced_work_id = unlist(refs)
    )
  })
  cat_line("  References: ", nrow(references))

  # Save Parquet
  cat_line("\nSaving Parquet files...")
  write_parquet(works, file.path(dir_silver, "works.parquet"))
  write_parquet(authorships, file.path(dir_silver, "authorships.parquet"))
  write_parquet(concepts, file.path(dir_silver, "concepts.parquet"))
  write_parquet(references, file.path(dir_silver, "references.parquet"))
  cat_line("✓ Silver layer complete!")

  # Save CSV
  if (isTRUE(write_csv)) {
    cat_line("\nSaving CSV files...")
    readr::write_csv(works, file.path(dir_silver, "works.csv"))
    readr::write_csv(authorships, file.path(dir_silver, "authorships.csv"))
    readr::write_csv(concepts, file.path(dir_silver, "concepts.csv"))
    readr::write_csv(references, file.path(dir_silver, "references.csv"))
    cat_line("✓ CSV files saved!")
  }

  invisible(list(works = works, authorships = authorships, concepts = concepts, references = references))
}

# ----------------------------
# STEP 3: Gold (analytical tables)
# ----------------------------
build_gold <- function(silver, dir_gold = DIR_GOLD, write_csv = TRUE) {

  cat_line("\n--- STEP 3: GOLD LAYER (Analytical Tables) ---")
  cat_line("Building gold layer...")

  works <- silver$works
  concepts <- silver$concepts

  production_by_year <- works |>
    filter(!is.na(publication_year)) |>
    count(publication_year, name = "n") |>
    arrange(publication_year)

  production_by_type <- works |>
    mutate(type = if_else(is.na(type) | type == "", "unknown", type)) |>
    count(type, name = "n") |>
    arrange(desc(n))

  top_sources <- works |>
    mutate(source = if_else(is.na(source) | source == "", "unknown", source)) |>
    count(source, name = "n") |>
    arrange(desc(n)) |>
    slice_head(n = 20)

  top_concepts <- concepts |>
    filter(!is.na(concept_name) & concept_name != "") |>
    count(concept_name, name = "n") |>
    arrange(desc(n)) |>
    slice_head(n = 30)

  concept_edges <- concepts |>
    filter(!is.na(concept_name) & concept_name != "") |>
    distinct(work_id, concept_name) |>
    group_by(work_id) |>
    summarise(concepts = list(sort(unique(concept_name))), .groups = "drop") |>
    mutate(pairs = purrr::map(concepts, function(v) {
      if (length(v) < 2) return(NULL)
      combn(v, 2, simplify = FALSE)
    })) |>
    select(pairs) |>
    tidyr::unnest(pairs) |>
    mutate(
      source = purrr::map_chr(pairs, 1),
      target = purrr::map_chr(pairs, 2)
    ) |>
    select(source, target) |>
    count(source, target, name = "weight") |>
    arrange(desc(weight))

  # Save
  write_parquet(production_by_year, file.path(dir_gold, "production_by_year.parquet"))
  write_parquet(production_by_type, file.path(dir_gold, "production_by_type.parquet"))
  write_parquet(top_sources, file.path(dir_gold, "top_sources.parquet"))
  write_parquet(top_concepts, file.path(dir_gold, "top_concepts.parquet"))
  write_parquet(concept_edges, file.path(dir_gold, "concept_cooccurrence_edges.parquet"))

  if (isTRUE(write_csv)) {
    readr::write_csv(production_by_year, file.path(dir_gold, "production_by_year.csv"))
    readr::write_csv(production_by_type, file.path(dir_gold, "production_by_type.csv"))
    readr::write_csv(top_sources, file.path(dir_gold, "top_sources.csv"))
    readr::write_csv(top_concepts, file.path(dir_gold, "top_concepts.csv"))
    readr::write_csv(concept_edges, file.path(dir_gold, "concept_cooccurrence_edges.csv"))
  }

  cat_line("✓ Gold layer complete!")

  invisible(list(
    production_by_year = production_by_year,
    production_by_type = production_by_type,
    top_sources = top_sources,
    top_concepts = top_concepts,
    concept_cooccurrence_edges = concept_edges
  ))
}

# ----------------------------
# Public entry point: run everything
# ----------------------------
run_complete_extraction <- function(max_pages = NULL,
                                    polite_email = "",
                                    use_language_filter = TRUE,
                                    per_page = 200,
                                    delay_sec = 0.2,
                                    write_csv = TRUE,
                                    keep_abstract_inverted_index_json = TRUE) {

  cat_line("================================================================================")
  cat_line("COMPLETE DATA EXTRACTION PIPELINE")
  cat_line("OpenAlex API → Bronze(JSONL raw) → Silver(Parquet/CSV) → Gold")
  cat_line("================================================================================\n")

  q <- build_openalex_query(use_language_filter = use_language_filter)

  bronze_file <- file.path(DIR_BRONZE, paste0("openalex_bronze_", timestamp_now(), ".jsonl"))

  t0 <- Sys.time()

  bronze <- collect_openalex_bronze(
    out_jsonl = bronze_file,
    search_query = q$search,
    filter_query = q$filter,
    polite_email = polite_email,
    per_page = per_page,
    delay_sec = delay_sec,
    max_pages = if (is.null(max_pages)) Inf else max_pages
  )

  silver <- build_silver_from_jsonl(
    bronze_jsonl = bronze$out_file,
    write_csv = write_csv,
    keep_abstract_inverted_index_json = keep_abstract_inverted_index_json
  )

  gold <- build_gold(silver = silver, write_csv = write_csv)

  t1 <- Sys.time()

  cat_line("\n================================================================================")
  cat_line("✓ PIPELINE COMPLETE!")
  cat_line("================================================================================")
  cat_line("Elapsed time: ", round(as.numeric(difftime(t1, t0, units = "mins")), 3), " minutes\n")

  list(
    bronze = bronze$out_file,
    silver = list(
      works = file.path(DIR_SILVER, "works.parquet"),
      authorships = file.path(DIR_SILVER, "authorships.parquet"),
      concepts = file.path(DIR_SILVER, "concepts.parquet"),
      references = file.path(DIR_SILVER, "references.parquet")
    ),
    gold = list(
      production_by_year = file.path(DIR_GOLD, "production_by_year.parquet"),
      production_by_type = file.path(DIR_GOLD, "production_by_type.parquet"),
      top_sources = file.path(DIR_GOLD, "top_sources.parquet"),
      top_concepts = file.path(DIR_GOLD, "top_concepts.parquet"),
      concept_cooccurrence_edges = file.path(DIR_GOLD, "concept_cooccurrence_edges.parquet")
    ),
    elapsed_time = difftime(t1, t0, units = "mins")
  )
}
