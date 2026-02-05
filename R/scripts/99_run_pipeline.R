# ==============================================================================
# 99_run_pipeline.R
# Entry point to run the complete OpenAlex → Bronze → Silver → Gold pipeline
# ==============================================================================

cat("\n================ RUNNING FULL PIPELINE ================\n")

# 1) Garantir que estamos no root do projeto
if (!file.exists("R/scripts/01_complete_extraction_openalex.R")) {
  stop(
    "You must run this script from the project root directory.\n",
    "Current working directory: ", getwd()
  )
}

# 2) Carregar pipeline completo
source("R/scripts/01_complete_extraction_openalex.R", encoding = "UTF-8")

# 3) Mostrar configuração básica
cat("OpenAlex API key loaded: ", ifelse(nzchar(Sys.getenv("OPENALEX_API_KEY")), "YES", "NO"), "\n")
cat("Polite email: ", ifelse(nzchar(Sys.getenv("OPENALEX_EMAIL")), Sys.getenv("OPENALEX_EMAIL"), "NONE"), "\n")

# 4) Rodar pipeline
results <- run_complete_extraction(
  polite_email = Sys.getenv("OPENALEX_EMAIL", ""),
  use_language_filter = TRUE,     # se voltar 0 resultados, troque para FALSE
  per_page = 200,
  delay_sec = 0.2,
  write_csv = TRUE,
  keep_abstract_inverted_index_json = TRUE
)

# 5) Resumo final
cat("\n================ PIPELINE FINISHED ================\n")
print(results)

cat("\nCheck outputs:\n")
cat("Bronze:  ", results$bronze, "\n")
cat("Silver:  data/silver/*.parquet\n")
cat("Gold:    data/gold/*.parquet\n")
cat("====================================================\n")
