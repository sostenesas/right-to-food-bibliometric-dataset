# ==============================================================================
# 00_setup_openalex.R
# - Loads packages
# - Reads OpenAlex API key from environment
# - Provides clear error message for reproducibility
# ==============================================================================

# Packages
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(arrow)
  library(purrr)
  library(stringr)
  library(openalexR)
})

# ------------------------------------------------------------------------------
# OpenAlex authentication (DO NOT hardcode keys in the repo)
# ------------------------------------------------------------------------------
OPENALEX_API_KEY <- Sys.getenv("OPENALEX_API_KEY", unset = "")

if (!nzchar(OPENALEX_API_KEY)) {
  stop(
    "No OpenAlex API key found (OPENALEX_API_KEY).\n\n",
    "To reproduce this project:\n",
    "  1) Create your own key at https://openalex.org\n",
    "  2) Set it as an environment variable, e.g. in R:\n",
    "     Sys.setenv(OPENALEX_API_KEY = 'YOUR_KEY_HERE')\n\n",
    "Tip: Copy .Renviron.example -> .Renviron (do NOT commit .Renviron)."
  )
}

# Optional (polite email; also from env)
OPENALEX_EMAIL <- Sys.getenv("OPENALEX_EMAIL", unset = "")
