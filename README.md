# Brazilian Academic Production on the Right to Food (2014–2023)
Reproducible bibliometric dataset and pipeline based on OpenAlex.

## Dataset (Zenodo)
DOI: 10.5281/zenodo.18462393

## Repository structure
- `R/scripts/` — data collection and processing scripts
- `R/validation/` — validation metrics and automated checks
- `outputs/figures/` — figures used in the manuscript
- `outputs/validation/` — validation tables and logs
- `paper/` — manuscript (LaTeX/PDF)
- `docs/` — user guides

## OpenAlex API key (required)

This project requires an OpenAlex API key.

1. Create your own key at: https://openalex.org
2. Set it as an environment variable.

### Option A: Using .Renviron (recommended)
Copy `.Renviron.example` to `.Renviron` and fill your key:

OPENALEX_API_KEY=YOUR_KEY_HERE

Do not commit `.Renviron`.

### Option B: In an R session
```r
Sys.setenv(OPENALEX_API_KEY = "YOUR_KEY_HERE")

## Quick start (R)
```r
source("R/scripts/00_setup.R")
targets::tar_make()

## License

Code: MIT License  
Data: CC-BY 4.0 (see Zenodo DOI)
