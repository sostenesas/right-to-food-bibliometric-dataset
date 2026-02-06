# Brazilian Academic Production on the Right to Food (2014–2023)
Reproducible bibliometric dataset and pipeline based on OpenAlex.

## Dataset (Zenodo)
DOI: 10.5281/zenodo.18500452

## Repository structure
- `R/scripts/` — data collection and processing scripts
- `R/validation/` — validation metrics and automated checks
- `outputs/figures/` — figures used in the manuscript
- `outputs/validation/` — validation tables and logs
- `paper/` — manuscript (LaTeX)
- `docs/` — user guides

## Quick start (R)
```r
source("R/scripts/99_run_pipeline.R")

## License

Code: MIT License  
Data: CC-BY 4.0 (see Zenodo DOI)
