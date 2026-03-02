# Example Figures

R scripts that produce publication-quality visualizations from the IPEDS
database, following [Kieran Healy's](https://kieranhealy.org/publications/)
data visualization principles (minimal chrome, high data-ink ratio, direct
annotation, no legend boxes).

## Running

Run from the **project root** (where `ipeds.duckdb` lives):

```bash
cd /path/to/ipeds-database
Rscript examples/figures/degree_plots.R
```

Output PNGs are written to the current directory.

## Requirements

R packages: `ggplot2`, `duckdb`, `scales`, `ragg`, `dplyr`, `tidyr`.

```r
install.packages(c("ggplot2", "duckdb", "scales", "ragg", "dplyr", "tidyr"))
```

## Scripts

| Script | Description | Output |
|--------|-------------|--------|
| `degree_plots.R` | BA, MBA, and PhD total degrees 2000–2024 | `ba_degrees.png`, `mba_degrees.png`, `phd_degrees.png` |
| `mba_expanded_plot.R` | MBA vs. competing business master's (line chart) | `mba_expanded.png` |
| `mba_stacked_plot.R` | Stacked area: composition of business master's | `mba_stacked.png` |
| `econ_masters_plot.R` | Economics master's reclassification story | `econ_masters.png` |
| `residence_plot.R` | In-state vs. out-of-state enrollment trends | `residence_pct.png`, `residence_count.png` |
