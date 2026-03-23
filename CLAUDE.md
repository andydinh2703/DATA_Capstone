# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

An interactive R Shiny dashboard ("Geneva Kids Health Dashboard") for a non-profit client, comparing child health indicators across **Geneva (city)**, **Ontario County**, and **New York State** (2007–2021). Data sourced from [NYS Vital Statistics](https://www.health.ny.gov/statistics/vital_statistics/).

## Running the App

From the project root in terminal:
```bash
Rscript -e "shiny::runApp('healthy_beginnings/', port = 3838)"
```

Or from the RStudio console:
```r
shiny::runApp("healthy_beginnings/", port = 3838)
```

Then open http://127.0.0.1:3838.

Install required packages:
```r
install.packages(c("shiny", "bslib", "plotly", "bsicons", "tidyverse", "here"))
```

## Architecture

### Data Flow

`data/` CSVs → `healthy_beginnings/global.R` (load/clean/compute) → `healthy_beginnings/app.R` (UI + server)

**global.R** reads both CSVs with all columns as character, strips commas from numerics, computes Low Birth Weight Rate = (Low / Total) × 100, filters infant mortality to `Age == "Infant"`, and defines the shared color palette and year constants.

**app.R** uses a single pair of reusable functions (`make_tab_ui` / `make_tab_server`) called twice — once for the Low Birth Weight tab and once for the Infant Mortality tab. Each tab has a sidebar with year-range slider and location checkboxes, plus a main panel with 3 KPI value boxes, a Plotly line chart (trend), and a Plotly grouped bar chart. Charts are built with ggplot2 then converted via `ggplotly()`.

### Known Data Issues

- **NYS 2020**: `Total` field contains a comma (`"207,590"`) — cleaned on load in global.R
- **Ontario 2021 Neonatal**: 63 deaths from 979 births is likely a typo (63 → 6); does not affect the Infant tab
- **Geneva volatility**: ~120–170 births/year means a single death changes the rate dramatically — a caveat note is shown in the sidebar

### Color Palette

| Location | Hex       |
|----------|-----------|
| Geneva   | `#E74C3C` |
| Ontario  | `#2ECC71` |
| NYS      | `#3498DB` |

## Future Expansion

Datasets for additional tabs are staged in `data/raw_data/stable home/` (Income, Poverty, HealthInsCoverage, SNAP, FamilyStructure). Planned tabs include **Stable Home** and **Ready Mind** following the same `make_tab_ui` / `make_tab_server` pattern.

## EDA

`EDA/Infant_EDA.Rmd` is a standalone R Markdown document exploring data quality issues and mortality trends — it runs independently of the Shiny app.
