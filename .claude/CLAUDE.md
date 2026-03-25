# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

An interactive R Shiny dashboard ("Ready, Set, GROW!") for the non-profit **Success for Geneva's Children**, comparing child health and well-being indicators across **Geneva (city)**, **Ontario County**, and **New York State** (2007–2021). Data sourced from [NYS Vital Statistics](https://www.health.ny.gov/statistics/vital_statistics/).

## Running the App

From the project root in terminal:
```bash
Rscript -e "shiny::runApp('main/', port = 3838)"
```

Or from the RStudio console:
```r
shiny::runApp("main/", port = 3838)
```

Then open http://127.0.0.1:3838.

### Required Packages
```r
install.packages(c("shiny", "bslib", "plotly", "bsicons", "tidyverse", "here", "tigris", "sf"))
```

## Architecture

### Modular Source-Based Structure

```
DATA_Capstone/
├── main/
│   ├── global.R          # Sources all sub-app globals + module files
│   └── app.R             # Unified UI layout + server wiring (entry point)
├── healthy_beginnings/
│   ├── global.R          # Loads LBW, Infant Mortality, LBW-by-County data
│   ├── infant_mortality.R  # Module: make_im_ui() / make_im_server()
│   └── lbw.R              # Module: make_lbw_ui() / make_lbw_server()
├── stable_home/
│   ├── global.R          # Loads Poverty, Insurance, Family Structure, Income, Population, SNAP, TANF data
│   ├── app.R             # Module: make_sh_tab_ui() / make_sh_tab_server()
│   ├── pop_incom.R       # Module: make_pi_ui() / make_pi_server()
│   └── snap_tanf.R       # Module: make_snap_ui() / make_snap_server()
├── data/
│   └── raw_data/
│       ├── healthy_beginnings/   # LowBirthWeight.csv, Infant Mortality Data.csv, lowbirthweight_bycounty.csv
│       ├── stable home/          # PovertyFamilies.csv, HealthInsCoverage.csv, FamilyStructureUnder6A.csv, Income.csv, Population.csv, SnapOCDoSS.csv, TanfOCDSS.csv + others
│       └── ready_mind/           # (future)
└── EDA/                          # Standalone R Markdown explorations
```

### Data Flow

```
sub-app global.R (load/clean/compute) → main/global.R (source all) → main/app.R (UI + server)
```

Each sub-app's `global.R` owns its own data loading. The main `global.R` simply sources them and then sources all module files. `main/app.R` contains only UI layout and server wiring — no module definitions.

### Module Naming Convention

Module functions follow the pattern `make_<prefix>_ui()` / `make_<prefix>_server()`:

| Module              | UI Function          | Server Function        | File                              |
|---------------------|----------------------|------------------------|-----------------------------------|
| Infant Mortality    | `make_im_ui()`       | `make_im_server()`     | `healthy_beginnings/infant_mortality.R` |
| Low Birth Weight    | `make_lbw_ui()`      | `make_lbw_server()`    | `healthy_beginnings/lbw.R`        |
| Stable Home Tabs    | `make_sh_tab_ui()`   | `make_sh_tab_server()` | `stable_home/app.R`               |
| Pop/Income Index    | `make_pi_ui()`       | `make_pi_server()`     | `stable_home/pop_incom.R`         |
| SNAP/TANF           | `make_snap_ui()`     | `make_snap_server()`   | `stable_home/snap_tanf.R`         |

### Dashboard Tabs

1. **Overview** — Project background and goals
2. **Healthy Born** — Infant Mortality (line + bar) and Low Birth Weight (county heatmap)
3. **Stable Home** — Sub-pills: Poverty, Health Insurance, Family Structure, Population Index, Income Index, SNAP, TANF
4. **Ready Mind** — Placeholder (future expansion)

### Known Data Issues

- **NYS 2020**: `Total` field contains a comma (`"207,590"`) — cleaned on load in `healthy_beginnings/global.R`
- **Ontario 2021 Neonatal**: 63 deaths from 979 births is likely a typo (63 → 6); does not affect the Infant tab
- **Geneva volatility**: ~120–170 births/year means a single death changes the rate dramatically — a caveat note is shown in the sidebar

### Color Palette

| Location | Hex       |
|----------|-----------|
| Geneva   | `#E74C3C` |
| Ontario  | `#2ECC71` |
| NYS      | `#3498DB` |

### Key Technical Notes

- All file paths use `here::here()` — the working directory must be the project root (`DATA_Capstone/`)
- The `stable home` data directory has a space in its name: `data/raw_data/stable home/`
- `tigris` and `sf` are used for geospatial county boundaries in the LBW heatmap
- Module files do **not** contain `shinyApp()` calls — they are pure module definitions sourced by `main/global.R`

## EDA

`EDA/Infant_EDA.Rmd` is a standalone R Markdown document exploring data quality issues and mortality trends — it runs independently of the Shiny app.

## Future Expansion

The **Ready Mind** tab is next, following the same module pattern: create `ready_mind/global.R` for data, module files for UI/server, source them in `main/global.R`, and wire into `main/app.R`.
