# Geneva Kids Health Dashboard

An interactive R Shiny dashboard comparing child health indicators across
**Geneva**, **Ontario County**, and **New York State** (2007–2021).

---

## Project Structure

```
DATA_Capstone/
├── app/
│   ├── global.R        # Data loading, cleaning, shared objects
│   └── app.R           # Shiny UI + server (two-tab dashboard)
├── data/
│   ├── LowBirthWeight.csv
│   └── Infant Mortality Data.csv
├── EDA/
│   └── Infant_EDA.Rmd  # Exploratory data analysis
├── DATA_Capstone.Rproj
└── README.md
```

---

## How to Run

From the **project root** in your terminal:

```bash
Rscript -e "shiny::runApp('app/', port = 3838)"
```

Or from the **RStudio console**:

```r
shiny::runApp("app/", port = 3838)
```

Then open **http://127.0.0.1:3838** in a browser.

---

## Required R Packages

| Package    | Purpose                              |
|------------|--------------------------------------|
| `shiny`    | Web application framework            |
| `bslib`    | Bootstrap 5 theming & layout         |
| `plotly`   | Interactive charts                   |
| `bsicons`  | Bootstrap icons for value boxes      |
| `tidyverse`| Data wrangling (`dplyr`, `ggplot2`, `readr`, etc.) |
| `here`     | Robust file path resolution          |

Install any missing packages with:

```r
install.packages(c("shiny", "bslib", "plotly", "bsicons", "tidyverse", "here"))
```

---

## Data Sources

All data comes from the [NYS Vital Statistics](https://www.health.ny.gov/statistics/vital_statistics/) tables.

### `LowBirthWeight.csv`

| Column   | Description                          |
|----------|--------------------------------------|
| Location | Geneva, Ontario, or NYS              |
| Source   | URL of the original NYS table        |
| Year     | 2007–2021                            |
| Total    | Total live births                    |
| Low      | Number of low birth weight births    |

The app computes: **Rate = Low / Total × 100** (percentage).

### `Infant Mortality Data.csv`

| Column   | Description                              |
|----------|------------------------------------------|
| Location | Geneva, Ontario, or NYS                  |
| Source   | URL of the original NYS table            |
| Year     | 2007–2021                                |
| Total    | Total live births                        |
| Deaths   | Number of deaths                         |
| Rate     | Deaths per 1,000 live births             |
| Age      | `Infant` (first year) or `Neonatal` (first 28 days) |

The app filters to **Age == "Infant"** only.

#### Known Data Issues

- **NYS 2020**: `Total` field contains a comma (`"207,590"`). Cleaned on load.
- **Ontario 2021 Neonatal**: Shows 63 deaths from 979 births — likely a typo
  (63 instead of 6). Does not affect the Infant tab.
- **Geneva volatility**: With only ~120–170 births per year, a single death
  changes the rate dramatically. A caveat note is displayed in the sidebar.

---

## App Architecture

### `global.R` — Data Loading

1. Reads both CSVs using `readr::read_csv()` (all columns as character first)
2. Cleans numeric fields (strips commas, converts types)
3. Computes low birth weight rate for `lbw` dataframe
4. Filters infant mortality to `Age == "Infant"` for `im` dataframe
5. Defines shared color palette and year range constants

### `app.R` — UI & Server

The app uses **Shiny modules** for code reuse. A single pair of functions
(`make_tab_ui` / `make_tab_server`) is called twice — once for each tab.

#### UI (`page_navbar`)

- **Tab bar**: Low Birth Weight | Infant Mortality
- **Each tab** uses `layout_sidebar()`:
  - **Sidebar**: Year range slider, location checkboxes, caveat note
  - **Main panel**:
    - 3 value boxes (KPI cards) showing the latest year's rate per location
    - Plotly line chart — trend over time with hover tooltips
    - Plotly grouped bar chart — year-by-year side-by-side comparison

#### Server (modular)

- `filtered()` reactive: subsets data by selected year range and locations
- `latest_year()` reactive: the most recent year in the filtered range
- KPI values: extracted from the latest year for each location
- Line chart: `ggplot2` → `ggplotly()` conversion
- Bar chart: `ggplot2` → `ggplotly()` conversion

#### Color Palette

| Location | Color   | Hex       |
|----------|---------|-----------|
| Geneva   | Red     | `#E74C3C` |
| Ontario  | Green   | `#2ECC71` |
| NYS      | Blue    | `#3498DB` |

---

## Future Expansion

The dashboard is designed to be extended. Potential additions:

- **Neonatal Mortality** toggle within the Infant Mortality tab
- **Overview** tab with summary statistics across all indicators
- **Stable Home** / **Ready Mind** tabs for additional health domains
- Custom CSS styling (a `www/styles.css` can be added later)
- Geographic map visualization (using `leaflet` with NY shapefiles)
