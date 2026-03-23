# Stable Home Dashboard — Design Spec

**Date:** 2026-03-23
**Status:** Approved

---

## Overview

A standalone R Shiny app (`stable_home/`) that lets non-profit staff and community stakeholders explore household stability indicators across **Geneva (city)**, **Ontario County**, and **New York State**. The central question a user should be able to answer: *"Does household stability influence kids' success?"*

The app mirrors the structure of the existing `healthy_beginnings/` app — same module pattern, same color palette, same chart style — so the two apps feel like a coherent suite.

---

## App Structure

```
stable_home/
├── app.R        # make_tab_ui / make_tab_server + page_navbar UI + server
└── global.R     # data loading and cleaning
```

**To run:**
```bash
Rscript -e "shiny::runApp('stable_home/', port = 3839)"
```

---

## Data (`global.R`)

All three source CSVs live in `data/raw_data/stable home/`.

### `poverty_df` — from `PovertyFamilies.csv`
- Filter to `Community %in% c("Geneva", "Ontario", "NYS")` — drop sub-geographies ("Geneva GCSD", "Geneva Town", "Ontario wo Geneva", etc.)
- Rename `Community` → `Location`
- Keep `Year` (integer) and `PCT` (family poverty rate %)

### `insurance_df` — from `HealthInsCoverage.csv`
- Normalize `Community`: replace `"City of Geneva"` → `"Geneva"` for consistency with the rest of the app
- Keep `Year` (integer) and `PCT` (% of population with health insurance)

### `family_df` — from `FamilyStructureUnder6A.csv`
- Filter to `Community %in% c("Geneva", "Ontario", "NYS")`
- Filter to `Type %in% c("Two parents", "Single mother", "Single father")` — drop `"All"`
- Rename `Community` → `Location`
- Compute `PCT = Proportion * 100` (% of households with children under 6 that are this household type)
- Keep `Year` (integer), `Type`, and `PCT`
- Note: data is sparse — only years 2010, 2012, 2013, 2014, 2015, 2017 are present

### Shared constants (reuse from `healthy_beginnings/global.R`)
```r
location_colors <- c(Geneva = "#E74C3C", Ontario = "#2ECC71", NYS = "#3498DB")
```

---

## UI (`app.R`)

### Top-level structure

```r
page_navbar(
  title = "Stable Home Dashboard",
  nav_panel("Stable Home", stable_home_ui())
)
```

Or equivalently, the `navset_pill` can sit directly inside `page_navbar` as a single `nav_panel`.

### `make_tab_ui` — extended signature

Add one optional parameter to the existing function:

```r
make_tab_ui <- function(id, title, year_min, year_max, metric_label,
                        extra_controls = NULL) {
  # existing layout_sidebar() body
  # extra_controls appended at the bottom of the sidebar if non-NULL
}
```

All existing calls pass no `extra_controls` and are unaffected.

### Three pill sub-tabs

```r
navset_pill(
  nav_panel("Poverty",
    make_tab_ui("poverty",
      title        = "Poverty Rate",
      year_min     = 2010, year_max = 2023,
      metric_label = "% families in poverty"
    )
  ),
  nav_panel("Health Insurance",
    make_tab_ui("insurance",
      title        = "Health Insurance Coverage",
      year_min     = 2012, year_max = 2023,
      metric_label = "% with health insurance"
    )
  ),
  nav_panel("Family Structure",
    make_tab_ui("family",
      title        = "Family Structure (children under 6)",
      year_min     = 2010, year_max = 2017,
      metric_label = "% of households",
      extra_controls = checkboxGroupInput(
        "family_types",
        label    = "HOUSEHOLD TYPE",
        choices  = c("Two parents", "Single mother", "Single father"),
        selected = c("Two parents", "Single mother", "Single father")
      )
    )
  )
)
```

### KPI value boxes (per sub-tab)

Three value boxes (one per location) showing the latest year's metric:

| Sub-tab | Value displayed |
|---|---|
| Poverty | Latest year poverty rate % |
| Health Insurance | Latest year insurance coverage % |
| Family Structure | Latest year PCT for the first checked household type (falls back to "Two parents") |

A helper note below the Family Structure KPI boxes: *"Values shown for [selected type]"*

---

## Server (`app.R`)

### `make_tab_server` — extended signature

Add one optional parameter:

```r
make_tab_server <- function(id, data, metric_col = "PCT",
                            extra_filter = NULL) {
  moduleServer(id, function(input, output, session) {
    filtered <- reactive({
      df <- data |>
        filter(Year >= input$year_range[1],
               Year <= input$year_range[2],
               Location %in% input$locations)
      if (!is.null(extra_filter)) df <- extra_filter(df, input)
      df
    })
    # ... rest of existing server logic unchanged
  })
}
```

### Three server calls

```r
make_tab_server("poverty",   data = poverty_df)
make_tab_server("insurance", data = insurance_df)
make_tab_server("family",    data = family_df,
  extra_filter = function(df, input) {
    req(length(input$family_types) > 0)
    df |> filter(Type %in% input$family_types)
  }
)
```

### Family Structure chart — line style mapping

Since the chart now carries two dimensions (location + household type), differentiate with:

- **Color** → location (Geneva / Ontario / NYS, same palette)
- **Line type** → household type

```r
scale_linetype_manual(values = c(
  "Two parents"   = "solid",
  "Single mother" = "dashed",
  "Single father" = "dotted"
))
```

The `aes()` mapping adds `linetype = Type` for family structure charts. Poverty and insurance charts are unaffected (no `Type` column).

---

## Edge Cases

| Case | Handling |
|---|---|
| Family structure data gaps (no 2011, 2016) | Set `connectgaps = FALSE` in Plotly so gaps show as breaks, not interpolated lines |
| All household types unchecked | `req(length(input$family_types) > 0)` guard in `extra_filter` — panel shows nothing rather than erroring |
| Sub-geography rows in PovertyFamilies.csv | Filtered out in `global.R` before app loads |
| "City of Geneva" label mismatch | Normalized to "Geneva" in `global.R` |
| Geneva small-sample volatility | Italic caveat note in Family Structure sidebar (same pattern as healthy_beginnings) |

---

## Color Palette

| Location | Color | Hex |
|---|---|---|
| Geneva | Red | `#E74C3C` |
| Ontario | Green | `#2ECC71` |
| NYS | Blue | `#3498DB` |

---

## Out of Scope

- Connecting stability indicators to health outcomes visually (possible future "Ready Mind" tab)
- Geographic map visualization
- Neonatal vs. infant breakdown
- SNAP / Income / Population datasets (staged in `data/raw_data/stable home/` for future use)
