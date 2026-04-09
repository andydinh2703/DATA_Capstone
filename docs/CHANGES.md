# Changelog — 2026-03-30

Summary of all changes made to the Ready, Set, GROW! Shiny dashboard in this session.

---

## 1. Stable Home — Bar Charts Replaced

**Files changed:** `stable_home/app.R`, `main/app.R`

### Problem
The year-by-year bar charts in the Stable Home tabs were not effective:
- **Poverty & Health Insurance**: The dodged bar charts duplicated the line chart above without adding new insight, and became cluttered with many years of data.
- **Family Structure**: The bar chart encoded household type using opacity levels (1.0 / 0.65 / 0.35 alpha), a subtle visual cue that most viewers missed entirely.

### Solution
Replaced both bar chart variants with more story-driven charts, applying the **gray-color principle** throughout: Geneva is rendered in its brand red (`#E74C3C`) while Ontario and NYS are muted gray (`#BBBBBB`) so the viewer's eye goes directly to Geneva without needing to read a legend.

#### Poverty & Health Insurance → Slope Chart
Shows exactly two data points per location: the first year and the last year of the selected range, connected by a line. The direction and steepness of the slope immediately answers *"did it get better or worse?"* Value labels (e.g. `17.3%`) are shown next to each dot.

- Card header renamed from `"Year-by-Year Comparison"` to `"Change Over Selected Period"`
- Includes a `req(start_yr < end_yr)` guard so the chart gracefully skips rendering if the slider is collapsed to a single year

#### Family Structure → Small Multiples (Faceted Line Chart)
Three side-by-side panels — one per household type (Two parents | Single mother | Single father) — via `facet_wrap(~ Type, ncol = 3)`. Each panel shows all three locations as lines, with Geneva in red and Ontario/NYS in gray. Removes the confusing alpha-opacity encoding entirely.

- Card header renamed to `"Trends by Household Type"`

### Other changes in `stable_home/app.R`
- Internal Shiny output ID renamed from `bar_chart` to `comparison_chart` to accurately reflect the new chart types
- New `bottom_chart_label` parameter added to `make_sh_tab_ui()` (default: `"Year-by-Year Comparison"` for backward compatibility)

### Changes in `main/app.R`
The three `make_sh_tab_ui()` call sites updated with explicit `bottom_chart_label` arguments:
- Poverty: `"Change Over Selected Period"`
- Health Insurance: `"Change Over Selected Period"`
- Family Structure: `"Trends by Household Type"`

---

## 2. Infant Mortality — Unit Label Fix

**File changed:** `healthy_beginnings/infant_mortality.R`

### Problem
The app was displaying infant mortality rates with a `%` symbol (e.g. `13.4%`), but the `Rate` column in the CSV stores **deaths per 1,000 live births** — not a percentage. This made Geneva's rate of `13.4` appear as if 13.4% of infants died, when the correct reading is 13.4 per 1,000 births (~1.34%).

The rate values are correct and sourced directly from NYS Vital Statistics. The issue was purely in the display labels.

### Fix
Updated three display locations:

| Location | Before | After |
|---|---|---|
| KPI value box | `13.4%` | `13.4 per 1,000` |
| Y-axis label | `Infant Mortality Rate (%)` | `Infant Mortality Rate (per 1,000 births)` |
| Hover tooltip | `Rate: 13.4%` | `Rate: 13.4 per 1,000` |

---

## 3. Infant Mortality — KPI Shows Average Instead of Latest Year

**File changed:** `healthy_beginnings/infant_mortality.R`

### Problem
The three KPI value boxes (Geneva, Ontario County, New York State) showed the rate for the **latest year** in the selected range only, ignoring the rest of the selection.

### Fix
Changed `kpi_val()` to compute the **mean rate over the full selected year range**, consistent with how the Stable Home tab KPIs work. Also removed the now-unused `latest_year` reactive.

---

## 4. Healthy Born — Switched to navset_pill Navigation

**Files changed:** `healthy_beginnings/lbw.R`, `main/app.R`

### Problem
The Healthy Born tab used action buttons + `reactiveVal` + `renderUI` to switch between Infant Mortality and Low Birth Weight views. This was more complex than needed and visually inconsistent with the Stable Home tab, which uses pill-style navigation.

### Fix

#### `healthy_beginnings/lbw.R`
Updated `make_lbw_ui()` to match the structure of `make_im_ui()`:
- Added `label` parameter
- Changed return value from `tagList(...)` to `nav_panel(title = label, layout_sidebar(...))`
- Moved the description text into the sidebar
- Map rendered inside a `card()` with `card_header("Low Birth Weight by County")`

#### `main/app.R` — UI
Replaced the Healthy Born tab content:

**Removed:**
- `uiOutput("dynamic_header")`
- `fluidRow` with two `actionButton`s (`"infant"`, `"low_birth_weight"`)
- `uiOutput("dynamic_plot")`

**Added:**
```r
navset_pill(
  make_im_ui("im", "Infant Mortality"),
  make_lbw_ui("lbw", "Low Birth Weight")
)
```

#### `main/app.R` — Server
Removed the reactive switching logic:
- `current_view <- reactiveVal(...)`
- `observeEvent(input$infant, ...)`
- `observeEvent(input$low_birth_weight, ...)`
- `output$dynamic_plot <- renderUI(...)`

The `make_im_server()` and `make_lbw_server()` calls were kept unchanged.

---

## 5. .gitignore — Agent Tooling Excluded

**File changed:** `.gitignore`

Added entries to prevent AI agent tooling files from being committed:

```
.agents
skills-lock.json
```

`.claude` and `.skills-lock.json` were already present. The `.agents/` directory (containing superpowers skill files installed via `npx skills add`) and `skills-lock.json` were staged accidentally and have been removed from the index.

---

# Changelog — 2026-04-09

Summary of all changes made to the Ready, Set, GROW! Shiny dashboard in this session.

---

## 1. Module/Dev-App Split — lbw.R and pop_incom.R

**Files changed:** `healthy_beginnings/lbw.R`, `healthy_beginnings/lbw_dev.R`,
`stable_home/pop_incom.R`, `stable_home/pop_incom_dev.R`, `main/global.R`

### Problem
Both `lbw.R` and `pop_incom.R` were developed as standalone Shiny apps in Posit Cloud.
When saved and synced to the repo, they contained `shinyApp()` calls at the bottom and
replaced the module functions (`make_lbw_ui`, `make_pi_ui`, etc.) the main app depends on.
This caused the main app to crash on startup because sourcing these files triggered a
standalone app launch instead of registering the module functions.

### Solution
Separated each file into two files with a consistent `_dev.R` naming convention:

- **Module file** (`lbw.R`, `pop_incom.R`): exports only `make_X_ui` / `make_X_server`.
  No `library()` calls, no top-level data, no `shinyApp()`. Safe to source from `main/global.R`.
- **Dev app** (`lbw_dev.R`, `pop_incom_dev.R`): standalone test app that loads all libraries,
  sources the module, and calls `shinyApp()`. Run directly in RStudio for isolated testing.
  NOT sourced by the main app.

### New features brought from Posit into the modules

#### lbw.R
- **Leaflet interactive map** replaces the static `ggplot + geom_sf()` map.
  Uses `colorNumeric("viridis")` fill, hover labels, and a red outline for Ontario County.
  Uses `leafletProxy` so the map doesn't rebuild on every slider change — only polygons update.
- **Plotly trend line chart** added alongside the map. Shows all NY counties in grey with
  Ontario highlighted in red.
- UI layout changed to `layout_columns(col_widths = c(6, 6))` to show map and chart side by side.
- `leaflet` and `htmltools` added to `main/global.R` as new dependencies.

#### pop_incom.R
- **Location checkbox** (`checkboxGroupInput`) added to the sidebar, consistent with
  `infant_mortality.R` and `snap_tanf.R`. Allows filtering by Geneva / Ontario / NYS.
- `filtered_data` reactive now filters by both year range and selected locations.

---

## 2. main/global.R — New Dependencies

**File changed:** `main/global.R`

Added `library(leaflet)` and `library(htmltools)` to support the new interactive map
in the LBW module. Inserted after `library(plotly)`, before `library(tigris)`.
