# Stable Home Dashboard Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `stable_home/` as a standalone R Shiny app that lets users explore household stability indicators (poverty, health insurance, family structure) across Geneva, Ontario County, and NYS.

**Architecture:** Two files mirror the `healthy_beginnings/` structure exactly — `global.R` loads and cleans three CSVs into tidy data frames, and `app.R` reuses the `make_tab_ui`/`make_tab_server` module pattern with two small extensions (optional `extra_controls` sidebar param and optional `extra_filter` reactive param). A single top-level `nav_panel("Stable Home")` contains a `navset_pill` with three inner sub-tabs.

**Tech Stack:** R, shiny, bslib, plotly, ggplot2, tidyverse, bsicons, here

---

## File Map

| File | Action | Responsibility |
|---|---|---|
| `stable_home/global.R` | Create | Load & clean `poverty_df`, `insurance_df`, `family_df`; shared constants |
| `stable_home/app.R` | Create | Extended `make_tab_ui`/`make_tab_server`; UI + server |

Source CSVs (read-only):
- `data/raw_data/stable home/PovertyFamilies.csv`
- `data/raw_data/stable home/HealthInsCoverage.csv`
- `data/raw_data/stable home/FamilyStructureUnder6A.csv`

Reference (do not modify):
- `healthy_beginnings/global.R` — pattern reference for data loading
- `healthy_beginnings/app.R` — pattern reference for module functions

---

## Task 1: Create `stable_home/global.R`

**Files:**
- Create: `stable_home/global.R`

- [ ] **Step 1: Create the file with libraries and shared constants**

```r
# ──────────────────────────────────────────────────────────
# global.R — Data loading & shared objects
# Stable Home Dashboard
# ──────────────────────────────────────────────────────────

library(tidyverse)
library(shiny)
library(bslib)
library(plotly)

# ── Shared constants ─────────────────────────────────────
location_colors <- c(
  "Geneva"  = "#E74C3C",
  "Ontario" = "#2ECC71",
  "NYS"     = "#3498DB"
)

location_levels <- c("Geneva", "Ontario", "NYS")
```

- [ ] **Step 2: Add poverty_df loading**

```r
# ── Poverty (families below poverty line) ───────────────
poverty_raw <- read_csv(
  here::here("data", "raw_data", "stable home", "PovertyFamilies.csv"),
  col_types = cols(.default = col_character())
)

poverty_df <- poverty_raw %>%
  filter(Community %in% location_levels) %>%
  mutate(
    Location = factor(Community, levels = location_levels),
    Year     = as.integer(Year),
    PCT      = as.numeric(PCT)
  ) %>%
  select(Location, Year, PCT) %>%
  arrange(Location, Year)

poverty_year_min <- min(poverty_df$Year, na.rm = TRUE)   # 2010
poverty_year_max <- max(poverty_df$Year, na.rm = TRUE)   # 2023
```

- [ ] **Step 3: Add insurance_df loading**

```r
# ── Health insurance coverage ────────────────────────────
insurance_raw <- read_csv(
  here::here("data", "raw_data", "stable home", "HealthInsCoverage.csv"),
  col_types = cols(.default = col_character())
)

insurance_df <- insurance_raw %>%
  mutate(
    Community = case_when(
      Community == "City of Geneva" ~ "Geneva",
      TRUE                          ~ Community
    )
  ) %>%
  filter(Community %in% location_levels) %>%
  mutate(
    Location = factor(Community, levels = location_levels),
    Year     = as.integer(Year),
    PCT      = as.numeric(PCT)
  ) %>%
  select(Location, Year, PCT) %>%
  arrange(Location, Year)

insurance_year_min <- min(insurance_df$Year, na.rm = TRUE)   # 2012
insurance_year_max <- max(insurance_df$Year, na.rm = TRUE)   # 2023
```

- [ ] **Step 4: Add family_df loading**

```r
# ── Family structure (households with children under 6) ─
family_raw <- read_csv(
  here::here("data", "raw_data", "stable home", "FamilyStructureUnder6A.csv"),
  col_types = cols(.default = col_character())
)

family_df <- family_raw %>%
  filter(
    Community %in% location_levels,
    Type %in% c("Two parents", "Single mother", "Single father")
  ) %>%
  mutate(
    Location = factor(Community, levels = location_levels),
    Year     = as.integer(Year),
    PCT      = round(as.numeric(Proportion) * 100, 1),
    Type     = factor(Type, levels = c("Two parents", "Single mother", "Single father"))
  ) %>%
  select(Location, Year, Type, PCT) %>%
  arrange(Location, Year, Type)

family_year_min <- min(family_df$Year, na.rm = TRUE)   # 2010
family_year_max <- max(family_df$Year, na.rm = TRUE)   # 2017
```

- [ ] **Step 5: Manually verify data loaded correctly**

Open an R console in the `stable_home/` directory and run:
```r
source("global.R")

# Check row counts and no sub-geographies leaked through
nrow(poverty_df)    # expect ~42 rows (3 locations × ~14 years)
unique(poverty_df$Location)   # expect: Geneva, Ontario, NYS only

nrow(insurance_df)  # expect ~36 rows (3 locations × 12 years)
unique(insurance_df$Location) # expect: Geneva, Ontario, NYS — NO "City of Geneva"

nrow(family_df)     # expect ~45 rows (3 locations × 5 years × 3 types)
unique(family_df$Type)  # expect: Two parents, Single mother, Single father — NO "All"

# Spot-check a value
poverty_df %>% filter(Location == "Geneva", Year == 2023)
# Expect PCT = 15.2
```

---

## Task 2: Create `stable_home/app.R` — Module Functions

**Files:**
- Create: `stable_home/app.R`

- [ ] **Step 1: Add source and extended make_tab_ui**

`extra_controls` is passed as a **function `function(ns) { ... }`** so the input IDs it creates get properly namespaced inside the module. This avoids the bug where a pre-built widget would register its ID in the global namespace instead of the module's namespace.

```r
# ──────────────────────────────────────────────────────────
# app.R — Stable Home Dashboard
# ──────────────────────────────────────────────────────────

source("global.R")

# ── Helper: build one tab's UI ───────────────────────────
# year_min / year_max passed explicitly (each sub-tab has its own range)
# extra_controls: optional function(ns) returning a UI element, appended
#   to the sidebar. Passed as a function so input IDs are namespaced correctly.
make_tab_ui <- function(id, label, year_min, year_max,
                        rate_label, extra_controls = NULL) {
  ns <- NS(id)
  nav_panel(
    title = label,
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        sliderInput(
          ns("year_range"), "Year Range",
          min = year_min, max = year_max,
          value = c(year_min, year_max),
          step = 1, sep = "", ticks = FALSE
        ),
        checkboxGroupInput(
          ns("locations"), "Locations",
          choices  = location_levels,
          selected = location_levels
        ),
        if (!is.null(extra_controls)) extra_controls(ns),
        helpText(
          strong("Note:"), "Geneva has a small population.",
          "Percentages may vary year to year."
        )
      ),
      # ── Main panel ──
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title    = "Geneva",
          value    = textOutput(ns("kpi_geneva")),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors["Geneva"], fg = "#fff")
        ),
        value_box(
          title    = "Ontario County",
          value    = textOutput(ns("kpi_ontario")),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors["Ontario"], fg = "#fff")
        ),
        value_box(
          title    = "New York State",
          value    = textOutput(ns("kpi_nys")),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors["NYS"], fg = "#fff")
        )
      ),
      card(
        card_header("Trend Over Time"),
        plotlyOutput(ns("line_chart"), height = "380px")
      ),
      card(
        card_header("Year-by-Year Comparison"),
        plotlyOutput(ns("bar_chart"), height = "320px")
      )
    )
  )
}
```

- [ ] **Step 2: Add extended make_tab_server**

Key differences from healthy_beginnings:
- `extra_filter`: optional function `(df, input) -> df` applied after year/location filter
- Line chart conditionally maps `linetype = Type` when a `Type` column exists
- `connectgaps = FALSE` in plotly for line chart (handles family structure data gaps)

```r
# ── Helper: build one tab's server logic ─────────────────
# extra_filter: optional function(df, input) applied after year/location filter
make_tab_server <- function(id, data, rate_col, rate_label,
                            extra_filter = NULL) {
  moduleServer(id, function(input, output, session) {

    filtered <- reactive({
      req(input$year_range, input$locations)
      df <- data %>%
        filter(
          Year     >= input$year_range[1],
          Year     <= input$year_range[2],
          Location %in% input$locations
        )
      if (!is.null(extra_filter)) df <- extra_filter(df, input)
      df
    })

    latest_year <- reactive({
      req(nrow(filtered()) > 0)
      max(filtered()$Year)
    })

    # ── KPI value for a single location ──
    kpi_val <- function(loc) {
      df <- filtered()
      # For family structure data (has Type column), use first selected type
      if ("Type" %in% names(df) && !is.null(input$family_types) &&
          length(input$family_types) > 0) {
        df <- df %>% filter(Type == input$family_types[1])
      }
      d <- df %>% filter(Location == loc, Year == latest_year())
      if (nrow(d) == 0) return("—")
      paste0(round(d[[rate_col]][1], 1), "%")
    }

    output$kpi_geneva  <- renderText(kpi_val("Geneva"))
    output$kpi_ontario <- renderText(kpi_val("Ontario"))
    output$kpi_nys     <- renderText(kpi_val("NYS"))

    # ── Line chart ──
    output$line_chart <- renderPlotly({
      req(nrow(filtered()) > 0)
      has_type <- "Type" %in% names(filtered())

      if (has_type) {
        p <- filtered() %>%
          ggplot(aes(
            x        = Year,
            y        = .data[[rate_col]],
            color    = Location,
            linetype = Type,
            group    = interaction(Location, Type),
            text     = paste0(
              "<b>", Location, " — ", Type, "</b><br>",
              "Year: ", Year, "<br>",
              rate_label, ": ", .data[[rate_col]], "%"
            )
          )) +
          geom_line(linewidth = 1.1) +
          geom_point(size = 2.5) +
          scale_color_manual(values = location_colors) +
          scale_linetype_manual(
            values = c(
              "Two parents"   = "solid",
              "Single mother" = "dashed",
              "Single father" = "dotted"
            )
          )
      } else {
        p <- filtered() %>%
          ggplot(aes(
            x     = Year,
            y     = .data[[rate_col]],
            color = Location,
            group = Location,
            text  = paste0(
              "<b>", Location, "</b><br>",
              "Year: ", Year, "<br>",
              rate_label, ": ", .data[[rate_col]], "%"
            )
          )) +
          geom_line(linewidth = 1.1) +
          geom_point(size = 2.5) +
          scale_color_manual(values = location_colors)
      }

      p <- p +
        scale_x_continuous(breaks = seq(
          min(filtered()$Year), max(filtered()$Year), 1
        )) +
        labs(
          x        = "Year",
          y        = paste0(rate_label, " (%)"),
          color    = NULL,
          linetype = NULL
        ) +
        theme_minimal(base_size = 13) +
        theme(
          axis.text.x      = element_text(angle = 45, hjust = 1),
          legend.position  = "top",
          panel.grid.minor = element_blank()
        )

      ggplotly(p, tooltip = "text") %>%
        layout(
          legend    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
          hovermode = "closest"
        ) %>%
        # connectgaps = FALSE so sparse family structure years show as breaks
        style(connectgaps = FALSE) %>%
        config(displayModeBar = FALSE)
    })

    # ── Bar chart ──
    output$bar_chart <- renderPlotly({
      req(nrow(filtered()) > 0)
      has_type <- "Type" %in% names(filtered())

      if (has_type) {
        p <- filtered() %>%
          ggplot(aes(
            x    = factor(Year),
            y    = .data[[rate_col]],
            fill = Location,
            alpha = Type,
            text = paste0(
              "<b>", Location, " — ", Type, "</b><br>",
              "Year: ", Year, "<br>",
              rate_label, ": ", .data[[rate_col]], "%"
            )
          )) +
          geom_col(position = position_dodge(width = 0.8), width = 0.7) +
          scale_fill_manual(values = location_colors) +
          scale_alpha_manual(
            values = c(
              "Two parents"   = 1.0,
              "Single mother" = 0.65,
              "Single father" = 0.35
            )
          )
      } else {
        p <- filtered() %>%
          ggplot(aes(
            x    = factor(Year),
            y    = .data[[rate_col]],
            fill = Location,
            text = paste0(
              "<b>", Location, "</b><br>",
              "Year: ", Year, "<br>",
              rate_label, ": ", .data[[rate_col]], "%"
            )
          )) +
          geom_col(position = position_dodge(width = 0.8), width = 0.7) +
          scale_fill_manual(values = location_colors)
      }

      p <- p +
        labs(
          x     = "Year",
          y     = paste0(rate_label, " (%)"),
          fill  = NULL,
          alpha = NULL
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position  = "top",
          panel.grid.minor = element_blank()
        )

      ggplotly(p, tooltip = "text") %>%
        layout(
          legend    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
          hovermode = "closest"
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}
```

---

## Task 3: Create `stable_home/app.R` — UI and Server

**Files:**
- Modify: `stable_home/app.R` (append to file created in Task 2)

- [ ] **Step 1: Add the UI**

```r
# ════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════
ui <- page_navbar(
  title = "Stable Home",
  theme = bs_theme(version = 5),
  nav_spacer(),
  nav_panel(
    "Stable Home",
    navset_pill(
      make_tab_ui(
        id         = "poverty",
        label      = "Poverty",
        year_min   = poverty_year_min,
        year_max   = poverty_year_max,
        rate_label = "Family Poverty Rate"
      ),
      make_tab_ui(
        id         = "insurance",
        label      = "Health Insurance",
        year_min   = insurance_year_min,
        year_max   = insurance_year_max,
        rate_label = "Insurance Coverage"
      ),
      make_tab_ui(
        id         = "family",
        label      = "Family Structure",
        year_min   = family_year_min,
        year_max   = family_year_max,
        rate_label = "% of Households",
        extra_controls = function(ns) {
          checkboxGroupInput(
            ns("family_types"),
            label    = "Household Type",
            choices  = c("Two parents", "Single mother", "Single father"),
            selected = c("Two parents", "Single mother", "Single father")
          )
        }
      )
    )
  )
)
```

- [ ] **Step 2: Add the server**

```r
# ════════════════════════════════════════════════════════
# Server
# ════════════════════════════════════════════════════════
server <- function(input, output, session) {

  make_tab_server(
    id         = "poverty",
    data       = poverty_df,
    rate_col   = "PCT",
    rate_label = "Family Poverty Rate"
  )

  make_tab_server(
    id         = "insurance",
    data       = insurance_df,
    rate_col   = "PCT",
    rate_label = "Insurance Coverage"
  )

  make_tab_server(
    id         = "family",
    data       = family_df,
    rate_col   = "PCT",
    rate_label = "% of Households",
    extra_filter = function(df, input) {
      req(length(input$family_types) > 0)
      df %>% filter(Type %in% input$family_types)
    }
  )

}

# ════════════════════════════════════════════════════════
# Run
# ════════════════════════════════════════════════════════
shinyApp(ui, server)
```

---

## Task 4: Manual Verification

**Files:** none (verification only)

- [ ] **Step 1: Launch the app**

From the project root:
```bash
Rscript -e "shiny::runApp('stable_home/', port = 3839)"
```
Open http://localhost:3839. Expected: app loads with "Stable Home" navbar, three pills visible.

- [ ] **Step 2: Verify Poverty tab**

- Select "Poverty" pill
- KPI boxes should show three values (Geneva ~15%, Ontario ~5%, NYS ~10%)
- Line chart shows 3 colored lines trending from 2010–2023
- Bar chart shows grouped bars by year
- Drag year slider to 2015–2020, confirm charts update
- Uncheck Ontario, confirm Ontario disappears from charts

- [ ] **Step 3: Verify Health Insurance tab**

- Select "Health Insurance" pill — year slider should reset to 2012–2023
- KPI boxes show insurance coverage % (all should be 90%+)
- Charts update correctly with slider and location checkboxes

- [ ] **Step 4: Verify Family Structure tab**

- Select "Family Structure" pill — year slider should reset to 2010–2017
- All three household types checked by default
- Line chart shows 9 lines (3 locations × 3 types), differentiated by color + linetype
- Gaps between 2010 and 2012, and between 2015 and 2017 should appear as **breaks** in the line (not connected)
- Bar chart shows grouped bars with opacity differentiation by type
- Uncheck "Single father" — lines and bars for that type disappear
- Uncheck all types — charts go blank (no error)
- KPI boxes show values for first checked type ("Two parents" if checked)

- [ ] **Step 5: Verify no sub-geographies appear**

In the Poverty sidebar, confirm the location checkboxes only show: Geneva, Ontario County, NYS — no "Geneva GCSD", "Ontario wo Geneva", etc.
