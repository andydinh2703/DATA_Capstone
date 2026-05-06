# ──────────────────────────────────────────────────────────
# family.R — Family Structure Module Functions
# Provides: make_family_ui, make_family_server
# Data objects (family_df, location_colors, location_levels)
# loaded by stable_home/global.R
# ──────────────────────────────────────────────────────────

# ── Module-local constants ───────────────────────────────
fs_color_good <- "#2E8B6A"
fs_color_bad  <- "#C0392B"
fs_color_neutral <- "#7C868E"

# Direction-of-favorability per household type (for color-coded stat cards)
fs_higher_is_better <- c(
  "Two parents"   = TRUE,
  "Single mother" = FALSE,
  "Single father" = FALSE
)

# ── UI module ────────────────────────────────────────────
# make_family_ui: builds a single nav_panel tab for Family Structure.
#   id    — Shiny module ID (must match make_family_server call)
#   label — text shown on the pill tab (e.g. "Family Structure")
make_family_ui <- function(id, label) {
  ns <- NS(id)
  nav_panel(
    title = label,
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        radioButtons(
          ns("fs_type"),
          label    = "Household Type",
          choices  = c("Two parents", "Single mother", "Single father"),
          selected = "Two parents"
        ),
        helpText(
          strong("Note:"), "Geneva has a small population.",
          "Percentages may vary year to year — focus on long-run",
          "directional trends rather than single-year movements."
        )
      ),
      uiOutput(ns("stat_cards")),
      card(
        card_header("Trend by Household Type — 2010 to 2023"),
        plotlyOutput(ns("fs_line_chart"), height = "380px")
      ),
      card(
        style = "margin-top: 10px;",
        p(strong("Family Structure: "),
          "Share of households with children under 6, by household type.",
          "Select a type at the left to compare Geneva against Ontario County",
          "and New York State across 2010–2023. Stat cards show",
          strong("3-year trailing averages"),
          "to smooth out year-to-year sampling noise — most relevant for",
          "Geneva given its small population. The line chart still plots",
          "the raw single-year values so you can see the underlying volatility.",
          "Green indicates a favorable direction for children's outcomes,",
          "red an unfavorable one.")
      )
    )
  )
}

# ── Server module ────────────────────────────────────────
# make_family_server: handles reactivity for the Family Structure tab.
#   id   — Shiny module ID (must match make_family_ui call)
#   data — long-format data frame with Year, Location, Type, PCT columns
#          (family_df from stable_home/global.R)
make_family_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # ── Reactive: wide-format slice for the selected type ──
    # Columns: Year, Geneva, Ontario, NYS
    type_df <- reactive({
      req(input$fs_type)
      data %>%
        filter(Type == input$fs_type) %>%
        select(Year, Location, PCT) %>%
        tidyr::pivot_wider(names_from = Location, values_from = PCT) %>%
        arrange(Year)
    })

    # ── Helpers ────────────────────────────────────────────
    fmt_pct <- function(x) {
      if (length(x) == 0 || is.na(x)) return("—")
      paste0(round(x, 1), "%")
    }
    fmt_pp <- function(x) {
      if (length(x) == 0 || is.na(x)) return("—")
      sign <- if (x >= 0) "+" else ""
      paste0(sign, round(x, 1), " pts")
    }
    # Returns favorable color if movement is in the better direction for the type
    stat_color <- function(value, type) {
      if (length(value) == 0 || is.na(value)) return(fs_color_neutral)
      better <- fs_higher_is_better[[type]]
      if ((better && value >= 0) || (!better && value <= 0)) fs_color_good
      else fs_color_bad
    }

    # ── Stat cards (rendered as one row of 6 value_boxes) ──
    # Values are 3-year trailing averages to smooth small-population noise
    # (especially for Geneva). Year ranges derive from min/max of the data.
    output$stat_cards <- renderUI({
      df   <- type_df()
      type <- input$fs_type

      max_yr <- max(df$Year, na.rm = TRUE)
      min_yr <- min(df$Year, na.rm = TRUE)
      avg <- function(col) mean(df[[col]], na.rm = TRUE)

      g_avg <- avg("Geneva")
      o_avg <- avg("Ontario")
      n_avg <- avg("NYS")

      avg_lbl <- paste0(min_yr, "–", max_yr, " Average")

      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title    = "Geneva",
          value    = fmt_pct(g_avg),
          p(avg_lbl, style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors[["Geneva"]], fg = "#fff")
        ),
        value_box(
          title    = "Ontario County",
          value    = fmt_pct(o_avg),
          p(avg_lbl, style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors[["Ontario"]], fg = "#fff")
        ),
        value_box(
          title    = "New York State",
          value    = fmt_pct(n_avg),
          p(avg_lbl, style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors[["NYS"]], fg = "#fff")
        )
      )
    })

    # ── Line chart ─────────────────────────────────────────
    output$fs_line_chart <- renderPlotly({
      df   <- type_df()
      type <- input$fs_type

      plot_ly(df, x = ~Year) %>%
        add_trace(
          y      = ~Geneva,
          name   = "Geneva",
          type   = "scatter",
          mode   = "lines+markers",
          line   = list(color = location_colors[["Geneva"]], width = 2.5),
          marker = list(color = location_colors[["Geneva"]], size = 7),
          hovertemplate = "Geneva: %{y:.1f}%<extra></extra>"
        ) %>%
        add_trace(
          y      = ~Ontario,
          name   = "Ontario County",
          type   = "scatter",
          mode   = "lines+markers",
          line   = list(color = location_colors[["Ontario"]], width = 2, dash = "dash"),
          marker = list(color = location_colors[["Ontario"]], size = 5),
          hovertemplate = "Ontario County: %{y:.1f}%<extra></extra>"
        ) %>%
        add_trace(
          y      = ~NYS,
          name   = "New York State",
          type   = "scatter",
          mode   = "lines+markers",
          line   = list(color = location_colors[["NYS"]], width = 2, dash = "dot"),
          marker = list(color = location_colors[["NYS"]], size = 5),
          hovertemplate = "NYS: %{y:.1f}%<extra></extra>"
        ) %>%
        layout(
          xaxis     = list(title = "Year", tickmode = "linear", dtick = 1,
                           tickangle = -45),
          yaxis     = list(title = paste0(type, " (% of households)"),
                           ticksuffix = "%", rangemode = "tozero"),
          legend    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
          hovermode = "x unified",
          margin    = list(l = 50, r = 20, t = 20, b = 60)
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}
