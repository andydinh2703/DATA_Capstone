# ──────────────────────────────────────────────────────────
# family.R — Family Structure Module Functions
# Provides: make_family_ui, make_family_server
# Data objects (family_df, location_colors, location_levels)
# loaded by stable_home/global.R
# ──────────────────────────────────────────────────────────

# ── Module-local constants ───────────────────────────────
fs_color_good <- "#1D9E75"
fs_color_bad  <- "#E74C3C"
fs_color_neutral <- "#6C757D"

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
      latest_yrs   <- (max_yr - 2):max_yr
      baseline_yrs <- min_yr:(min_yr + 2)

      avg <- function(col, yrs) mean(df[[col]][df$Year %in% yrs], na.rm = TRUE)

      g_late <- avg("Geneva",  latest_yrs)
      o_late <- avg("Ontario", latest_yrs)
      n_late <- avg("NYS",     latest_yrs)
      g_base <- avg("Geneva",  baseline_yrs)
      gap    <- g_late - n_late
      chg    <- g_late - g_base

      gap_col <- stat_color(gap, type)
      chg_col <- stat_color(chg, type)
      gap_lbl <- if (identical(gap_col, fs_color_good)) "favorable vs state" else "unfavorable vs state"
      chg_lbl <- if (identical(chg_col, fs_color_good)) "improvement" else "decline"

      late_lbl <- paste0(min(latest_yrs),   "–", max(latest_yrs))
      base_lbl <- paste0(min(baseline_yrs), "–", max(baseline_yrs))

      layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        value_box(
          title    = paste0("Geneva · ", late_lbl),
          value    = fmt_pct(g_late),
          p("3-yr avg", style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors[["Geneva"]], fg = "#fff")
        ),
        value_box(
          title    = paste0("Ontario County · ", late_lbl),
          value    = fmt_pct(o_late),
          p("3-yr avg", style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors[["Ontario"]], fg = "#fff")
        ),
        value_box(
          title    = paste0("NYS · ", late_lbl),
          value    = fmt_pct(n_late),
          p("3-yr avg", style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors[["NYS"]], fg = "#fff")
        ),
        value_box(
          title    = "Gap vs NYS",
          value    = fmt_pp(gap),
          p(gap_lbl, style = "margin: 0; font-size: 0.85em;"),
          showcase = bsicons::bs_icon("rulers"),
          theme    = value_box_theme(bg = gap_col, fg = "#fff")
        ),
        value_box(
          title    = paste0("Geneva · ", base_lbl),
          value    = fmt_pct(g_base),
          p("baseline (3-yr avg)", style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("flag-fill"),
          theme    = value_box_theme(bg = fs_color_neutral, fg = "#fff")
        ),
        value_box(
          title    = paste0("Change since ", min_yr),
          value    = fmt_pp(chg),
          p(chg_lbl, style = "margin: 0; font-size: 0.85em;"),
          showcase = bsicons::bs_icon("graph-up-arrow"),
          theme    = value_box_theme(bg = chg_col, fg = "#fff")
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
