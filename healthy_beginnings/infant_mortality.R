# ──────────────────────────────────────────────────────────
# infant_mortality.R — Infant Mortality Module Functions
# Provides: make_im_ui, make_im_server
# Data objects (im, year_min/max) loaded by global.R
# ──────────────────────────────────────────────────────────

# ── UI module ────────────────────────────────────────────
make_im_ui <- function(id, label) {
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
        helpText(
          strong("Note:"), "Geneva has ~120–170 births per year.",
          "Small numbers can produce volatile year-to-year rates."
        )
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title    = "Geneva",
          value    = textOutput(ns("kpi_geneva")),
          p(textOutput(ns("avg_subtitle")), style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = "#D94F4F", fg = "#fff")
        ),
        value_box(
          title    = "Ontario County",
          value    = textOutput(ns("kpi_ontario")),
          p(textOutput(ns("avg_subtitle2")), style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = "#4CAF7D", fg = "#fff")
        ),
        value_box(
          title    = "New York State",
          value    = textOutput(ns("kpi_nys")),
          p(textOutput(ns("avg_subtitle3")), style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = "#3D7FBA", fg = "#fff")
        )
      ),
      card(
        card_header("Trend Over Time"),
        plotlyOutput(ns("line_chart"), height = "380px")
      )
    )
  )
}

# ── Server module ────────────────────────────────────────
make_im_server <- function(id, data, rate_col, rate_label) {
  moduleServer(id, function(input, output, session) {

    filtered <- reactive({
      req(input$year_range, input$locations)
      data %>%
        filter(
          Year >= input$year_range[1],
          Year <= input$year_range[2],
          Location %in% input$locations
        )
    })

    kpi_val <- function(loc) {
      d <- filtered() %>% filter(Location == loc)
      if (nrow(d) == 0) return("—")
      paste0(round(mean(d[[rate_col]], na.rm = TRUE), 1), " per 1,000")
    }

    output$kpi_geneva  <- renderText(kpi_val("Geneva"))
    output$kpi_ontario <- renderText(kpi_val("Ontario"))
    output$kpi_nys     <- renderText(kpi_val("NYS"))

    output$avg_subtitle <- renderText({
      paste0(input$year_range[1], "–", input$year_range[2], " Average")
    })
    output$avg_subtitle2 <- renderText({
      paste0(input$year_range[1], "–", input$year_range[2], " Average")
    })
    output$avg_subtitle3 <- renderText({
      paste0(input$year_range[1], "–", input$year_range[2], " Average")
    })

    output$line_chart <- renderPlotly({
      req(nrow(filtered()) > 0)

      # Pivot to wide format for direct plot_ly traces
      df <- filtered() %>%
        select(Year, Location, .data[[rate_col]]) %>%
        tidyr::pivot_wider(names_from = Location, values_from = .data[[rate_col]]) %>%
        arrange(Year)

      plot_ly(df, x = ~Year) %>%
        add_trace(
          y      = ~Geneva,
          name   = "Geneva",
          type   = "scatter",
          mode   = "lines+markers",
          line   = list(color = location_colors[["Geneva"]], width = 2.5),
          marker = list(color = location_colors[["Geneva"]], size = 7),
          hovertemplate = paste0("Geneva: %{y:.1f} per 1,000<extra></extra>")
        ) %>%
        add_trace(
          y      = ~Ontario,
          name   = "Ontario County",
          type   = "scatter",
          mode   = "lines+markers",
          line   = list(color = location_colors[["Ontario"]], width = 2, dash = "dash"),
          marker = list(color = location_colors[["Ontario"]], size = 5),
          hovertemplate = paste0("Ontario County: %{y:.1f} per 1,000<extra></extra>")
        ) %>%
        add_trace(
          y      = ~NYS,
          name   = "New York State",
          type   = "scatter",
          mode   = "lines+markers",
          line   = list(color = location_colors[["NYS"]], width = 2, dash = "dot"),
          marker = list(color = location_colors[["NYS"]], size = 5),
          hovertemplate = paste0("NYS: %{y:.1f} per 1,000<extra></extra>")
        ) %>%
        layout(
          xaxis     = list(title = "Year", tickmode = "linear", dtick = 1,
                           tickangle = -45),
          yaxis     = list(title = paste0(rate_label, " (per 1,000 births)"),
                           rangemode = "tozero"),
          legend    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
          hovermode = "x unified",
          margin    = list(l = 50, r = 20, t = 20, b = 60)
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}
