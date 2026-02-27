# ──────────────────────────────────────────────────────────
# app.R — Geneva Kids Health Dashboard
# Two-tab Shiny app: Low Birth Weight & Infant Mortality
# ──────────────────────────────────────────────────────────

source("global.R")

# ── Helper: build one tab's UI ──────────────────────────
make_tab_ui <- function(id, label) {
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
      # ── Main panel ──
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title    = "Geneva",
          value    = textOutput(ns("kpi_geneva")),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = "#E74C3C", fg = "#fff")
        ),
        value_box(
          title    = "Ontario County",
          value    = textOutput(ns("kpi_ontario")),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = "#2ECC71", fg = "#fff")
        ),
        value_box(
          title    = "New York State",
          value    = textOutput(ns("kpi_nys")),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = "#3498DB", fg = "#fff")
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

# ── Helper: build one tab's server logic ────────────────
make_tab_server <- function(id, data, rate_col, rate_label) {
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

    latest_year <- reactive({
      req(nrow(filtered()) > 0)
      max(filtered()$Year)
    })

    # ── KPI value for a single location ──
    kpi_val <- function(loc) {
      d <- filtered() %>% filter(Location == loc, Year == latest_year())
      if (nrow(d) == 0) return("—")
      paste0(d[[rate_col]], "%")
    }

    output$kpi_geneva  <- renderText(kpi_val("Geneva"))
    output$kpi_ontario <- renderText(kpi_val("Ontario"))
    output$kpi_nys     <- renderText(kpi_val("NYS"))

    # ── Line chart ──
    output$line_chart <- renderPlotly({
      req(nrow(filtered()) > 0)
      p <- filtered() %>%
        ggplot(aes(
          x = Year,
          y = .data[[rate_col]],
          color = Location,
          group = Location,
          text = paste0(
            "<b>", Location, "</b><br>",
            "Year: ", Year, "<br>",
            rate_label, ": ", .data[[rate_col]], "%"
          )
        )) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2.5) +
        scale_color_manual(values = location_colors) +
        scale_x_continuous(breaks = seq(year_min, year_max, 1)) +
        labs(
          x     = "Year",
          y     = paste0(rate_label, " (%)"),
          color = NULL
        ) +
        theme_minimal(base_size = 13) +
        theme(
          axis.text.x  = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          panel.grid.minor = element_blank()
        )

      ggplotly(p, tooltip = "text") %>%
        layout(
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
          hovermode = "closest"
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ── Bar chart ──
    output$bar_chart <- renderPlotly({
      req(nrow(filtered()) > 0)
      p <- filtered() %>%
        ggplot(aes(
          x = factor(Year),
          y = .data[[rate_col]],
          fill = Location,
          text = paste0(
            "<b>", Location, "</b><br>",
            "Year: ", Year, "<br>",
            rate_label, ": ", .data[[rate_col]], "%"
          )
        )) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        scale_fill_manual(values = location_colors) +
        labs(
          x    = "Year",
          y    = paste0(rate_label, " (%)"),
          fill = NULL
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position  = "top",
          panel.grid.minor = element_blank()
        )

      ggplotly(p, tooltip = "text") %>%
        layout(
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
          hovermode = "closest"
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}

# ════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════
ui <- page_navbar(
  title = "Healthy Beginnings",
  theme = bs_theme(version = 5),
  nav_spacer(),
  make_tab_ui("lbw", "Low Birth Weight"),
  make_tab_ui("im",  "Infant Mortality")
)

# ════════════════════════════════════════════════════════
# Server
# ════════════════════════════════════════════════════════
server <- function(input, output, session) {

  make_tab_server("lbw", data = lbw, rate_col = "Rate", rate_label = "Low Birth Weight Rate")
  make_tab_server("im",  data = im,  rate_col = "Rate", rate_label = "Infant Mortality Rate")

}

# ════════════════════════════════════════════════════════
# Run
# ════════════════════════════════════════════════════════
shinyApp(ui, server)
