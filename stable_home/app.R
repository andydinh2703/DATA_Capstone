# ──────────────────────────────────────────────────────────
# app.R — Stable Home Module Functions
# Provides: make_sh_tab_ui, make_sh_tab_server
# ──────────────────────────────────────────────────────────

# ── Helper: build one tab's UI ───────────────────────────
# year_min / year_max: passed explicitly — each sub-tab has its own range
# extra_controls: optional function(ns) returning a UI element appended to
#   the sidebar. Must be a function so input IDs are namespaced correctly.
make_sh_tab_ui <- function(id, label, year_min, year_max,
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
      # ── Main panel ──────────────────────────────────────
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

# ── Helper: build one tab's server logic ─────────────────
# extra_filter: optional function(df, input) applied after year/location filter
make_sh_tab_server <- function(id, data, rate_col, rate_label,
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

    # ── KPI value (average within filtered range) ──
    kpi_val <- function(loc) {
      df <- filtered()
      # For family structure (has Type column), show value for first selected type
      if ("Type" %in% names(df) && !is.null(input$family_types) &&
          length(input$family_types) > 0) {
        df <- df %>% filter(Type == input$family_types[1])
      }
      d <- df %>% filter(Location == loc)
      if (nrow(d) == 0) return("—")
      paste0(round(mean(d[[rate_col]], na.rm = TRUE), 1), "%")
    }

    output$kpi_geneva  <- renderText(kpi_val("Geneva"))
    output$kpi_ontario <- renderText(kpi_val("Ontario"))
    output$kpi_nys     <- renderText(kpi_val("NYS"))

    # ── Line chart ───────────────────────────────────────
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
              "<b>", Location, " \u2014 ", Type, "</b><br>",
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
        # connectgaps = FALSE: data gaps (e.g. 2011 missing) show as breaks
        style(connectgaps = FALSE) %>%
        config(displayModeBar = FALSE)
    })

    # ── Bar chart ────────────────────────────────────────
    output$bar_chart <- renderPlotly({
      req(nrow(filtered()) > 0)
      has_type <- "Type" %in% names(filtered())

      if (has_type) {
        p <- filtered() %>%
          ggplot(aes(
            x     = factor(Year),
            y     = .data[[rate_col]],
            fill  = Location,
            alpha = Type,
            text  = paste0(
              "<b>", Location, " \u2014 ", Type, "</b><br>",
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
