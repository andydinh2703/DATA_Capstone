# ──────────────────────────────────────────────────────────
# app.R — Stable Home Module Functions
# Provides: make_sh_tab_ui, make_sh_tab_server
# ──────────────────────────────────────────────────────────

# ── Helper: build one tab's UI ───────────────────────────
# year_min / year_max: passed explicitly — each sub-tab has its own range
# extra_controls: optional function(ns) returning a UI element appended to
#   the sidebar. Must be a function so input IDs are namespaced correctly.
make_sh_tab_ui <- function(id, label, year_min, year_max,
                        rate_label, extra_controls = NULL,
                        bottom_chart_label = "Year-by-Year Comparison") {
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
          p(textOutput(ns("avg_subtitle")), style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors["Geneva"], fg = "#fff")
        ),
        value_box(
          title    = "Ontario County",
          value    = textOutput(ns("kpi_ontario")),
          p(textOutput(ns("avg_subtitle2")), style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors["Ontario"], fg = "#fff")
        ),
        value_box(
          title    = "New York State",
          value    = textOutput(ns("kpi_nys")),
          p(textOutput(ns("avg_subtitle3")), style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = location_colors["NYS"], fg = "#fff")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Trend Over Time"),
          plotlyOutput(ns("line_chart"), height = "380px")
        ),
        card(
          card_header(bottom_chart_label),
          plotlyOutput(ns("comparison_chart"), height = "380px")
        )
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

    output$avg_subtitle <- renderText({
      paste0(input$year_range[1], "–", input$year_range[2], " Average")
    })
    output$avg_subtitle2 <- renderText({
      paste0(input$year_range[1], "–", input$year_range[2], " Average")
    })
    output$avg_subtitle3 <- renderText({
      paste0(input$year_range[1], "–", input$year_range[2], " Average")
    })

    # ── Line chart ───────────────────────────────────────
    output$line_chart <- renderPlotly({
      req(nrow(filtered()) > 0)
      has_type <- "Type" %in% names(filtered())

      if (has_type) {
        # Type-aware chart: keep ggplotly for facet support
        p <- suppressWarnings(filtered() %>%
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
          ) +
          scale_x_continuous(breaks = seq(
            min(filtered()$Year), max(filtered()$Year), 1
          )) +
          labs(x = "Year", y = paste0(rate_label, " (%)"),
               color = NULL, linetype = NULL) +
          theme_minimal(base_size = 13) +
          theme(
            axis.text.x      = element_text(angle = 45, hjust = 1),
            legend.position  = "top",
            panel.grid.minor = element_blank()
          ))

        ggplotly(p, tooltip = "text") %>%
          layout(
            legend    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
            hovermode = "closest"
          ) %>%
          style(connectgaps = FALSE) %>%
          config(displayModeBar = FALSE)

      } else {
        # Direct plot_ly for simple location comparison (Poverty / Insurance)
        df <- filtered() %>%
          select(Year, Location, .data[[rate_col]]) %>%
          tidyr::pivot_wider(names_from = Location, values_from = .data[[rate_col]]) %>%
          arrange(Year)

        p <- plot_ly(df, x = ~Year)
        if ("Geneva" %in% names(df)) {
          p <- p %>% add_trace(
            y      = ~Geneva,
            name   = "Geneva",
            type   = "scatter",
            mode   = "lines+markers",
            line   = list(color = location_colors[["Geneva"]], width = 2.5),
            marker = list(color = location_colors[["Geneva"]], size = 7),
            connectgaps = TRUE,
            hovertemplate = paste0("Geneva: %{y:.1f}%<extra></extra>")
          )
        }
        if ("Ontario" %in% names(df)) {
          p <- p %>% add_trace(
            y      = ~Ontario,
            name   = "Ontario County",
            type   = "scatter",
            mode   = "lines+markers",
            line   = list(color = location_colors[["Ontario"]], width = 2, dash = "dash"),
            marker = list(color = location_colors[["Ontario"]], size = 5),
            connectgaps = TRUE,
            hovertemplate = paste0("Ontario County: %{y:.1f}%<extra></extra>")
          )
        }
        if ("NYS" %in% names(df)) {
          p <- p %>% add_trace(
            y      = ~NYS,
            name   = "New York State",
            type   = "scatter",
            mode   = "lines+markers",
            line   = list(color = location_colors[["NYS"]], width = 2, dash = "dot"),
            marker = list(color = location_colors[["NYS"]], size = 5),
            connectgaps = TRUE,
            hovertemplate = paste0("NYS: %{y:.1f}%<extra></extra>")
          )
        }

        p %>% layout(
            xaxis     = list(title = "Year", tickmode = "linear", dtick = 1,
                             tickangle = -45),
            yaxis     = list(title = paste0(rate_label, " (%)"),
                             ticksuffix = "%", rangemode = if (id == "insurance") "normal" else "tozero"),
            legend    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
            hovermode = "x unified",
            margin    = list(l = 50, r = 20, t = 20, b = 60)
          ) %>%
          config(displayModeBar = FALSE)
      }
    })

    # ── Comparison chart (slope or small multiples) ──────
    output$comparison_chart <- renderPlotly({
      df <- filtered()
      req(nrow(df) > 0)

      if ("Type" %in% names(df)) {
        # ── Small multiples: faceted line chart by household type ──
        p <- suppressWarnings(df %>%
          mutate(line_color = if_else(Location == "Geneva", "Geneva", "Other")) %>%
          ggplot(aes(
            x     = Year,
            y     = .data[[rate_col]],
            group = Location,
            color = line_color,
            text  = paste0(
              "<b>", Location, "</b><br>",
              "Year: ", Year, "<br>",
              "Type: ", Type, "<br>",
              rate_label, ": ", .data[[rate_col]], "%"
            )
          )) +
          geom_line(linewidth = 1) +
          geom_point(size = 2) +
          facet_wrap(~ Type, ncol = 3) +
          scale_color_manual(values = c("Geneva" = "#D94F4F", "Other" = "#BBBBBB")) +
          scale_x_continuous(breaks = \(x) pretty(x, n = 4)) +
          labs(x = "Year", y = paste0(rate_label, " (%)"), color = NULL) +
          theme_minimal(base_size = 13) +
          theme(
            legend.position = "none",
            axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
            strip.text      = element_text(face = "bold", size = 11),
            panel.spacing   = unit(1, "lines")
          ))

        ggplotly(p, tooltip = "text") %>%
          layout(hovermode = "closest") %>%
          config(displayModeBar = FALSE)

      } else {
        # ── Slope chart: first vs last year per location (direct plot_ly) ──
        start_yr <- min(df$Year)
        end_yr   <- max(df$Year)
        req(start_yr < end_yr)

        slope_df <- df %>%
          filter(Year == start_yr | Year == end_yr) %>%
          select(Year, Location, .data[[rate_col]]) %>%
          tidyr::pivot_wider(names_from = Location, values_from = .data[[rate_col]]) %>%
          arrange(Year) %>%
          mutate(endpoint = factor(Year))

        make_slope_trace <- function(p, col, name, color, width, dash, size) {
          vals <- slope_df[[col]]
          p %>% add_trace(
            x      = ~endpoint,
            y      = vals,
            name   = name,
            type   = "scatter",
            mode   = "lines+markers+text",
            line   = list(color = color, width = width, dash = dash),
            marker = list(color = color, size = size),
            text   = paste0(vals, "%"),
            textposition = "top center",
            textfont     = list(size = 11),
            hovertemplate = paste0(name, ": %{y:.1f}%<extra></extra>")
          )
        }

        p <- plot_ly(slope_df, x = ~endpoint)
        if ("Geneva" %in% names(slope_df)) p <- p %>% make_slope_trace("Geneva",  "Geneva",         location_colors[["Geneva"]],  2.5, "solid", 8)
        if ("Ontario" %in% names(slope_df)) p <- p %>% make_slope_trace("Ontario", "Ontario County",  "#BBBBBB",                    2,   "dash",  6)
        if ("NYS" %in% names(slope_df)) p <- p %>% make_slope_trace("NYS",     "New York State",  "#BBBBBB",                    2,   "dot",   6)
        
        p %>% layout(
            xaxis     = list(title = ""),
            yaxis     = list(title = paste0(rate_label, " (%)"),
                             ticksuffix = "%"),
            legend    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
            hovermode = "x unified",
            margin    = list(l = 50, r = 20, t = 30, b = 30)
          ) %>%
          config(displayModeBar = FALSE)
      }
    })
  })
}
