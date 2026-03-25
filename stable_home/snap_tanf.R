# ──────────────────────────────────────────────────────────
# snap_tanf.R — SNAP / TANF Module Functions
# Provides: make_snap_tanf_ui, make_snap_tanf_server
# ──────────────────────────────────────────────────────────

# ── Helper: line chart ───────────────────────────────────
snap_line_plot <- function(df, metric, colors) {
  ggplot(df, aes(x = Year, y = .data[[metric]],
                 color = Community, group = Community)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = pretty(df$Year, n = 8)) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 10, angle = 35, hjust = 1))
}

# ── Helper: slope chart (% change from first year) ───────
snap_slope_plot <- function(df, colors) {
  df <- df %>%
    group_by(Community) %>%
    mutate(
      baseline   = val[Year == min(Year)],
      pct_change = round((val - baseline) / baseline * 100, 1)
    ) %>%
    ungroup()

  ggplot(df, aes(x = Year, y = pct_change,
                 color = Community, group = Community,
                 text = paste0(Community,
                               "<br>Year: ", Year,
                               "<br>Change: ", pct_change, "%"))) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = pretty(df$Year, n = 8)) +
    labs(y = "% Change from First Year", x = "Year") +
    theme_minimal(base_size = 16) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 10, angle = 35, hjust = 1))
}

# ── Helper: KPI (average within filtered range) ──────────
snap_kpi <- function(df, community, metric) {
  vals <- df %>%
    filter(Community == community) %>%
    pull(metric)
  if (length(vals) == 0 || all(is.na(vals))) return("—")
  avg_val <- mean(vals, na.rm = TRUE)
  if (grepl("PCT$", metric)) {
    paste0(round(avg_val, 1), "%")
  } else {
    format(round(avg_val, 0), big.mark = ",")
  }
}

# ── UI builder ───────────────────────────────────────────
make_snap_tanf_ui <- function(id, label) {
  ns <- NS(id)
  nav_panel(
    title = label,
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        checkboxGroupInput(
          ns("community"), "Communities",
          choices  = snap_communities,
          selected = c("Geneva", "Ontario")
        ),
        radioButtons(
          ns("metric"), "Metric",
          choices  = c("Count" = "count", "Percent" = "pct"),
          selected = "count"
        ),
        sliderInput(
          ns("year_range"), "Year Range",
          min = snap_year_min, max = snap_year_max,
          value = c(snap_year_min, snap_year_max),
          step = 1, sep = "", ticks = FALSE
        )
      ),

      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title    = "Geneva",
          value    = textOutput(ns("kpi_geneva")),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = community_colors["Geneva"], fg = "#fff")
        ),
        value_box(
          title    = "Ontario County",
          value    = textOutput(ns("kpi_ontario")),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = community_colors["Ontario"], fg = "#fff")
        ),
        value_box(
          title    = "Geneva Town",
          value    = textOutput(ns("kpi_geneva_town")),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = community_colors["Geneva Town"], fg = "#fff")
        ),
        value_box(
          title    = "Ontario w/o Geneva",
          value    = textOutput(ns("kpi_ontario_wo")),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = community_colors["Ontario wo Geneva"], fg = "#fff")
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(textOutput(ns("line_title"))),
          plotlyOutput(ns("line_chart"), height = "450px")
        ),
        card(
          card_header(textOutput(ns("slope_title"))),
          plotlyOutput(ns("slope_chart"), height = "450px")
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────
make_snap_tanf_server <- function(id, program) {
  # program: "SNAP" or "TANF"
  metric_col <- program
  pct_col    <- paste0(program, "PCT")

  moduleServer(id, function(input, output, session) {

    active_col <- reactive({
      if (input$metric == "count") metric_col else pct_col
    })

    filtered <- reactive({
      snap_data %>%
        filter(
          Community %in% input$community,
          Year >= input$year_range[1],
          Year <= input$year_range[2]
        )
    })

    # ── KPIs (average within year range) ─────────────────
    output$kpi_geneva      <- renderText(snap_kpi(filtered(), "Geneva",            active_col()))
    output$kpi_ontario     <- renderText(snap_kpi(filtered(), "Ontario",           active_col()))
    output$kpi_geneva_town <- renderText(snap_kpi(filtered(), "Geneva Town",       active_col()))
    output$kpi_ontario_wo  <- renderText(snap_kpi(filtered(), "Ontario wo Geneva", active_col()))

    # ── Chart titles ─────────────────────────────────────
    output$line_title  <- renderText(paste(program, "Trend Over Time"))
    output$slope_title <- renderText(paste(program, "% Change from First Year"))

    # ── Line chart ───────────────────────────────────────
    output$line_chart <- renderPlotly({
      p <- snap_line_plot(filtered(), active_col(), community_colors)
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })

    # ── Slope chart ──────────────────────────────────────
    output$slope_chart <- renderPlotly({
      df <- filtered() %>%
        group_by(Community, Year) %>%
        summarise(val = mean(.data[[active_col()]], na.rm = TRUE), .groups = "drop")
      p <- snap_slope_plot(df, community_colors)
      ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
  })
}
