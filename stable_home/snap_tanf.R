# ──────────────────────────────────────────────────────────
# snap_tanf.R — SNAP / TANF Module Functions
# Provides: make_snap_tanf_ui, make_snap_tanf_server
# Data objects (snap_data, snap_communities, snap_year_min/max,
# community_colors) loaded by stable_home/global.R
# ──────────────────────────────────────────────────────────

# ── Helper: line styles per community ─────────────────────
snap_line_styles <- list(
  "Geneva"            = list(dash = "solid", width = 2.5, size = 7),
  "Ontario"           = list(dash = "dash",  width = 2,   size = 5),
  "Ontario wo Geneva" = list(dash = "dot",   width = 2,   size = 5)
)

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

# ── UI module ────────────────────────────────────────────
# make_snap_tanf_ui: builds a single nav_panel tab for SNAP & TANF.
#   id    — Shiny module ID (must match make_snap_tanf_server call)
#   label — text shown on the pill tab (e.g. "SNAP & TANF")
make_snap_tanf_ui <- function(id, label) {
  ns <- NS(id)
  nav_panel(
    title = label,
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        radioButtons(
          ns("program"), "Program",
          choices  = c("SNAP", "TANF"),
          selected = "SNAP"
        ),
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
        col_widths = c(4, 4, 4),
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

# ── Server module ─────────────────────────────────────────
# make_snap_tanf_server: handles reactivity for the combined SNAP & TANF tab.
#   id — Shiny module ID (must match make_snap_tanf_ui call)
#   Program (SNAP or TANF) is selected via input$program radio button.
make_snap_tanf_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    metric_col <- reactive({ input$program })
    pct_col    <- reactive({ paste0(input$program, "PCT") })

    active_col <- reactive({
      if (input$metric == "count") metric_col() else pct_col()
    })

    filtered <- reactive({
      snap_data %>%
        filter(
          Community %in% input$community,
          Year >= input$year_range[1],
          Year <= input$year_range[2]
        )
    })

    # ── KPIs ─────────────────────────────────────────────
    output$kpi_geneva      <- renderText(snap_kpi(filtered(), "Geneva",            active_col()))
    output$kpi_ontario     <- renderText(snap_kpi(filtered(), "Ontario",           active_col()))
    output$kpi_ontario_wo  <- renderText(snap_kpi(filtered(), "Ontario wo Geneva", active_col()))

    # ── Chart titles ─────────────────────────────────────
    output$line_title  <- renderText(paste(input$program, "Trend Over Time"))
    output$slope_title <- renderText(paste(input$program, "% Change from First Year"))

    # ── Line chart (direct plot_ly) ──────────────────────
    output$line_chart <- renderPlotly({
      df <- filtered()
      req(nrow(df) > 0)

      col <- active_col()
      communities <- unique(df$Community)

      # Pivot wide
      wide <- df %>%
        select(Year, Community, all_of(col)) %>%
        tidyr::pivot_wider(names_from = Community, values_from = all_of(col)) %>%
        arrange(Year)

      p <- plot_ly(wide, x = ~Year)
      for (comm in communities) {
        if (comm %in% names(wide)) {
          style <- snap_line_styles[[comm]] %||% list(dash = "solid", width = 1.5, size = 5)
          clr   <- community_colors[comm] %||% "#999999"
          p <- p %>% add_trace(
            y      = wide[[comm]],
            name   = comm,
            type   = "scatter",
            mode   = "lines+markers",
            line   = list(color = clr, width = style$width, dash = style$dash),
            marker = list(color = clr, size = style$size),
            hovertemplate = paste0(comm, ": %{y:.1f}<extra></extra>")
          )
        }
      }

      p %>%
        layout(
          xaxis     = list(title = "Year", tickmode = "linear", dtick = 1,
                           tickangle = -45),
          yaxis     = list(title = col, rangemode = "tozero"),
          legend    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
          hovermode = "x unified",
          margin    = list(l = 60, r = 20, t = 20, b = 60)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ── Slope chart (% change from first year, direct plot_ly) ──
    output$slope_chart <- renderPlotly({
      df <- filtered() %>%
        group_by(Community, Year) %>%
        summarise(val = mean(.data[[active_col()]], na.rm = TRUE), .groups = "drop")
      req(nrow(df) > 0)

      # Compute % change from first year per community
      df <- df %>%
        group_by(Community) %>%
        mutate(
          baseline   = val[Year == min(Year)],
          pct_change = round((val - baseline) / baseline * 100, 1)
        ) %>%
        ungroup()

      communities <- unique(df$Community)

      # Pivot wide for pct_change
      wide <- df %>%
        select(Year, Community, pct_change) %>%
        tidyr::pivot_wider(names_from = Community, values_from = pct_change) %>%
        arrange(Year)

      p <- plot_ly(wide, x = ~Year)
      for (comm in communities) {
        if (comm %in% names(wide)) {
          style <- snap_line_styles[[comm]] %||% list(dash = "solid", width = 1.5, size = 5)
          clr   <- community_colors[comm] %||% "#999999"
          p <- p %>% add_trace(
            y      = wide[[comm]],
            name   = comm,
            type   = "scatter",
            mode   = "lines+markers",
            line   = list(color = clr, width = style$width, dash = style$dash),
            marker = list(color = clr, size = style$size),
            hovertemplate = paste0(comm, ": %{y:.1f}%<extra></extra>")
          )
        }
      }

      p %>%
        add_trace(
          y      = rep(0, nrow(wide)),
          x      = wide$Year,
          type   = "scatter",
          mode   = "lines",
          line   = list(color = "gray60", width = 1, dash = "dash"),
          showlegend = FALSE,
          hoverinfo  = "skip"
        ) %>%
        layout(
          xaxis     = list(title = "Year", tickmode = "linear", dtick = 1,
                           tickangle = -45),
          yaxis     = list(title = "% Change from First Year",
                           ticksuffix = "%"),
          legend    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
          hovermode = "x unified",
          margin    = list(l = 60, r = 20, t = 20, b = 60)
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}
