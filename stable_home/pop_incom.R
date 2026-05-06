# ──────────────────────────────────────────────────────────
# pop_incom.R — Population & Income Index Module Functions
# Provides: make_pi_ui, make_pi_server
# Data objects (pop_income_index, pi_year_min/max, location_levels)
# loaded by stable_home/global.R
# ──────────────────────────────────────────────────────────

# ── UI module ────────────────────────────────────────────
# make_pi_ui: builds a single nav_panel tab for Population & Income Index.
#   id    — Shiny module ID (must match make_pi_server call)
#   label — text shown on the pill tab (e.g. "Population & Income Index")
make_pi_ui <- function(id, label) {
  ns <- NS(id)
  nav_panel(
    title = label,
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        radioButtons(
          ns("variable"),
          "Select variable:",
          choices  = c("Population Index" = "population_index",
                       "Income Index"     = "income_index"),
          selected = "population_index"
        ),
        sliderInput(
          ns("year_range"),
          "What year would you like to explore?",
          min   = pi_year_min, max = pi_year_max,
          value = c(pi_year_min, pi_year_max),
          step = 1, sep = "", ticks = FALSE
        ),
        checkboxGroupInput(
          ns("locations"), "Locations",
          choices  = location_levels,
          selected = location_levels
        ),
        uiOutput(ns("warning"))
      ),
      card(
        card_header(label),
        plotlyOutput(ns("plot"), height = "400px")
      ),
      card(
        style = "margin-top: 10px;",
        p(strong("Population and Income Index: "),
          "The population and income index variables represented on the y-axes show how each location has changed over time, relative to the baseline year (2014). Values greater than 100 show an increase since 2014, while values below 100 indicate a decrease.")
      )
    )
  )
}

# ── Server module ────────────────────────────────────────
# make_pi_server: handles reactivity for the combined Population & Income tab.
#   id   — Shiny module ID (must match make_pi_ui call)
#   data — data frame with Year, Location, population_index, income_index columns
#          (pop_income_index from stable_home/global.R)
make_pi_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    filtered_data <- reactive({
      req(input$year_range, input$locations)
      data |>
        filter(
          Year     >= input$year_range[1],
          Year     <= input$year_range[2],
          Location %in% input$locations
        )
    })

    y_var <- reactive({
      input$variable
    })

    y_label <- reactive({
      switch(input$variable,
        "population_index" = "Population Index",
        "income_index"     = "Income Index"
      )
    })

    output$warning <- renderUI({
      if (diff(input$year_range) <= 5) {
        tags$div(
          "Year range must be greater than 5",
          style = "color: red; font-weight: bold;"
        )
      }
    })

    output$plot <- renderPlotly({
      validate(
        need(diff(input$year_range) > 5, " ")
      )

      df <- filtered_data() |>
        select(Year, Location, all_of(y_var())) |>
        tidyr::pivot_wider(names_from = Location, values_from = all_of(y_var())) |>
        arrange(Year)

      # Rename "City of Geneva" if present (fallback safety)
      if ("City of Geneva" %in% names(df)) {
        df <- df |> rename(Geneva = `City of Geneva`)
      }

      p <- plot_ly(df, x = ~Year)
      
      if ("Geneva" %in% names(df)) {
        p <- p |> add_trace(
          y      = ~Geneva,
          name   = "Geneva",
          type   = "scatter",
          mode   = "lines+markers",
          line   = list(color = location_colors[["Geneva"]], width = 2.5),
          marker = list(color = location_colors[["Geneva"]], size = 7),
          connectgaps = TRUE,
          hovertemplate = paste0("Geneva: %{y:.1f}<extra></extra>")
        )
      }
      if ("Ontario" %in% names(df)) {
        p <- p |> add_trace(
          y      = ~Ontario,
          name   = "Ontario County",
          type   = "scatter",
          mode   = "lines+markers",
          line   = list(color = location_colors[["Ontario"]], width = 2, dash = "dash"),
          marker = list(color = location_colors[["Ontario"]], size = 5),
          connectgaps = TRUE,
          hovertemplate = paste0("Ontario County: %{y:.1f}<extra></extra>")
        )
      }
      if ("NYS" %in% names(df)) {
        p <- p |> add_trace(
          y      = ~NYS,
          name   = "New York State",
          type   = "scatter",
          mode   = "lines+markers",
          line   = list(color = location_colors[["NYS"]], width = 2, dash = "dot"),
          marker = list(color = location_colors[["NYS"]], size = 5),
          connectgaps = TRUE,
          hovertemplate = paste0("NYS: %{y:.1f}<extra></extra>")
        )
      }

      p |> layout(
          xaxis     = list(title = "Year", tickmode = "linear", dtick = 1,
                           tickangle = -45),
          yaxis     = list(title = y_label(), rangemode = "normal"),
          legend    = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
          hovermode = "x unified",
          margin    = list(l = 50, r = 20, t = 20, b = 60)
        ) |>
        config(displayModeBar = FALSE)
    })
  })
}
