# ──────────────────────────────────────────────────────────
# pop_incom.R — Population & Income Index Module Functions
# Provides: make_pi_ui, make_pi_server
# Data objects (pop_income_index, pi_year_min/max, location_levels)
# loaded by stable_home/global.R
# ──────────────────────────────────────────────────────────

# ── UI module ────────────────────────────────────────────
# make_pi_ui: builds the nav_panel tab for a Population or Income Index chart.
#   id               — Shiny module ID (must match make_pi_server call)
#   label            — text shown on the pill tab and card header (e.g. "Population Index")
#   show_description — if TRUE, renders an explanatory card below the chart
#                      (used for the first tab only; FALSE by default)
make_pi_ui <- function(id, label, show_description = FALSE) {
  ns <- NS(id)
  nav_panel(
    title = label,
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
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
      if (show_description) {
        card(
          style = "margin-top: 10px;",
          p(strong("Population and Income Index: "),
            "The population and income index variables represented on the y-axes show how each location has changed over time, relative to the baseline year (2014). Values greater than 100 show an increase since 2014, while values below 100 indicate a decrease.")
        )
      }
    )
  )
}

# ── Server module ────────────────────────────────────────
# make_pi_server: handles reactivity for a Population or Income Index tab.
#   id      — Shiny module ID (must match make_pi_ui call)
#   data    — data frame containing Year, Location, and index columns
#             (pop_income_index from stable_home/global.R)
#   y_var   — column name to plot on y-axis as a string
#             (e.g. "population_index" or "income_index")
#   y_label — human-readable axis label shown on the chart
#             (e.g. "Population Index" or "Income Index")
make_pi_server <- function(id, data, y_var, y_label) {
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
      p <- ggplot(
        filtered_data(),
        aes(
          x     = Year,
          y     = .data[[y_var]],
          color = Location
        )
      ) +
        geom_point(aes(
          text = paste0(
            "Year: ", Year,
            "<br>", y_label, ": ", round(.data[[y_var]], 2),
            "<br>Location: ", Location
          )
        )) +
        geom_smooth(se = FALSE, linewidth = 0.5) +
        labs(x = "Year", y = y_label) +
        theme_minimal()

      ggplotly(p, tooltip = "text")
    })
  })
}
