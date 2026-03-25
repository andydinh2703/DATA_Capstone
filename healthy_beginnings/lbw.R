# ──────────────────────────────────────────────────────────
# lbw.R — Low Birth Weight Heatmap Module Functions
# Provides: make_lbw_ui, make_lbw_server
# Data objects (birth_rate_with_counties) loaded by global.R
# ──────────────────────────────────────────────────────────

# ── UI module ────────────────────────────────────────────
make_lbw_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Exploring Low Birth Weight Across New York State"),
    sliderInput(
      ns("year"), "What year would you like to explore?",
      min = lbw_county_year_min, max = lbw_county_year_max,
      value = lbw_county_year_min, sep = "", step = 1, ticks = FALSE
    ),
    p("This heat map displays the percentage of babies born with a low birth weight
        (<2500g) for each county in New York State. Ontario County, which is where the
        city of Geneva is located, is outlined in red."),
    plotOutput(ns("map"))
  )
}

# ── Server module ────────────────────────────────────────
make_lbw_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- reactive({
      req(input$year)
      data |>
        filter(Year == input$year)
    })
    output$map <- renderPlot({
      ggplot(filtered_data(), aes(fill = percentage)) +
        geom_sf() +
        geom_sf(data = filtered_data() |> filter(County == "Ontario"),
                fill = NA, color = "red", linewidth = 2) +
        theme_void() +
        labs(fill = "Percentage") +
        scale_fill_viridis_c()
    })
  })
}
