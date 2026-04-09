# ──────────────────────────────────────────────────────────
# lbw.R — Low Birth Weight Heatmap Module Functions
# Provides: make_lbw_ui, make_lbw_server
# Data objects (birth_rate_with_counties, lbw_county_year_min/max)
# loaded by healthy_beginnings/global.R
# ──────────────────────────────────────────────────────────

# ── UI module ────────────────────────────────────────────
# make_lbw_ui: builds the nav_panel tab for the Low Birth Weight section.
#   id    — Shiny module ID (must match make_lbw_server call)
#   label — text shown on the pill tab (e.g. "Low Birth Weight")
make_lbw_ui <- function(id, label) {
  ns <- NS(id)
  nav_panel(
    title = label,
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        sliderInput(
          ns("year"), "What year would you like to explore?",
          min = lbw_county_year_min, max = lbw_county_year_max,
          value = lbw_county_year_min, sep = "", step = 1, ticks = FALSE
        ),
        p("This heat map displays the percentage of babies born with a low birth weight
          (<2500g) for each county in New York State. Ontario County, which is where the
          city of Geneva is located, is outlined in red.")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Low Birth Weight by County"),
          leafletOutput(ns("map"), height = "500px")
        ),
        card(
          card_header("Trend by County"),
          plotlyOutput(ns("lines"), height = "500px")
        )
      )
    )
  )
}

# ── Server module ────────────────────────────────────────
# make_lbw_server: handles reactivity for the LBW module.
#   id   — Shiny module ID (must match make_lbw_ui call)
#   data — sf data frame of low birth weight by county and year
#          (birth_rate_with_counties from healthy_beginnings/global.R)
make_lbw_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    filtered_data <- reactive({
      req(input$year)
      data |>
        filter(Year == input$year) |>
        filter(!sf::st_is_empty(geometry))
    })

    all_data <- reactive({
      data |>
        filter(!sf::st_is_empty(geometry)) |>
        mutate(highlight = ifelse(County == "Ontario", "Ontario", "Other"))
    })

    output$map <- renderLeaflet({
      df <- filtered_data()
      pal <- colorNumeric("viridis", domain = df$percentage, na.color = "transparent")
      labels <- sprintf(
        "<strong>%s</strong><br/>%0.1f%% low birth weight",
        df$County, df$percentage
      ) |> lapply(htmltools::HTML)

      leaflet(df) |>
        addProviderTiles("CartoDB.Positron") |>
        addPolygons(
          fillColor    = ~pal(percentage),
          color        = "black",
          weight       = 1,
          fillOpacity  = 0.8,
          label        = labels,
          labelOptions = labelOptions(direction = "auto")
        ) |>
        addPolygons(
          data   = df |> filter(County == "Ontario"),
          fill   = FALSE,
          color  = "red",
          weight = 3
        ) |>
        addLegend(
          pal      = pal,
          values   = df$percentage,
          title    = "Low Birth Weight (%)",
          position = "bottomright"
        )
    })

    output$lines <- renderPlotly({
      df <- all_data()
      p <- ggplot() +
        geom_line(
          data = df |> filter(highlight == "Other"),
          aes(x = Year, y = percentage, group = County, text = County),
          color = "grey", linewidth = 0.5, alpha = 0.7
        ) +
        geom_line(
          data = df |> filter(highlight == "Ontario"),
          aes(x = Year, y = percentage, group = County, text = County),
          color = "red", linewidth = 1.2, alpha = 0.9
        ) +
        labs(y = "% Low Birth Weight", x = "Year") +
        theme_minimal()
      ggplotly(p, tooltip = "text")
    })
  })
}
