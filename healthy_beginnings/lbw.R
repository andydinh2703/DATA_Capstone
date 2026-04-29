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
        selectInput(
          ns("n_counties"),
          "Number of comparison counties:",
          choices  = c("5" = 5, "10" = 10, "15" = 15, "20" = 20, "All" = "all"),
          selected = 5
        ),
        actionButton(ns("resample"), "Resample Counties"),
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

    # NY-wide 3-year centered rolling average — stable reference regardless of resampling
    avg_data <- reactive({
      data |>
        filter(!sf::st_is_empty(geometry)) |>
        sf::st_drop_geometry() |>
        group_by(Year) |>
        summarise(avg_pct = mean(percentage, na.rm = TRUE), .groups = "drop") |>
        arrange(Year) |>
        mutate(avg_pct = round(zoo::rollmean(avg_pct, k = 3, fill = "extend", align = "center"), 1))
    })

    # Re-sample comparison counties when button clicked or count changes
    filtered_line_data <- eventReactive(list(input$resample, input$n_counties), {
      req(input$n_counties)
      all_counties   <- unique(data$County[!sf::st_is_empty(data$geometry)])
      other_counties <- setdiff(all_counties, "Ontario")
      if (input$n_counties == "all") {
        selected_counties <- other_counties
      } else {
        n <- as.numeric(input$n_counties)
        selected_counties <- sample(other_counties, size = min(n, length(other_counties)))
      }
      data |>
        filter(County %in% c("Ontario", selected_counties)) |>
        filter(!sf::st_is_empty(geometry)) |>
        sf::st_drop_geometry() |>
        mutate(highlight = ifelse(County == "Ontario", "Ontario", "Other"))
    }, ignoreNULL = FALSE)

    # Shared helper: adds polygons, county name labels, and legend to any leaflet/proxy object
    add_map_layers <- function(map_obj, df) {
      pal <- colorNumeric("viridis", domain = df$percentage, na.color = "transparent")
      hover_labels <- sprintf(
        "<strong>%s</strong><br/>%0.1f%% low birth weight",
        df$County, df$percentage
      ) |> lapply(htmltools::HTML)
      centroids <- suppressWarnings(
        sf::st_coordinates(sf::st_centroid(sf::st_geometry(df)))
      )
      map_obj |>
        addPolygons(
          fillColor    = ~pal(percentage),
          color        = "black",
          weight       = 1,
          fillOpacity  = 0.8,
          label        = hover_labels,
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
    }

    # Render base map with initial year's data — avoids blank map on first load
    output$map <- renderLeaflet({
      bbox    <- sf::st_bbox(data)
      init_df <- data |>
        filter(Year == lbw_county_year_min) |>
        filter(!sf::st_is_empty(geometry))

      add_map_layers(
        leaflet(init_df) |>
          addProviderTiles("CartoDB.Positron") |>
          fitBounds(
            lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
          ),
        init_df
      )
    })

    # Update polygons, county labels, and legend when year changes
    observe({
      df <- filtered_data()
      add_map_layers(
        leafletProxy(session$ns("map"), data = df) |>
          clearShapes() |>
          clearMarkers() |>
          clearControls(),
        df
      )
    })

    output$lines <- renderPlotly({
      df  <- filtered_line_data()
      avg <- avg_data()
      p <- suppressWarnings(ggplot() +
        geom_line(
          data = df |> filter(highlight == "Other"),
          aes(x = Year, y = percentage, group = County, text = County),
          color = "grey", linewidth = 0.5, alpha = 0.7
        ) +
        geom_line(
          data = avg,
          aes(x = Year, y = avg_pct, group = 1,
              text = paste0("NY Average: ", avg_pct, "%")),
          color = "steelblue", linewidth = 1, linetype = "dashed"
        ) +
        geom_line(
          data = df |> filter(highlight == "Ontario"),
          aes(x = Year, y = percentage, group = County, text = County),
          color = "red", linewidth = 1.2, alpha = 0.9
        ) +
        labs(y = "% Low Birth Weight", x = "Year") +
        theme_minimal())
      ggplotly(p, tooltip = "text")
    })
  })
}
