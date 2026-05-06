# ──────────────────────────────────────────────────────────
# proficiency.R — School District Proficiency Module
# Provides: make_proficiency_ui, make_proficiency_server
# Data objects (proficiency_df, proficiency_ny_counties, proficiency_counties)
# loaded by ready_mind/global.R
# ──────────────────────────────────────────────────────────

# ── Helper: KPI percentage for a single district ─────────
# Returns formatted "XX.X%" or "N/A" for the given district and column.
proficiency_kpi_pct <- function(df, district, col) {
  val <- df %>% filter(District == district) %>% pull(col)
  if (length(val) == 0 || all(is.na(val))) return("N/A")
  paste0(round(val[1], 1), "%")
}

# ── Helper: Statewide ranking for a single district ──────
# Returns formatted "#NNN / NNN" based on the rank column.
proficiency_kpi_rank <- function(df, district, rank_col, total) {
  val <- df %>% filter(District == district) %>% pull(rank_col)
  if (length(val) == 0 || all(is.na(val))) return("N/A")
  paste0(val[1], " / ", total)
}

# ── Helper: County average for a given subject ───────────
# Returns formatted "XX.X%" for the county mean, or "N/A".
proficiency_kpi_county <- function(county_avg, county, col) {
  val <- county_avg %>% filter(County == county) %>% pull(col)
  if (length(val) == 0 || all(is.na(val))) return("N/A")
  paste0(round(val[1], 1), "%")
}

# ── UI module ────────────────────────────────────────────
# make_proficiency_ui: builds the nav_panel tab for NY School District Proficiency.
#   id    — Shiny module ID (must match make_proficiency_server call)
#   label — text shown on the pill tab (e.g. "School Proficiency")
make_proficiency_ui <- function(id, label) {
  ns <- NS(id)
  nav_panel(
    title = label,
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        radioButtons(
          ns("subject"), "Subject",
          choices  = c("ELA", "Math"),
          selected = "ELA"
        ),
        radioButtons(
          ns("grade"), "Grade",
          choices  = c("Grade 3" = "3", "Grade 4" = "4", "Grade 8" = "8"),
          selected = "4"
        ),

        p("Percentage of students scoring at or above proficiency on New York State ",
          "assessments. Geneva City SD is highlighted in red. Ontario County, where ",
          "Geneva is located, is outlined in red on the map.")
      ),

      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title    = "Geneva City SD",
          value    = textOutput(ns("kpi_geneva_pct")),
          p(textOutput(ns("kpi_geneva_source")), style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("mortarboard-fill"),
          theme    = value_box_theme(bg = "#D94F4F", fg = "#fff")
        ),
        value_box(
          title    = "Ontario County Avg",
          value    = textOutput(ns("kpi_ontario_avg")),
          p(textOutput(ns("kpi_ontario_source")), style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = value_box_theme(bg = "#4CAF7D", fg = "#fff")
        ),
        value_box(
          title    = "NY State Avg",
          value    = textOutput(ns("kpi_state_avg")),
          p(textOutput(ns("kpi_state_source")), style = "margin: 0; font-size: 0.85em; opacity: 0.85;"),
          showcase = bsicons::bs_icon("map-fill"),
          theme    = value_box_theme(bg = "#3D7FBA", fg = "#fff")
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(textOutput(ns("map_title"))),
          leafletOutput(ns("map"), height = "450px")
        ),
        card(
          card_header(textOutput(ns("bar_title"))),
          plotlyOutput(ns("bar_chart"), height = "450px")
        )
      ),

      card(
        card_header(textOutput(ns("trend_title"))),
        plotlyOutput(ns("trend_chart"), height = "350px")
      )
    )
  )
}

# ── Server module ────────────────────────────────────────
# make_proficiency_server: handles reactivity for the proficiency module.
#   id        — Shiny module ID (must match make_proficiency_ui call)
#   data      — district-level data frame (proficiency_df from ready_mind/global.R)
#   county_sf — sf object with county-level averages (proficiency_ny_counties)
make_proficiency_server <- function(id, data, county_sf) {
  moduleServer(id, function(input, output, session) {

    # ── Column name reactives ────────────────────────────
    # Maps the subject toggle to the correct column names in data / county_sf.
    pct_col  <- reactive({ if (input$subject == "ELA") "ela_pct"  else "math_pct"  })
    avg_col  <- reactive({ if (input$subject == "ELA") "ela_avg"  else "math_avg"  })

    total_districts <- nrow(data)

    # ── Most recent grade-specific value for Geneva ───────
    geneva_grade_latest <- reactive({
      df <- geneva_grade_df %>%
        filter(Grade == as.integer(input$grade), Subject == input$subject) %>%
        arrange(desc(Year))
      if (nrow(df) == 0) return(NULL)
      df[1, ]
    })

    # ── KPIs ─────────────────────────────────────────────
    output$kpi_geneva_pct <- renderText({
      val <- data %>% filter(District == "GENEVA CITY SD") %>% pull(pct_col())
      if (length(val) > 0 && !all(is.na(val))) return(paste0(round(val[1], 1), "%"))
      latest <- geneva_grade_latest()
      if (is.null(latest)) return("N/A")
      paste0(round(latest$PCT, 1), "%")
    })
    output$kpi_geneva_source <- renderText({
      val <- data %>% filter(District == "GENEVA CITY SD") %>% pull(pct_col())
      if (length(val) > 0 && !all(is.na(val))) return("")
      latest <- geneva_grade_latest()
      if (is.null(latest)) return("")
      paste0("Grade ", input$grade, " · ", latest$Year)
    })
    output$kpi_ontario_source <- renderText({
      val <- data %>% filter(District == "GENEVA CITY SD") %>% pull(pct_col())
      if (length(val) > 0 && !all(is.na(val))) return("")
      latest <- geneva_grade_latest()
      if (is.null(latest)) return("")
      paste0("Grade ", input$grade, " · ", latest$Year)
    })
    output$kpi_state_source <- renderText({
      val <- data %>% filter(District == "GENEVA CITY SD") %>% pull(pct_col())
      if (length(val) > 0 && !all(is.na(val))) return("")
      latest <- geneva_grade_latest()
      if (is.null(latest)) return("")
      paste0("Grade ", input$grade, " · ", latest$Year)
    })
    output$kpi_geneva_rank <- renderText(
      proficiency_kpi_rank(data, "GENEVA CITY SD", rank_col(), total_districts)
    )
    output$kpi_ontario_avg <- renderText({
      val <- county_sf %>%
        sf::st_drop_geometry() %>%
        filter(County == "Ontario") %>%
        pull(avg_col())
      if (length(val) == 0 || all(is.na(val))) "N/A" else paste0(round(val[1], 1), "%")
    })
    output$kpi_state_avg <- renderText({
      val <- mean(data[[pct_col()]], na.rm = TRUE)
      if (is.na(val)) "N/A" else paste0(round(val, 1), "%")
    })

    # ── Chart titles ─────────────────────────────────────
    output$map_title <- renderText(paste(input$subject, "Proficiency by County"))
    output$bar_title <- renderText(
      paste0(input$subject, " Proficiency — Ontario County")
    )

    # ── Shared helper: adds polygons and legend to any leaflet/proxy object ──
    add_proficiency_layers <- function(map_obj, sf_df, col, subject) {
      domain <- sf_df[[col]]
      pal    <- colorNumeric("viridis", domain = domain, na.color = "transparent")
      labels <- sprintf(
        "<strong>%s County</strong><br/>Avg %s proficiency: %s",
        sf_df$County,
        subject,
        ifelse(is.na(sf_df[[col]]), "N/A", paste0(round(sf_df[[col]], 1), "%"))
      ) %>% lapply(htmltools::HTML)

      map_obj %>%
        addPolygons(
          fillColor    = pal(domain),
          color        = "white",
          weight       = 1,
          fillOpacity  = 0.8,
          label        = labels,
          labelOptions = labelOptions(direction = "auto")
        ) %>%
        addPolygons(
          data   = sf_df %>% filter(County == "Ontario"),
          fill   = FALSE,
          color  = "#D94F4F",
          weight = 3
        ) %>%
        addLegend(
          pal      = pal,
          values   = domain,
          title    = paste(subject, "Avg %"),
          position = "bottomright",
          na.label = "No data"
        )
    }

    # ── Leaflet base map with initial ELA layer ───────────
    output$map <- renderLeaflet({
      add_proficiency_layers(
        leaflet(county_sf) %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lng = -76.1, lat = 43.0, zoom = 6),
        county_sf, col = "ela_avg", subject = "ELA"
      )
    })

    # ── Update map polygons when subject changes ──────────
    observe({
      col   <- avg_col()
      sf_df <- county_sf
      add_proficiency_layers(
        leafletProxy(session$ns("map"), data = sf_df) %>%
          clearShapes() %>%
          clearControls(),
        sf_df, col = col, subject = input$subject
      )
    })

    # ── Geneva grade-level trend ─────────────────────────
    geneva_trend_filtered <- reactive({
      geneva_grade_df %>%
        filter(Grade == as.integer(input$grade), Subject == input$subject)
    })

    output$trend_title <- renderText({
      paste0("Geneva City SD — Grade ", input$grade, " ", input$subject, " Proficiency Over Time")
    })

    output$trend_chart <- renderPlotly({
      df <- geneva_trend_filtered()
      validate(need(nrow(df) > 0, "No data available for the selected grade and subject."))

      plot_ly(
        df,
        x         = ~Year,
        y         = ~PCT,
        type      = "scatter",
        mode      = "lines+markers",
        line      = list(color = "#D94F4F", width = 2),
        marker    = list(color = "#D94F4F", size = 7),
        text      = ~paste0(input$subject, " (Grade ", input$grade, ")<br>Year: ", Year, "<br>", PCT, "% proficient"),
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis  = list(title = "Year", tickformat = "d"),
          yaxis  = list(title = paste(input$subject, "% Proficient"), range = c(0, 100))
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ── Filtered district data for bar chart ─────────────
    filtered_districts <- reactive({
      pct <- pct_col()
      df  <- data %>% filter(County == "Ontario")

      # If Geneva's statewide value is NA, substitute grade-specific most recent
      geneva_idx <- which(df$District == "GENEVA CITY SD")
      if (length(geneva_idx) > 0 && is.na(df[[pct]][geneva_idx])) {
        latest <- geneva_grade_latest()
        if (!is.null(latest)) df[[pct]][geneva_idx] <- latest$PCT
      }

      df %>%
        filter(!is.na(.data[[pct]])) %>%
        arrange(desc(.data[[pct]])) %>%
        mutate(
          bar_color = ifelse(District == "GENEVA CITY SD", "#D94F4F", "#BBBBBB"),
          pct_val   = .data[[pct]],
          District  = factor(District, levels = rev(District))
        )
    })

    # ── Horizontal bar chart ─────────────────────────────
    output$bar_chart <- renderPlotly({
      df <- filtered_districts()
      validate(need(nrow(df) > 0, "No data available for the selected county and subject."))

      plot_ly(
        df,
        x           = ~pct_val,
        y           = ~District,
        type        = "bar",
        orientation = "h",
        marker      = list(color = ~bar_color),
        text        = ~paste0(District, "<br>", pct_val, "%"),
        hoverinfo   = "text"
      ) %>%
        layout(
          xaxis  = list(title = paste(input$subject, "% Proficient"), range = c(0, 100)),
          yaxis  = list(title = ""),
          margin = list(l = 220)
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}
