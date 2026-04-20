# ──────────────────────────────────────────────────────────
# ell.R — English Language Learner Module Functions
# Provides: make_ell_ui, make_ell_server
# Data objects (ell) loaded by ready_mind/global.R
# ──────────────────────────────────────────────────────────

# ── UI module ────────────────────────────────────────────
# make_ell_ui: builds the nav_panel tab for the ELL section.
#   id    — Shiny module ID (must match make_ell_server call)
#   label — text shown on the pill tab (e.g. "English Language Learners")
make_ell_ui <- function(id, label) {
  ns <- NS(id)
  nav_panel(
    title = label,
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        sliderInput(
          ns("year_range_count"),
          "Select Year Range (Count):",
          min   = min(ell$year_start),
          max   = max(ell$year_start),
          value = c(min(ell$year_start), max(ell$year_start)),
          sep   = "",
          ticks = FALSE
        ),
        sliderInput(
          ns("year_range_prop"),
          "Select Year Range (Proportion):",
          min   = min(ell$year_start),
          max   = max(ell$year_start),
          value = c(min(ell$year_start), max(ell$year_start)),
          sep   = "",
          ticks = FALSE
        ),
        p("This dashboard shows English Language Learner (ELL) trends in Geneva City
          School District over time. The Count chart shows the total number of ELL
          students each year; the Proportion chart shows ELL students as a share of
          total enrollment. The two year-range sliders are independent.")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("ELL Count Over Time"),
          plotlyOutput(ns("ell_count_plot"), height = "500px")
        ),
        card(
          card_header("ELL Proportion Over Time"),
          plotlyOutput(ns("ell_prop_plot"), height = "500px")
        )
      )
    )
  )
}

# ── Server module ────────────────────────────────────────
# make_ell_server: handles reactivity for the ELL module.
#   id   — Shiny module ID (must match make_ell_ui call)
#   data — data frame with columns: year_start, Year, Count, Enrollment, Proportion
#          (ell from ready_mind/global.R)
make_ell_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    filtered_count <- reactive({
      data |>
        dplyr::filter(
          year_start >= input$year_range_count[1],
          year_start <= input$year_range_count[2]
        )
    })

    filtered_prop <- reactive({
      data |>
        dplyr::filter(
          year_start >= input$year_range_prop[1],
          year_start <= input$year_range_prop[2]
        )
    })

    output$ell_count_plot <- renderPlotly({
      df <- filtered_count()
      plot_ly(
        data       = df,
        x          = ~year_start,
        y          = ~Count,
        type       = "scatter",
        mode       = "lines+markers",
        line       = list(color = "steelblue", shape = "spline", width = 3),
        marker     = list(color = "steelblue"),
        name       = "ELL Count",
        text       = ~Year,
        customdata = ~Enrollment,
        hovertemplate = paste(
          "<b>Year:</b> %{text}<br>",
          "<b>ELL Count:</b> %{y}<br>",
          "<b>Total Enrollment:</b> %{customdata}<extra></extra>"
        )
      ) %>%
        layout(
          xaxis = list(
            title     = "Year",
            tickmode  = "array",
            tickvals  = df$year_start[seq(1, nrow(df), 2)],
            ticktext  = df$Year[seq(1, nrow(df), 2)],
            tickangle = 45,
            tickfont  = list(size = 14),
            titlefont = list(size = 16)
          ),
          yaxis = list(
            title     = "ELL Count",
            tickfont  = list(size = 14),
            titlefont = list(size = 16)
          )
        )
    })

    output$ell_prop_plot <- renderPlotly({
      df <- filtered_prop()
      plot_ly(
        data       = df,
        x          = ~year_start,
        y          = ~Proportion,
        type       = "scatter",
        mode       = "lines+markers",
        line       = list(color = "#9CAF88", shape = "spline", width = 3),
        marker     = list(color = "#9CAF88"),
        name       = "ELL Proportion",
        text       = ~Year,
        customdata = ~Count,
        hovertemplate = paste(
          "<b>Year:</b> %{text}<br>",
          "<b>ELL %:</b> %{y:.1%}<br>",
          "<b>ELL Count:</b> %{customdata}<extra></extra>"
        )
      ) %>%
        layout(
          xaxis = list(
            title     = "Year",
            tickmode  = "array",
            tickvals  = df$year_start[seq(1, nrow(df), 2)],
            ticktext  = df$Year[seq(1, nrow(df), 2)],
            tickangle = 45,
            tickfont  = list(size = 14),
            titlefont = list(size = 16)
          ),
          yaxis = list(
            title      = "ELL Proportion of Total Enrollment",
            tickformat = ".0%",
            tickfont   = list(size = 14),
            titlefont  = list(size = 16)
          )
        )
    })
  })
}
