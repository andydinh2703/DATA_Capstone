# ──────────────────────────────────────────────────────────
# main/app.R — Ready, Set, GROW! Unified Dashboard
# ──────────────────────────────────────────────────────────

source("global.R")

# ════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════
ui <- bslib::page_fluid(
  titlePanel("Ready, Set, GROW!"),
  tabsetPanel(
    id = "tabset",

              # ── Overview Tab ─────────────────────────────
              tabPanel("Overview",
                       titlePanel("Information about the project"),
                       actionButton("g_children_success", "Success for Geneva's Children"),
                       actionButton("goals", "Goals"),
                       actionButton("data_source", "Where the data came from"),
                       br(),
                       br(),
                       uiOutput("overview_text")
              ),

              # ── Healthy Born Tab ─────────────────────────
              tabPanel("Healthy Born",
                       navset_pill(
                         make_im_ui("im", "Infant Mortality"),
                         make_lbw_ui("lbw", "Low Birth Weight")
                       )),

              # ── Stable Home Tab ──────────────────────────
              tabPanel("Stable Home",
                       navset_pill(
                         make_sh_tab_ui(
                           id                 = "poverty",
                           label              = "Poverty",
                           year_min           = poverty_year_min,
                           year_max           = poverty_year_max,
                           rate_label         = "Family Poverty Rate",
                           bottom_chart_label = "Change Over Selected Period"
                         ),
                         make_sh_tab_ui(
                           id                 = "insurance",
                           label              = "Health Insurance",
                           year_min           = insurance_year_min,
                           year_max           = insurance_year_max,
                           rate_label         = "Insurance Coverage",
                           bottom_chart_label = "Change Over Selected Period"
                         ),
                         make_sh_tab_ui(
                           id                 = "family",
                           label              = "Family Structure",
                           year_min           = family_year_min,
                           year_max           = family_year_max,
                           rate_label         = "% of Households",
                           bottom_chart_label = "Trends by Household Type",
                           extra_controls = function(ns) {
                             checkboxGroupInput(
                               ns("family_types"),
                               label    = "Household Type",
                               choices  = c("Two parents", "Single mother", "Single father"),
                               selected = c("Two parents", "Single mother", "Single father")
                             )
                           }
                         ),
                         make_pi_ui(
                           id    = "pi",
                           label = "Population & Income Index"
                         ),
                         make_snap_tanf_ui(
                           id    = "snap_tanf",
                           label = "SNAP & TANF"
                         )
                       )
              ),

              # ── Ready Mind Tab ───────────────────────────
              tabPanel("Ready Mind",
                       navset_pill(
                         make_proficiency_ui("prof", "School Proficiency")
                       ))
  )
)



# ════════════════════════════════════════════════════════
# Server
# ════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Overview Tab ───────────────────────────────────────
  overview_section <- reactiveVal("children")
  observeEvent(input$goals, {
    overview_section("goals")
  })
  observeEvent(input$data_source, {
    overview_section("data_source")
  })
  observeEvent(input$g_children_success, {
    overview_section("children")
  })
  output$overview_text <- renderUI({
    if (overview_section() == "goals") {
      tagList(
        h4("There are three main goals of this project:"),
        tags$ol(
          tags$li("Determine if Geneva's children are born healthy."),
          tags$li("Determine if Geneva's children have a stable home."),
          tags$li("Determine if Geneva's children have a ready mind.")
        )
      )
    } else if (overview_section() == "data_source") {
      p("text about where data came from")
    } else if (overview_section() == "children") {
      tagList(
      h4("Background on Success for Geneva's Children: "),
      p("The mission of Success for Geneva's Children is to mobilize the community to
        improve the health and well-being of all our children and their families."),
      h4("Success for Geneva's Children Statement of Purpose: "),
      p("\"Through understanding the needs and interests of children and their parents,
        we collectively bring resources to improving their quality of life.  We strive to
        build effective interventions and supports, knowing their profound and beneficial
        impact on the individual child, the family, and the community.\"")
      )
    }
  })

  # ── Healthy Born Tab ───────────────────────────────────
  make_im_server(
    "im", data = im, rate_col = "Rate", rate_label = "Infant Mortality Rate"
  )
  make_lbw_server(
    "lbw", data = birth_rate_with_counties
  )

  # ── Stable Home Tab ────────────────────────────────────
  make_sh_tab_server(
    id         = "poverty",
    data       = poverty_df,
    rate_col   = "PCT",
    rate_label = "Family Poverty Rate"
  )

  make_sh_tab_server(
    id         = "insurance",
    data       = insurance_df,
    rate_col   = "PCT",
    rate_label = "Insurance Coverage"
  )

  make_sh_tab_server(
    id         = "family",
    data       = family_df,
    rate_col   = "PCT",
    rate_label = "% of Households",
    extra_filter = function(df, input) {
      req(length(input$family_types) > 0)
      df %>% filter(Type %in% input$family_types)
    }
  )

  make_pi_server("pi", pop_income_index)

  # ── SNAP / TANF ────────────────────────────────────────
  make_snap_tanf_server("snap_tanf")

  # ── Ready Mind Tab ─────────────────────────────────────
  make_proficiency_server("prof", proficiency_df, proficiency_ny_counties)
}

shinyApp(ui, server)
