 # ──────────────────────────────────────────────────────────
# main/app.R — Ready, Set, GROW! Unified Dashboard
# ──────────────────────────────────────────────────────────

source("global.R")

# ════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════
ui <- bslib::page_fluid(
  div(
    style = "display: flex; justify-content: space-between; align-items: center;
             padding: 8px 16px; margin-bottom: 4px;",
    h2("Ready, Set, GROW!", style = "margin: 0;"),
    img(src = "success_logo.png", height = "65px",
        alt = "Success for Geneva's Children")
  ),
  tabsetPanel(
    id = "tabset",

              # ── Overview Tab ─────────────────────────────
              tabPanel("Overview",
                       navset_pill(
                         nav_panel("Success for Geneva's Children",
                                   includeMarkdown("docs/background_info.md")),
                         nav_panel("Goals",
                                   includeMarkdown("docs/goals.md")),
                         nav_panel("Data Source",
                                   includeMarkdown("docs/data_source.md"))
                       )
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
                         make_family_ui(
                           id    = "family",
                           label = "Family Structure"
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
                         make_proficiency_ui("prof", "School Proficiency"),
                         make_ell_ui("ell", "English Language Learners")
                       ))
  )
)



# ════════════════════════════════════════════════════════
# Server
# ════════════════════════════════════════════════════════
server <- function(input, output, session) {

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

  make_family_server("family", family_df)

  make_pi_server("pi", pop_income_index)

  # ── SNAP / TANF ────────────────────────────────────────
  make_snap_tanf_server("snap_tanf")

  # ── Ready Mind Tab ─────────────────────────────────────
  make_proficiency_server("prof", proficiency_df, proficiency_ny_counties)
  make_ell_server("ell", ell)
}

shinyApp(ui, server)
