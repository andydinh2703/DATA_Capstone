 # ──────────────────────────────────────────────────────────
# main/app.R — Ready, Set, GROW! Unified Dashboard
# ──────────────────────────────────────────────────────────

source("global.R")

# ════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════
# ── Theme ───────────────────────────────────────────────
rsg_theme <- bs_theme(
  version   = 5,
  bg        = "#FAFAFA",
  fg        = "#2C3E50",
  primary   = "#D94F4F",
  secondary = "#4CAF7D",
  info      = "#3D7FBA",
  font_scale = 0.95,
  "card-border-color" = "#E0E0E0"
)

ui <- bslib::page_fluid(
  theme = rsg_theme,
  tags$head(tags$style(HTML("
    /* ── Tab styling ─────────────────────────────────── */
    .nav-tabs .nav-link, .nav-pills .nav-link {
      color: #5D6D7E;
    }
    .nav-tabs .nav-link:hover, .nav-pills .nav-link:hover {
      color: #2C3E50;
    }
    .nav-tabs .nav-link.active {
      font-weight: 600;
      color: #2C3E50;
      border-top: 3px solid #3D7FBA;
      border-bottom: none;
    }
    .nav-tabs .nav-link {
      border-top: 3px solid transparent;
    }
    .nav-pills .nav-link.active {
      background-color: #3D7FBA;
      color: #fff;
    }
    /* ── Card polish ─────────────────────────────────── */
    .card { border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,0.06); }
    .card-header { font-weight: 600; background-color: #F5F5F5; }
    /* ── Sidebar ─────────────────────────────────────── */
    .sidebar { background-color: #F8F8F8; }
    /* ── Value boxes ─────────────────────────────────── */
    .value-box { border-radius: 8px; }
  "))),
  div(
    class = "dashboard-header",
    style = "display: flex; justify-content: space-between; align-items: center;
             padding: 16px 24px; margin-bottom: 8px;
             background: #FFFFFF;
             border-bottom: 3px solid #D94F4F; border-radius: 8px;",
    div(
      h2("Ready, Set, GROW!", style = "margin: 0; font-weight: 700; color: #2C3E50;"),
      p("A Community Dashboard for Geneva's Children",
        style = "margin: 4px 0 0 0; font-size: 0.95em; color: #5D6D7E; font-style: italic;")
    ),
    img(src = "success_logo.png", height = "70px",
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
                                   includeMarkdown("docs/data_source.md")),
                         nav_panel("Dashboard Guide",
                                   includeMarkdown("docs/dashboard_guide.md"))
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
