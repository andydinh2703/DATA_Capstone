# ──────────────────────────────────────────────────────────
# pop_incom_dev.R — Standalone dev/test app for pop_incom module
# NOT sourced by main app. Run directly in RStudio to test.
# ──────────────────────────────────────────────────────────

library(here)
library(shiny)
library(bslib)
library(plotly)
library(tidyverse)

source(here::here("stable_home", "global.R"))
source(here::here("stable_home", "pop_incom.R"))

ui <- fluidPage(
  titlePanel("Population & Income Index — Dev"),
  navset_pill(
    make_pi_ui("pop_index", "Population Index", show_description = TRUE),
    make_pi_ui("inc_index", "Income Index",     show_description = FALSE)
  )
)

server <- function(input, output, session) {
  make_pi_server("pop_index", pop_income_index, "population_index", "Population Index")
  make_pi_server("inc_index", pop_income_index, "income_index",     "Income Index")
}

shinyApp(ui, server)
