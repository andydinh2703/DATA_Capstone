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
    make_pi_ui("pi", "Population & Income Index")
  )
)

server <- function(input, output, session) {
  make_pi_server("pi", pop_income_index)
}

shinyApp(ui, server)
