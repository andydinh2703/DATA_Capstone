# ──────────────────────────────────────────────────────────
# family_dev.R — Standalone dev/test app for Family Structure module
# NOT sourced by main app. Run directly in RStudio to test.
# ──────────────────────────────────────────────────────────

library(here)
library(shiny)
library(bslib)
library(plotly)
library(tidyverse)

source(here::here("stable_home", "global.R"))
source(here::here("stable_home", "family.R"))

ui <- page_fluid(
  titlePanel("Family Structure — Dev"),
  navset_pill(
    make_family_ui("family", "Family Structure")
  )
)

server <- function(input, output, session) {
  make_family_server("family", family_df)
}

shinyApp(ui, server)
