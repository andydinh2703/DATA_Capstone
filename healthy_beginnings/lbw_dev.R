# ──────────────────────────────────────────────────────────
# lbw_dev.R — Standalone dev/test app for the lbw module
# NOT sourced by main app. Run directly in RStudio to test.
# ──────────────────────────────────────────────────────────

library(here)
library(shiny)
library(bslib)
library(plotly)
library(leaflet)
library(htmltools)
library(tidyverse)
library(tigris)
library(sf)

source(here::here("healthy_beginnings", "global.R"))
source(here::here("healthy_beginnings", "lbw.R"))

ui <- fluidPage(
  titlePanel("LBW Dev — Low Birth Weight"),
  make_lbw_ui("lbw", "Low Birth Weight")
)

server <- function(input, output, session) {
  make_lbw_server("lbw", birth_rate_with_counties)
}

shinyApp(ui, server)
