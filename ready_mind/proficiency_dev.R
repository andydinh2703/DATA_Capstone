# ──────────────────────────────────────────────────────────
# proficiency_dev.R — Standalone dev/test app for proficiency module
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

source(here::here("ready_mind", "global.R"))
source(here::here("ready_mind", "proficiency.R"))

ui <- fluidPage(
  titlePanel("Ready Mind Dev — School District Proficiency"),
  navset_pill(
    make_proficiency_ui("prof", "School Proficiency")
  )
)

server <- function(input, output, session) {
  make_proficiency_server("prof", proficiency_df, proficiency_ny_counties)
}

shinyApp(ui, server)
