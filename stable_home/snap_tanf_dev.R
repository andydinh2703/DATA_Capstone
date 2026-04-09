# ──────────────────────────────────────────────────────────
# snap_tanf_dev.R — Standalone dev/test app for snap_tanf module
# NOT sourced by main app. Run directly in RStudio to test.
# ──────────────────────────────────────────────────────────

library(here)
library(shiny)
library(bslib)
library(plotly)
library(tidyverse)

source(here::here("stable_home", "global.R"))
source(here::here("stable_home", "snap_tanf.R"))

ui <- fluidPage(
  titlePanel("SNAP & TANF — Dev"),
  navset_pill(
    make_snap_tanf_ui("snap_tanf", "SNAP & TANF")
  )
)

server <- function(input, output, session) {
  make_snap_tanf_server("snap_tanf")
}

shinyApp(ui, server)
