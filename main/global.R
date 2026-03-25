# ──────────────────────────────────────────────────────────
# global.R — Unified data loading for the main dashboard
# Sources sub-app globals and loads additional datasets
# ──────────────────────────────────────────────────────────

library(tidyverse)
library(shiny)
library(bslib)
library(plotly)
library(tigris)
library(sf)

# ── Source sub-app data ──────────────────────────────────
source(here::here("healthy_beginnings", "global.R"))
source(here::here("stable_home", "global.R"))

# ── Source module files ──────────────────────────────────

source(here::here("stable_home", "app.R"))                   # make_sh_tab_ui/server
source(here::here("stable_home", "pop_incom.R"))             # make_pi_ui/server
source(here::here("stable_home", "snap_tanf.R"))             # make_snap_tanf_ui/server
source(here::here("healthy_beginnings", "lbw.R"))            # make_lbw_ui/server
source(here::here("healthy_beginnings", "infant_mortality.R"))# make_im_ui/server
