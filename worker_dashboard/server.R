## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: otto.kassi@oii.ox.ac.uk
## Data Source: https://dx.doi.org/10.6084/m9.figshare.3761562
## ================================================================================

library("shiny")
library("rfigshare")
library("lubridate")
library("highcharter")
library("dygraphs")
library("htmltools")
library("tidyverse")
library("shinyBS")
library("shinyjs")
library("leaflet")
library("sf")
library("viridis")
library("rlang")
library("forcats")
library("leaflet.extras") ## Needed for background color of leaflet map

source("data-processing.R", local = T)

iLabour_branding <- function(x) {
  hc_credits(
    hc = x,
    text = 'Source: Online Labour Index',
    enabled = TRUE,
    href = 'http://ilabour.oii.ox.ac.uk/online-labour-index/',
    position = list(align = "right")
  )
}


shinyServer(function(input, output, session) {
  
  source("dominant-occupation-worldmap.R", local = TRUE)$value
  
  source(file = "tab_occupation-x-worker-country.R", local = TRUE)$value
  
  source(file = "tab_worldmap.R", local = TRUE)$value
  
  # Commented out as no longer required.
  # source(file = "tab_stacked-area-chart.R", local = TRUE)$value
      })