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
library("plotly")
library("highcharter")
library("dygraphs")
library("xts")
library("htmltools")
library("tidyverse")
library("shinyBS")
library("shinyjs")
library("forcats")

# source("oidnChaRts.R")
source("data-processing.R", local = T)

source("hc_subcategorised_employment.R", local = T)

iLabour_branding <- function(x) {
  hc_credits(
    hc = x,
    text = 'Source: Online Labour Index',
    enabled = TRUE,
    href = 'http://ilabour.oii.ox.ac.uk/online-labour-index/',
    position = list(align = "right")
  )
}

custom_ts_selector <- function(x) {
  hc_rangeSelector(hc = x,
                   buttons = list(
                     list(
                       type = 'month',
                       count = 1,
                       text = '1mth'
                     ),
                     list(
                       type = 'month',
                       count = 3,
                       text = '3mth'
                     ),
                     list(
                       type = 'month',
                       count = 6,
                       text = '6mth'
                     ),
                     list(type = 'ytd',
                          text = 'YTD'),
                     list(
                       type = 'year',
                       count = 1,
                       text = '1y'
                     ),
                     list(type = 'all',
                          text = 'All')
                   ))
}

shinyServer(function(input, output, session) {
  ## === Landing Tab
  output$landing_xts_highchart <- renderHighchart({
    if (is.null(input$landing_rollmean_k)) {
      shinyjs::show(id = "loading-content",
                    anim = TRUE,
                    animType = "fade")
    } else {
      shinyjs::hide(id = "loading-content",
                    anim = TRUE,
                    animType = "fade")
    }
    
    
    selected_categories <- "Total"
    
    actual_labour_index <-
      gig_economy_by_occupation[gig_economy_by_occupation$occupation == "Total",]
    
    actual_labour_index <- actual_labour_index %>%
      mutate(moving.average = rollmean(count, k = as.numeric(input$landing_rollmean_k), na.pad = TRUE, align = "right")) ## CORRECT WINDOWING
    
    highchart(type = "stock") %>%
      hc_add_series(data = actual_labour_index,
                    type = "line",
                    hcaes(x = date,
                          y = moving.average),
                    name = "Online Labour Index") %>%
      hc_tooltip(valueDecimals = 1,
                 xDateFormat = "%d %b %Y") %>%
      hc_yAxis("opposite" = FALSE,
               title = list("text" = "Online Labour Index")) %>%
      custom_ts_selector %>%
      iLabour_branding
    
  })
  
  source("by_employer_country.R", local = TRUE)$value
  source("by_occupation.R", local = TRUE)$value
  

  source("tab_occupation-x-emply-country.R", local = TRUE)$value
  
  })