## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: otto.kassi@oii.ox.ac.uk
## Data Source: https://dx.doi.org/10.6084/m9.figshare.3761562.v56
## ================================================================================

appCSS <- "
#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #000000;
}
"

library(shiny)
library(highcharter)
library(plotly)
library(dygraphs)
library(htmltools)
library(shinyBS)
library(shinyjs)
library("leaflet")

shinyServer(fluidPage(
  # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.5.2/animate.min.css")),
  theme = "animate.min.css",
  useShinyjs(),
  inlineCSS(appCSS),
  tabsetPanel(
    # tabPanel(
    #   "Worker Allocation Pyramid",
    #   fluidPage(
    #     selectInput(
    #       "pyramid_categories",
    #       "How should pyramid be split?",
    #       choices = c("country", "region")
    #     ),
    #     highchartOutput("population_pyramid_hc")
    #   )
    # ),
    tabPanel(
      "Worker world map",
      fluidPage(
        fluidRow(column(
          radioButtons(
            "world_map_dominant_or_occupation",
            "",
            choices = c(
              "Show top occupation in each country" = "dominant",
              "Select occupation" = "single-occupation"
            ),
            inline = TRUE
          ),
          width = 12
        ),
        style = "position:relative;z-index:10000;"),
        
        div(uiOutput("worldmap_selected_occupation_UI"),
            style = "position:relative;z-index:10000;"),
        
        
        div(id = "loading-choropleth",
            fluidPage(
              h2(class = "animated infinite pulse", "Loading data...")
              # HTML("<img src=images/cruk-logo.png width='50%'></img>")
            )),
        leafletOutput("worker_occupation_choropleth",
                      height = "500px"),
        HTML("<p align='right'>Source: Online Labour Index</p>")
      )
    ),
    tabPanel(
      "Occupation x worker country",
      fluidPage(
        # selectInput(
        #   "occupation_bar_value",
        #   "What should be displayed?",
        #   choices = list("Number of workers" = "~total.workers",
        #                  "Number of projects" = "~total.projects")
        # ),
        selectInput(
          "global_trends_group_by",
          "Group By",
          choices = list(
            "Geographic region" = "region",
            "Occupation" = "occupation",
            "Top 20 countries" = "country"
          ),
          width = "100%"
        ),
        radioButtons(
          "occupation_bar_stackby",
          label = "",
          choices = list(
            "Within group" = "percent",
            "Market share" = "value"
          ),
          selected = "value",
          inline = TRUE
        ),
        highchartOutput("occupation_barchart_hc")
      )
    ),
    # Commented out deliberately
    # tabPanel(
    #   "Worker history",
    #   fluidPage(
    #     selectInput(
    #       "occupation_stackedarea_value",
    #       "What should be displayed?",
    #       choices = list("Number of workers" = "total.workers",
    #                      "Number of projects" = "total.projects"),
    #       selected = "Number of workers"
    #     ),
    #     radioButtons(
    #       "occupation_stackedarea_stackby",
    #       label = "",
    #       choices = list(
    #         "Percentage (useful for comparison)" = "percent",
    #         "Actual values (how many workers/projects)" = "normal"
    #       ),
    #       inline = TRUE
    #     ),
    #     highchartOutput("occupation_history_hc")
    #   )
    # ),
    tabPanel(
      HTML(
        '<span class="glyphicon glyphicon-info-sign" aria-hidden="true""></span>'
      ),
      fluidPage(wellPanel(includeMarkdown(
        knitr::knit("app_description.Rmd")
      )))
    )
  )
  
))