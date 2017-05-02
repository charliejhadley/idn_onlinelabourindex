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

shinyServer(fluidPage(
  # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.5.2/animate.min.css")),
  theme = "animate.min.css",
  useShinyjs(),
  inlineCSS(appCSS),
  tabsetPanel(
    tabPanel("OLI",
             fluidPage(
               div(
                 id = "loading-content",
                 fluidPage(
                   h2(class = "animated infinite pulse","Loading data...")
                   # HTML("<img src=images/cruk-logo.png width='50%'></img>")
                   )
               ),
               radioButtons(
                 "landing_rollmean_k",
                 label = "",
                 choices = list(
                   "Show daily value" = 1,
                   "Show 28-day moving average" = 28
                 ),
                 selected = 28,
                 inline = TRUE
               ),
               highchartOutput("landing_xts_highchart", width = "100%", height = "500px")
             )),
    tabPanel("By occupation",
             fluidPage(fluidPage(
               # wellPanel("Add/remove occupations in the box below to change the data shown in the chart"),
               fluidRow(
                 column(uiOutput("occupation_rollmean_k_UI"),
                        width = 12)
               ),
               highchartOutput("occupation_xts_highchart", width = "100%")
             ))),
    tabPanel(
      "By employer country",
      fluidPage(
        # wellPanel("Add/remove regions in the box below to change the data shown in the chart"),
        uiOutput("region_rollmean_k_UI", width = "100%"),
        width = 5,
        highchartOutput("region_xts_highchart", width = "100%"),
        width = "100%"
      )
    ),
    tabPanel("Occupation x country",
             fluidPage(
               # wellPanel("Zoom into the chart by selecting an area of interest, pan around in the chart by holding SHIFT."),
               fluidRow(column(
                 uiOutput("global_trends_group_by_UI"),
                 width = 6
               ),
               column(
                 uiOutput("global_trends_stack_by_UI"),
                 width = 6
               )),
               highchartOutput(
                 "global_trends_stacked_bar_chart",
                 width = "100%",
                 height = "450px"
               )
             )),
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