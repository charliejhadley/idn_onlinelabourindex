library("htmlwidgets")
library("mapview")
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
library("leaflet")
library("sf")
library("viridis")
library("rlang")
library("forcats")
library("leaflet.extras") ## Needed for background color of leaflet map
library("oidnChaRts")

source("data-processing.R")
source("dominant-occupation-worldmap.R")

ui <- shinyUI(
  fluidPage(
    sliderInput("minZoom", "minZoom",
                min = 1,
                max = 5,
                value = 2,
                step = .1),
    actionButton("saveImage",label = "Save Image"),
    leafletOutput("dominant_occupation", height = "700px")
  )
)

server <- function(input, output, session){
  
  observeEvent(input$saveImage,
               {
                 lflet <- plot_shapefiles %>%
                   filter(!name == "Antarctica") %>%
                   leaflet(options = leafletOptions(minZoom = input$minZoom)) %>%
                   addPolygons(
                     label = ~ name,
                     popup = ~ dominant_popup_labels,
                     fillColor = ~ worldmap_dominant_palette(occupation.1),
                     color = "#000000",
                     weight = 0.8,
                     opacity = 1,
                     fillOpacity = 0.8
                   ) %>%
                   setMapWidgetStyle(style = list(background = "#aacbff")) %>%
                   addLegend(
                     colors = occupation_colours$colour,
                     labels = occupation_colours$label,
                     opacity = 0.7
                   )
                 
                 mapshot(lflet, file = "dominant_occupation.png")
                 
               })
  
  output$dominant_occupation <- renderLeaflet({
    worldmap_dominant_occupation_shapefiles %>%
      filter(!name == "Antarctica") %>%
      leaflet(options = leafletOptions(minZoom = input$minZoom)) %>%
      addPolygons(
        label = ~ name,
        popup = ~ dominant_popup_labels,
        fillColor = ~ worldmap_dominant_palette(occupation.1),
        color = "#000000",
        weight = 0.8,
        opacity = 1,
        fillOpacity = 0.8
      ) %>%
      setMapWidgetStyle(style = list(background = "#aacbff")) %>%
      addLegend(
        colors = occupation_colours$colour,
        labels = occupation_colours$label,
        opacity = 0.7
      )
  })
  
}

shinyApp(
  ui = ui,
  server = server
)

