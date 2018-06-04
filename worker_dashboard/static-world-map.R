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
library("ggthemes")


source("data-processing.R")
source("dominant-occupation-worldmap.R")

worldmap_dominant_occupation_shapefiles %>%
  filter(!name == "Antarctica") %>%
  # mutate(most.represented.occupation.percent = factor(most.represented.occupation.percent)) %>%
  mutate(
    most.represented.occupation.percent = fct_explicit_na(most.represented.occupation.percent, na_level = "Not enough data"),
    most.represented.occupation.percent = factor(most.represented.occupation.percent, levels = occupation_colours$label)
  ) %>%
  select(most.represented.occupation.percent, colour) %>%
  ggplot(aes(fill = most.represented.occupation.percent)) +
  geom_sf(aes(fill = most.represented.occupation.percent)) +
  scale_fill_manual(values = occupation_colours$colour) +
  theme_map() + 
  theme(panel.background = element_rect(fill='#aacbff'))


ggsave("ggplot2.png")

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
                 lflet <- worldmap_dominant_occupation_shapefiles %>%
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

