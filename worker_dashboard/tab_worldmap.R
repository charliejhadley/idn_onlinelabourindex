output$worldmap_selected_occupation_UI <- renderUI({
  if (input$world_map_dominant_or_occupation == "dominant") {
    return()
  }
  
  selectInput("worldmap_selected_occupation",
              "",
              choices = sort(unique(worker_data$occupation)))
  
})




source("dominant-occupation-worldmap.R", local = TRUE)$value

worldmap_occupation_max_palvalue <- worker_data %>%
  filter(timestamp == max(timestamp)) %>%
  group_by(occupation) %>%
  mutate(relative.number.of.workers = 100 * num_workers / sum(num_workers)) %>%
  ungroup() %>%
  select(relative.number.of.workers) %>%
  max() %>%
  ceiling()


vili_otto_colours <-
  c("#551c65",
    "#407d98",
    "#36a396",
    "#61c878",
    "#8fd85b",
    "#c6e240",
    "#fce942")

worldmap_occupation_palette <- colorBin(# "viridis",
  vili_otto_colours,
  bin = c(0.1, 2.5, 5, 10, 20, 30, worldmap_occupation_max_palvalue + 5))


# worldmap_occupation_palette <-
#   colorNumeric(
#     "viridis",
#     domain = c(.1,
#                worldmap_occupation_max_palvalue),
#     # domain = NULL,
#     na.color = "#e2e2e2"
#   )

worldmap_occupation_shapefiles <-
  eventReactive(input$worldmap_selected_occupation,
                {
                  worldmap_occupation_data <- worker_data %>%
                    filter(timestamp == max(timestamp)) %>%
                    filter(occupation == input$worldmap_selected_occupation) %>%
                    mutate(relative.number.of.workers = 100 * {
                      num_workers / sum(num_workers)
                    })
                  
                  worldmap_occupation_data %>%
                    select(country, relative.number.of.workers) %>%
                    arrange(desc(relative.number.of.workers))
                  
                  
                  ## Cutout values with less than 1%
                  worldmap_occupation_data <-
                    worldmap_occupation_data %>%
                    filter(relative.number.of.workers >= .1)
                  
                  shp_data <-
                    world_shapefiles %>%
                    mutate(relative.number.of.workers = ifelse(
                      name %in% worker_data$country,
                      plyr::mapvalues(
                        name,
                        from = worldmap_occupation_data$country,
                        to = worldmap_occupation_data$relative.number.of.workers
                      ),
                      NA
                    )) %>%
                    mutate(relative.number.of.workers = as.numeric(relative.number.of.workers))
                  worldmap_occupation_shapefiles <-
                    shp_data
                })



output$worker_occupation_choropleth <- renderLeaflet({
  shinyjs::show("loading-choropleth")
  
  if (input$world_map_dominant_or_occupation == "dominant") {
    plot_shapefiles <- worldmap_dominant_occupation_shapefiles
    shinyjs::hide("loading-choropleth")
    plot_shapefiles %>%
      filter(!name == "Antarctica") %>%
      leaflet(options = leafletOptions(minZoom = 1.3)) %>%
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
    
  } else {
    plot_shapefiles <- worldmap_occupation_shapefiles()
    
    shinyjs::hide("loading-choropleth")
    
    plot_shapefiles %>%
      filter(!name == "Antarctica") %>%
      leaflet(options = leafletOptions(minZoom = 1.6)) %>%
      addPolygons(
        color = "#000000",
        weight = 0.8,
        opacity = 1,
        fillOpacity = 0.8,
        label = ~ name,
        popup = ~ paste(
          "<b>",
          name,
          "</b>",
          "<br/>",
          
          ifelse(
            is.na(relative.number.of.workers),
            paste0(
              "Less than 0.1% of workers in ",
              input$worldmap_selected_occupation,
              " came from this country."
            ),
            paste0(
              round(relative.number.of.workers, 1),
              "% of workers in ",
              input$worldmap_selected_occupation,
              " come from this country."
            )
          )
          
        ),
        fillColor = ~ worldmap_occupation_palette(relative.number.of.workers)
      ) %>%
      addLegend(
        pal = worldmap_occupation_palette,
        value = ~ relative.number.of.workers,
        opacity = 0.9,
        labFormat = labelFormat(
          prefix = "",
          suffix = "%",
          between = " &ndash; ",
          digits = 3,
          big.mark = ",",
          transform = identity
        ),
        title = ~ paste0(
          "Country's share of global ",
          "<br/>",
          input$worldmap_selected_occupation,
          "<br/>",
          " workforce"
        ),
        na.label = "<0.1% of workforce"
      ) %>%
      setMapWidgetStyle(style = list(background = "#aacbff"))
  }
  
  
  
})
