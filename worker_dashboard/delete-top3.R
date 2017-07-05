##### ===== Add additional occupation info


## Original
worker_data %>%
  filter(timestamp == max(timestamp)) %>%
  group_by(occupation, country) %>%
  mutate(total.workers = sum(num_workers)) %>%
  mutate(total.projects = sum(num_projects)) %>%
  select(occupation, country, total.workers, colour) %>%
  group_by(country) %>%
  unique() %>%
  mutate(calc = {
    total.workers / sum(total.workers)
  }) %>%
  group_by(country) %>%
  select(-total.workers) %>%
  filter(calc == max(calc)) %>%
  rename(name = country) %>%
  ungroup()

## Modifications

worker_data %>%
  filter(timestamp == max(timestamp)) %>%
  group_by(occupation, country) %>%
  mutate(total.workers = sum(num_workers)) %>%
  mutate(total.projects = sum(num_projects)) %>%
  select(occupation, country, total.workers, colour) %>%
  group_by(country) %>%
  unique() %>%
  mutate(calc = {
    total.workers / sum(total.workers)
  }) %>%
  group_by(country) %>%
  select(-total.workers, -colour) %>%
  unique() %>%
  top_n(3, calc) %>%
  arrange(desc(calc)) %>%
  mutate(id = paste0("occupation.", row_number())) %>%
  ungroup() %>%
  # select(-calc) %>%
  filter(country == "Albania")


top3_occuptations <- worker_data %>%
  filter(timestamp == max(timestamp)) %>%
  group_by(occupation, country) %>%
  mutate(total.workers = sum(num_workers)) %>%
  mutate(total.projects = sum(num_projects)) %>%
  select(occupation, country, total.workers, colour) %>%
  group_by(country) %>%
  unique() %>%
  mutate(calc = {
    total.workers / sum(total.workers)
  }) %>%
  group_by(country) %>%
  select(-total.workers, -colour) %>%
  unique() %>%
  top_n(3, calc) %>%
  arrange(desc(calc)) %>%
  mutate(id = paste0("occupation.", row_number())) %>%
  ungroup() %>%
  select(-calc) %>%
  spread(id, occupation) %>%
  select(country, occupation.1, occupation.2, occupation.3)


dominant_shapefiles <- world_shapefiles %>%
  mutate(
    occupation.1 = ifelse(
      name %in% worker_data$country,
      plyr::mapvalues(name, from = top3_occuptations$country, to = top3_occuptations$occupation.1),
      NA
    ),
    occupation.1 = ifelse(
      occupation.1 %in% occupation_colours$occupation,
      occupation.1,
      NA
    ),
    occupation.2 = ifelse(
      name %in% worker_data$country,
      plyr::mapvalues(name, from = top3_occuptations$country, to = top3_occuptations$occupation.2),
      NA
    ),
    occupation.2 = ifelse(
      occupation.2 %in% occupation_colours$occupation,
      occupation.2,
      NA
    ),
    occupation.3 = ifelse(
      name %in% worker_data$country,
      plyr::mapvalues(name, from = top3_occuptations$country, to = top3_occuptations$occupation.3),
      NA
    ),
    occupation.3 = ifelse(
      occupation.3 %in% occupation_colours$occupation,
      occupation.3,
      NA
    ),
    most.represented.occupation.percent = ifelse(
      name %in% worker_data$country,
      plyr::mapvalues(name, from = top3_occuptations$country, to = top3_occuptations$occupation.1),
      NA
    ),
    colour = ifelse(
      name %in% worker_data$country,
      plyr::mapvalues(top3_occuptations$country, from = occupation_colours$occupation, to = occupation_colours$colour),
      NA
    )
  )

worldmap_dominant_palette <- colorFactor(occupation_colours$colour,
                                         domain = occupation_colours$label,
                                         levels = occupation_colours$occupation)


country_dominants_label <- function(name, occ.1, occ.2, occ.3){

  
  if(is.na(occ.1)){
    return(paste("There is not yet enough data about", name))
  } else {
    top1 <- paste(
      "<b>",
      name,
      "top occupations:",
      "</b>",
      "<br/>",
      "1.",occ.1,
      "<br/>")
  }
  
  
    if(is.na(occ.2)){
      
      top2 <- paste(top1, "There were no other occupations recorded for this country")
      
    } else {
      top2 <- paste(top1, "2.",occ.2, "<br/>")
      
    }
  
  if(is.na(occ.3)){
    top2
  } else {
    paste(top2, "3.",occ.3)
  }


}

country_dominants_label("Canada", "Clerical", "333", NA)
country_dominants_label("Canada", NA, NA, NA)


dominant_shapefiles <- dominant_shapefiles %>%
  mutate(map.popup = country_dominants_label(name, occupation.1, occupation.2, occupation.3))




my_labeller <- function(country, occ1){
  
  internal_1 <- occ1
  print)
  
  if(is.na(internal_1)){
    paste("Country is NA")
  } else {
    paste("Country is not NA")
  }
}

dominant_shapefiles %>%
  filter(!name == "Antarctica") %>%
  leaflet(options = leafletOptions(minZoom = 1.6)) %>%
  addPolygons(
    label = ~ name,
    # popup = ~ country_dominants_label(name, occ.1 = occupation.1, occ.2 = occupation.2, occ.3 = occupation.3),
    popup = ~ map.popup,
    fillColor = ~ worldmap_dominant_palette(occupation.1),
    color = "#000000",
    weight = 0.8,
    opacity = 1,
    fillOpacity = 0.8
  ) %>%
  addLegend(colors = occupation_colours$colour,
            labels = occupation_colours$label) %>%
  setMapWidgetStyle(style = list(background = "#aacbff"))













