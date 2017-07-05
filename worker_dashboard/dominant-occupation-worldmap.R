
country_dominants_label <- function(name, occ.1, occ.2, occ.3) {
  if (is.na(occ.1)) {
    return(paste("There is not yet enough data about", name))
  } else {
    top1 <- paste("<b>",
                  name,
                  "top occupations:",
                  "</b>",
                  "<br/>",
                  "1.",
                  occ.1,
                  "<br/>")
  }
  
  if (is.na(occ.2)) {
    top2 <-
      paste(top1,
            "There were no other occupations recorded for this country")
  } else {
    top2 <- paste(top1, "2.", occ.2, "<br/>")
  }
  if (is.na(occ.3)) {
    top2
  } else {
    paste(top2, "3.", occ.3)
  }
  
}


worldmap_dominant_palette <- colorFactor(occupation_colours$colour,
                                         domain = occupation_colours$label,
                                         levels = occupation_colours$occupation)





## Calculate top 3 occupations for each country
top3_occuptations <- worker_data %>%
  filter(timestamp == max(timestamp)) %>%
  group_by(occupation, country) %>%
  summarise(total.workers = sum(num_workers),
            total.projects = sum(num_projects)) %>%
  group_by(country) %>%
  mutate(workers.in.country = sum(total.workers)) %>%
  filter(workers.in.country > 30) %>% ## Drop countries with fewer than 30 workers in total.
  mutate(calc = total.workers / sum(total.workers)) %>%
  select(occupation, calc) %>%
  top_n(3, calc) %>%
  arrange(desc(calc)) %>%
  mutate(id = paste0("occupation.", row_number())) %>%
  ungroup() %>%
  select(-calc) %>%
  spread(id, occupation) %>%
  select(country, occupation.1, occupation.2, occupation.3)

## Add top3 occupations to the worldmap_dominant_occupation_shapefiles
worldmap_dominant_occupation_shapefiles <- world_shapefiles %>%
  mutate(
    occupation.1 = ifelse(
      name %in% worker_data$country,
      plyr::mapvalues(
        name,
        from = top3_occuptations$country,
        to = top3_occuptations$occupation.1,
        warn_missing = FALSE
      ),
      NA
    ),
    occupation.1 = ifelse(
      occupation.1 %in% occupation_colours$occupation,
      occupation.1,
      NA
    ),
    occupation.2 = ifelse(
      name %in% worker_data$country,
      plyr::mapvalues(
        name,
        from = top3_occuptations$country,
        to = top3_occuptations$occupation.2,
        warn_missing = FALSE
      ),
      NA
    ),
    occupation.2 = ifelse(
      occupation.2 %in% occupation_colours$occupation,
      occupation.2,
      NA
    ),
    occupation.3 = ifelse(
      name %in% worker_data$country,
      plyr::mapvalues(
        name,
        from = top3_occuptations$country,
        to = top3_occuptations$occupation.3,
        warn_missing = FALSE
      ),
      NA
    ),
    occupation.3 = ifelse(
      occupation.3 %in% occupation_colours$occupation,
      occupation.3,
      NA
    ),
    most.represented.occupation.percent = ifelse(
      name %in% worker_data$country,
      plyr::mapvalues(name, from = top3_occuptations$country, to = top3_occuptations$occupation.1, warn_missing = FALSE),
      NA
    ),
    colour = ifelse(
      name %in% worker_data$country,
      plyr::mapvalues(
        top3_occuptations$country,
        from = occupation_colours$occupation,
        to = occupation_colours$colour,
        warn_missing = FALSE
      ),
      NA
    )
  )

dominant_label_data <- worldmap_dominant_occupation_shapefiles %>%
  select(name, occupation.1, occupation.2, occupation.3)
st_geometry(dominant_label_data) <- NULL


dominant_popup_labels <-
  unlist(lapply(1:nrow(dominant_label_data), function(x) {
    do.call("country_dominants_label", unname(as.list(dominant_label_data[x, ])))
  }))

worldmap_dominant_occupation_shapefiles$dominant_popup_labels <- dominant_popup_labels

