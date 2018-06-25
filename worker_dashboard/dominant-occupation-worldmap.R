
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


top3_occuptations <- top3_occuptations %>%
  rowwise() %>%
  mutate(popup = country_dominants_label(country, occupation.1, occupation.2, occupation.3))

top3_occuptations %>%
  filter(country == "Sudan")

world_dominant_occupations <- world_shapefiles %>%
  left_join(top3_occuptations, by = c("name" = "country")) %>%
  left_join(occupation_colours, by = c("occupation.1" = "occupation")) %>%
  mutate(popup = if_else(is.na(popup), paste("There is not yet enough data about", name), popup))


