
actual_labour_index <-
  gig_economy_by_occupation[gig_economy_by_occupation$occupation == "Total",]

windowed <- actual_labour_index %>%
  mutate(moving.average = rollmean(count, k = 28, na.pad = TRUE, align = "right")) ## CORRECT WINDOWING

highchart(type = "stock") %>%
  hc_add_series(data = windowed,
                type = "line",
                hcaes(x = date,
                      y = moving.average)) %>%
  hc_tooltip(valueDecimals = 1,
             xDateFormat = "%Y") %>%
  hc_yAxis("opposite" = FALSE,
           title = list("text" = "Online Labour Index")) %>%
  custom_ts_selector %>%
  iLabour_branding
  

## ==== by occupation

index_by_occupation <- gig_economy_by_occupation %>%
  filter(occupation != "Total") %>%
  group_by(occupation) %>%
  arrange(desc(date), occupation) %>%
  mutate(moving.average = rollmean(count, k = 28, na.pad = TRUE, align = "right")) %>%
  ungroup()

legend_order <- index_by_occupation %>%
  group_by(occupation) %>%
  mutate(total = sum(count)) %>%
  arrange(desc(total)) %>%
  select(occupation) %>%
  unique() %>%
  .[[1]] %>%
  as.character()

index_by_occupation <- index_by_occupation %>%
  mutate(occupation = factor(occupation, levels = legend_order)) %>%
  arrange(occupation)

index_by_occupation


highchart(type = "stock") %>%
  hc_add_series(data = index_by_occupation,
                type = "line",
                hcaes(x = date,
                      y = moving.average,
                      group = occupation)) %>%
  hc_tooltip(valueDecimals = 1,
             xDateFormat = "%Y") %>%
  hc_yAxis("opposite" = FALSE,
           title = list("text" = "Online Labour Index")) %>%
  custom_ts_selector %>%
  iLabour_branding %>%
  hc_legend(enabled = TRUE, reverse = TRUE)


### ========= by country


selected_country_groups <- c(
  "United States",
  "Canada",
  "other Americas",
  "United Kingdom",
  "other Europe",
  "Australia",
  "India",
  "other Asia and Oceania",
  "all Africa"
)

index_by_countrygroup <- gig_economy_by_boundary %>%
  filter(country_group %in% selected_country_groups) %>%
  group_by(country_group, timestamp) %>%
  summarise(total = sum(count)) %>%
  rename(date = timestamp) %>% 
  group_by(date) %>%
  mutate(total = total / sum(total)) %>%
  ungroup()
    

index_total <- gig_economy_by_occupation %>%
  filter(occupation == "Total") %>%
  filter(date %in% index_by_countrygroup$date) %>%
  select(date, count)


index_by_countrygroup <- index_total %>%
  left_join(index_by_countrygroup) %>%
  mutate(labour.index = count * total) %>%
  arrange(date)


index_by_countrygroup <- index_by_countrygroup %>%
  group_by(country_group) %>%
  arrange(desc(date), country_group) %>%
  mutate(moving.average = rollmean(labour.index, k = as.numeric(input$region_rollmean_k), na.pad = TRUE, align = "right")) %>%
  ungroup()

legend_order <- index_by_countrygroup %>%
  arrange(desc(moving.average)) %>%
  select(country_group) %>%
  unique() %>%
  .[[1]]


index_by_countrygroup <- index_by_countrygroup %>%
  mutate(country_group = factor(country_group, levels = legend_order)) %>%
  arrange(country_group)


highchart(type = "stock") %>%
  hc_add_series(data = index_by_countrygroup,
                type = "line",
                hcaes(x = date,
                      y = moving.average,
                      group = country_group)) %>%
  hc_tooltip(valueDecimals = 1,
             xDateFormat = "%d %b %Y") %>%
  hc_yAxis("opposite" = FALSE,
           title = list("text" = "Online Labour Index")) %>%
  custom_ts_selector %>%
  iLabour_branding %>%
  hc_legend(enabled = TRUE, reverse = TRUE)





