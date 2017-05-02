library(oidnChaRts)

occupation_bar_chart_data <- worker_data %>%
  filter(timestamp == max(timestamp)) %>%
  group_by(occupation, region) %>%
  mutate(total.workers = sum(num_workers)) %>%
  mutate(total.projects = sum(num_projects)) %>%
  select(occupation, region, total.workers, total.projects) %>%
  ungroup() %>%
  unique()

stacked_bar_chart(
  data = occupation_bar_chart_data,
  library = "highcharter",
  categories.column = ~ occupation,
  subcategories.column = ~ region,
  value.column = ~ total.workers,
  stacking.type = "percent"
)
