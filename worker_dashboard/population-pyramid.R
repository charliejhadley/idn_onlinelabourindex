pop_df <- tibble(
  country = letters[1:10],
  negative.axis = -1 * rnorm(n = 10, mean = 15, sd = 7),
  positive.axis = rnorm(n = 10, mean = 10, sd = 4)
)


pop_df <- pop_df %>%
  mutate(total.length = positive.axis + {
    -1 * negative.axis
  }) %>%
  arrange(desc(total.length))



highchart() %>%
  hc_add_series(pop_df, "bar",
                hcaes(y = positive.axis,
                      x = country),
                name = "Projects") %>%
  hc_add_series(pop_df, "bar",
                hcaes(y = negative.axis,
                      x = country),
                name = "Workers") %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_xAxis(
    list(
      categories = pop_df$country,
      reversed = FALSE,
      opposite = FALSE
    ),
    list(opposite = TRUE,
         reversed = FALSE,
         categories = pop_df$country,
         linkedTo = 0)
  ) %>%
  hc_yAxis(labels = list(
    formatter = JS("function () {
                    return Math.abs(this.value) + '%';
                   }")
  )) %>%
  hc_tooltip(formatter = JS("function () {
                return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' +
                    'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);
            }"))


worker_data %>%
  filter(timestamp == max(timestamp)) %>%
  group_by(country) %>%
  mutate(total.workers = sum(num_workers),
         total.projects = sum(num_projects)) %>%
  ungroup() %>%
  select(-occupation, -num_workers, -num_projects) %>%
  unique() %>%
  mutate(total.workers = total.workers / sum(total.workers))




country_population_pyramid_data <- worker_data %>%
  filter(timestamp == max(timestamp)) %>%
  group_by(country) %>%
  mutate(total.workers = sum(num_workers),
         total.projects = sum(num_projects)) %>%
  ungroup() %>%
  select(-occupation, -num_workers, -num_projects) %>%
  unique() %>%
  mutate(total.workers = 100 * {total.workers / sum(total.workers)},
         total.projects = 100 * {total.projects / sum(total.projects)},
         workers.plus.projects = total.workers + total.projects) %>%
  arrange(desc(workers.plus.projects)) %>%
  slice(1:20)






highchart() %>%
  hc_add_series(country_population_pyramid_data, "bar",
                hcaes(y = total.projects,
                      x = country),
                name = "Projects") %>%
  hc_add_series(country_population_pyramid_data, "bar",
                hcaes(y = -1 * total.workers,
                      x = country),
                name = "Workers") %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_xAxis(
    list(
      categories = country_population_pyramid_data$country,
      reversed = TRUE,
      opposite = FALSE
    ),
    list(opposite = TRUE,
         reversed = TRUE,
         categories = country_population_pyramid_data$country,
         linkedTo = 0)
  ) %>%
  hc_yAxis(labels = list(
    formatter = JS("function () {
                   return Math.abs(this.value) + '%';
                   }")
  )) %>%
  hc_tooltip(formatter = JS("function () {
                            return '<b>' + this.series.name + ' in ' + this.point.category + '</b><br/>' +
                            Highcharts.numberFormat(Math.abs(this.point.y), 0);
                            }"))






