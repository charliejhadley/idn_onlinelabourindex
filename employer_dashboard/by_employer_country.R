## ==== By employer country

output$region_rollmean_k_UI <- renderUI({
  radioButtons(
    "region_rollmean_k",
    label = "",
    choices = list(
      "Show daily value" = 1,
      "Show 28-day moving average" = 28
    ),
    selected = 28,
    inline = TRUE
  )
})

output$region_xts_highchart <- renderHighchart({
  if (is.null(input$region_rollmean_k)) {
    return()
  }
  
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
    arrange(date) %>%
    mutate(moving.average = rollmean(
      labour.index,
      k = as.numeric(input$region_rollmean_k),
      na.pad = TRUE,
      align = "right"
    )) %>%
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
    hc_add_series(
      data = index_by_countrygroup,
      type = "line",
      hcaes(x = date,
            y = moving.average,
            group = country_group)
    ) %>%
    hc_tooltip(valueDecimals = 1,
               xDateFormat = "%d %b %Y") %>%
    hc_yAxis("opposite" = FALSE,
             title = list("text" = "Online Labour Index")) %>%
    custom_ts_selector %>%
    iLabour_branding %>%
    hc_legend(enabled = TRUE, reverse = TRUE)
  
})
