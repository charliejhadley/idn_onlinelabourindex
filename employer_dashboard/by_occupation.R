## ==== By occupation tab

output$occupation_rollmean_k_UI <- renderUI({
  radioButtons(
    "occupation_rollmean_k",
    label = "",
    choices = list(
      "Show daily value" = 1,
      "Show 28-day moving average" = 28
    ),
    selected = 28,
    inline = TRUE
  )
})

output$occupation_xts_highchart <- renderHighchart({
  if (is.null(input$occupation_rollmean_k)) {
    return()
  }
  
  index_by_occupation <- gig_economy_by_occupation %>%
    filter(occupation != "Total") %>%
    group_by(occupation) %>%
    arrange(date) %>%
    mutate(moving.average = rollmean(count, k = as.numeric(input$occupation_rollmean_k), na.pad = TRUE, align = "right")) %>%
    ungroup()
  
  legend_order <- index_by_occupation %>%
    group_by(occupation) %>%
    summarise(total = sum(count)) %>%
    arrange(desc(total)) %>%
    select(occupation) %>%
    unique() %>%
    .[[1]]
  
  index_by_occupation <- index_by_occupation %>%
    mutate(occupation = factor(occupation, levels = legend_order))
  
  smaller_dataset <- index_by_occupation %>%
    select(date, moving.average, occupation) %>%
    rename(x = date,
           y = moving.average,
           group = occupation)
  
  highchart(type = "stock") %>%
    hc_add_series(data = smaller_dataset,
                  type = "line",
                  hcaes(x = x,
                        y = y,
                        group = group)
    ) %>%
    hc_plotOptions(series = list(animation = FALSE, boostThreshold = 1)) %>%
    hc_scrollbar(liveRedraw = FALSE) %>%
    hc_tooltip(valueDecimals = 1,
               xDateFormat = "%d %b %Y",
               animation = FALSE,
               split = FALSE,
               shared = TRUE) %>%
    hc_yAxis(opposite = FALSE,
             title = list("text" = "Online Labour Index")) %>%
    custom_ts_selector %>%
    iLabour_branding %>%
    hc_legend(enabled = TRUE, reverse = TRUE, layout = "horizontal",
              itemStyle = list(width = 150))
  
  
})
