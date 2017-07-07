output$occupation_history_hc <- renderHighchart({
  occupation_history_data <- worker_data %>%
    group_by(timestamp, occupation) %>%
    mutate(total.workers = sum(num_workers)) %>%
    mutate(total.projects = sum(num_projects)) %>%
    select(timestamp, occupation, total.workers, total.projects) %>%
    ungroup() %>%
    unique()
  
  # Use switch as NSE hasn't fully arrived in highcharter yet
  hc <- switch(
    input$occupation_stackedarea_value,
    "total.workers" = {
      hchart(
        occupation_history_data,
        "area",
        hcaes(y = total.workers,
              x = timestamp,
              group = occupation),
        marker = list(radius = 1,
                      symbol = "circle")
      )
    },
    "total.projects" = {
      hchart(
        occupation_history_data,
        "area",
        hcaes(y = total.projects,
              x = timestamp,
              group = occupation),
        marker = list(radius = 1,
                      symbol = "circle")
      )
    }
  )
  
  hc %>%
    hc_plotOptions(area = list(stacking = input$occupation_stackedarea_stackby)) %>%
    hc_tooltip(split = TRUE)
})
