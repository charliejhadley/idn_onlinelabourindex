

ui <- fluidPage(
  fluidRow(column(
    radioButtons(
      "occupation_rollmean_k",
      label = "",
      choices = list(
        "Show daily value" = 1,
        "Show 28-day moving average" = 28
      ),
      selected = 28,
      inline = TRUE
    ),
    width = 12
  )),
  checkboxGroupInput(
    "selected_occupations",
    label = "Selected Occupations",
    inline = TRUE,
    width = "100%"
  ),
  dygraphOutput("dyg_occupation")
)


server <- function(input, output, session) {
  output$selected_occupations <- renderUI({
    checkboxGroupInput(
      "selected_occupations",
      label = "Selected Occupations",
      choices = levels(index_by_occupation$occupation),
      selected = levels(index_by_occupation$occupation),
      inline = TRUE,
      width = "100%"
    )
    
  })
  
  
  index_by_occupation <-
    eventReactive(c(input$selected_occupations, input$occupation_rollmean_k),
                  {
                    
                    print("start")
                    
                    print("as.numeric(input$occupation_rollmean_k)")
                    print(as.numeric(input$occupation_rollmean_k))
                    
                    index_by_occupation <- gig_economy_by_occupation %>%
                      filter(occupation != "Total") %>%
                      group_by(occupation) %>%
                      arrange(date) %>%
                      mutate(moving.average = rollmean(
                        count,
                        k = as.numeric(input$occupation_rollmean_k),
                        na.pad = TRUE,
                        align = "right"
                      )) %>%
                      ungroup()
                    
                    print("foo")
                    
                    legend_order <-
                      index_by_occupation %>%
                      group_by(occupation) %>%
                      summarise(total = sum(count)) %>%
                      arrange(desc(total)) %>%
                      select(occupation) %>%
                      unique() %>%
                      .[[1]]
                    
                    
                    index_by_occupation %>%
                      mutate(occupation = factor(occupation, levels = legend_order))
                    
                  })
  
  # observeEvent(c(input$selected_occupations, input$occupation_rollmean_k), {
  #   
  #   print("inside observe")
  #   
  #   index_by_occupation <- index_by_occupation()
  #   
  #   print(index_by_occupation)
  #   
  #   updateCheckboxGroupInput(session,
  #                            "selected_occupations",
  #                            choices = levels(index_by_occupation$occupation))
  # })
  
  observe(
    index_by_occupation <- index_by_occupation()
    
    print(index_by_occupation)
    
    updateCheckboxGroupInput(session,
                             "selected_occupations",
                             choices = levels(index_by_occupation$occupation))
  )
  
  
  output$occupation_xts_dyg <- renderDygraph({
    print(levels(index_by_occupation$occupation) %in% input$selected_occupations)
    
    index_by_occupation %>%
      select(date, occupation, moving.average) %>%
      spread(occupation, moving.average) %>%
      timetk::tk_xts(date_var = date) %>%
      dygraph(main = "Split by Occupation",
              width = "100%") %>%
      dyLegend(show = "follow", labelsSeparateLines = TRUE) %>%
      dyRangeSelector() %>%
      dyOptions(colors = RColorBrewer::brewer.pal(length(
        levels(index_by_occupation$occupation)
      ), "Set2")) %>%
      dyVisibility(visibility = levels(index_by_occupation$occupation) %in% input$selected_occupations)
    
  })
  
}

shinyApp(ui, server)
