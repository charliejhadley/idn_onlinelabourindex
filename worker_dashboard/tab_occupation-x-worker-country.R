output$occupation_barchart_hc <- renderHighchart({
  categories_column <- input$global_trends_group_by
  
  if (categories_column == "region") {
    subcategories_column <- "occupation"
  }
  
  if (categories_column == "occupation") {
    subcategories_column <- "region"
  }
  
  if (categories_column == "country") {
    subcategories_column <- "occupation"
  }
  
  top_n_categories <- switch (
    categories_column,
    "country" = 20,
    "region" = NA,
    "occupation" = NA
  )
  
  value_column <- sym("num_workers")
  
  worker_data %>%
    subcategorised_employment(categories_column,
                              subcategories_column,
                              value_column,
                              top_n = top_n_categories) %>%
    hc_subcategorised_employment(categories_column,
                                 subcategories_column,
                                 input$occupation_bar_stackby) %>%
    hc_theme_onlinelabourindex_workers(
      worker_data,
      categories_column,
      subcategories_column,
      value_column,
      top_n_categories
    )
  
})