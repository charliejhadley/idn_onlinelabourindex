output$global_trends_group_by_UI <- renderUI({
  selectInput(
    "global_trends_group_by",
    "Group By",
    choices = list(
      "Top 5 countries (others aggregated by region)" = "country_group",
      "Occupation" = "occupation",
      "Top 20 countries" = "country"
    ),
    width = "100%"
  )
})

output$global_trends_stack_by_UI <- renderUI({
  radioButtons(
    "global_trends_stack_by",
    "",
    choices = c("Within group" = "percent", "Market share" = "normal"),
    selected = "normal",
    width = "100%",
    inline = TRUE
  )
})

output$global_trends_stacked_bar_chart <- renderHighchart({
  if (is.null(input$global_trends_group_by)) {
    return()
  }

  categories_column <- input$global_trends_group_by

  if (categories_column == "occupation") {
    subcategories_column <- "country_group"
  } else {
    subcategories_column <- "occupation"
  }
  
  top_n_categories <- switch (categories_column,
    "country" = 20,
    "country_group" = 5,
    "occupation" = NA
  )
  
  
  gig_economy_by_boundary %>%
    subcategorised_employment(categories_column, subcategories_column, top_n = top_n_categories) %>%
    hc_subcategorised_employment(categories_column, subcategories_column, input$global_trends_stack_by)
  
})