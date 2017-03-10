stacked_bar_chart <- function(data = NA,
                              library = "highcharter",
                              categories_column = NA,
                              categories_order = NA,
                              subcategories_column = NA,
                              value_column = NA,
                              stacking_type = NA,
                              ordering_function = NA,
                              subcategories_order = NA) {
  switch (
    library,
    "highcharter" = hc_stacked_bar_chart(
      data = data,
      categories_column = categories_column,
      categories_order = categories_order,
      subcategories_column = subcategories_column,
      value_column = value_column,
      stacking_type = stacking_type,
      ordering_function = ordering_function,
      subcategories_order = subcategories_order
    ),
    "plotly" = {
      plotly_stacked_bar_chart(
        data = data,
        categories_column = categories_column,
        categories_order = categories_order,
        subcategories_column = subcategories_column,
        value_column = value_column,
        stacking_type = stacking_type,
        ordering_function = ordering_function,
        subcategories_order = subcategories_order
      )
    }
  )
}

#' highchart bar chart
hc_stacked_bar_chart <- function(data = NA,
                                 categories_column = NA,
                                 categories_order = NA,
                                 subcategories_column = NA,
                                 value_column = NA,
                                 stacking_type = NA,
                                 ordering_function = NA,
                                 subcategories_order = NA) {
  ## Compute categories_order, if necessary
  if (any(is.na(categories_order))) {
    categories_order <-unique(data[, categories_column])
  } else {
    categories_order <- categories_order
  }
  
  ## make wide
  ungroup(data) %>%
    spread_(subcategories_column, value_column) %>%
    setNames(make.names(names(.))) -> wide_data
  ## order category columns by categories_order
  wide_data <- wide_data[match(categories_order, wide_data[[categories_column]]), ]
  
  data_columns <- setdiff(colnames(wide_data), categories_column)
  
  subcategories_order <- make.names(subcategories_order)
  
  # if (is.na(ordering_function)) {
  #   ordered_measures <- data_columns
  # } else {
  #   ordered_measures <-
  #     data_columns[order(unlist(lapply(data_columns, function(x) {
  #       ordering_function(wide_data[, x])
  #     })), decreasing = TRUE)]
  # }
  chart <-
    highchart() %>% hc_xAxis(categories = categories_order, title = categories_column)
  
  invisible(lapply(data_columns, function(x) {
    chart <<-
      hc_add_series(
        hc = chart,
        name = gsub("[.]", " ", x),
        data = wide_data %>% select_(x) %>% unlist(use.names = F),
        index = {
          if (any(is.na(subcategories_order))) {
            if (is.na(stacking_type)) {
              length(subcategories_order) - which(subcategories_order == x) - 1
            } else {
              which(subcategories_order == x) - 1
            }
            
            
          } else
            
            if (is.na(stacking_type)) {
              length(subcategories_order) - which(rev(subcategories_order) == x) - 1
            } else {
              which(rev(subcategories_order) == x) - 1
            }
          
        }
      )
  }))
  
  chart %>%
    hc_chart(type = "bar") %>%
    hc_plotOptions(series = list(stacking = as.character(stacking_type))) %>%
    hc_legend(reversed = ifelse(is.na(stacking_type), FALSE, TRUE))
  
}

#' Plotly Stacked Bar Chart.
plotly_stacked_bar_chart <- function(data = NA,
                                     categories_column = NA,
                                     categories_order = NA,
                                     subcategories_column = NA,
                                     value_column = NA,
                                     stacking_type = NA,
                                     ordering_function = NA,
                                     subcategories_order = NA) {
  # sample_data %>% gather_("Activity", "Days", setdiff(colnames(sample_data), "Countries"))
  
  if (!any(is.na(categories_order))) {
    suppressWarnings(data <- left_join({
      df <- data.frame(rev(categories_order))
      colnames(df) <- categories_column
      df
    }, data, by = categories_column))
  }
  
  data_columns <- setdiff(colnames(data), categories_column)
  
  if (is.na(ordering_function)) {
    ordered_measures <- data_columns
  } else {
    ordered_measures <-
      data_columns[order(unlist(lapply(data_columns, function(x) {
        ordering_function(wide_data[, x])
      })), decreasing = TRUE)]
  }
  
  if (!any(is.na(subcategories_order))) {
    data[, subcategories_column] <-
      factor(data[, subcategories_column], levels = rev(subcategories_order))
  }
  
  chart <- plot_ly(
    data = data,
    type = "bar",
    y = ~eval(as.name(categories_column)),
    x = ~eval(as.name(value_column)),
    color = ~eval(as.name(subcategories_column)),
    orientation = "h"
  ) %>%
    layout(
      xaxis = list(title = value_column),
      yaxis = list(title = categories_column)
    )
  
  if (is.na(stacking_type)) {
    chart
  } else {
    chart %>%
      layout(barmode = "stack", barnorm = stacking_type)
  }
}
