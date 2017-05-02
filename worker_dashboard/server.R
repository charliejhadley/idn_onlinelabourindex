## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: otto.kassi@oii.ox.ac.uk
## Data Source: https://dx.doi.org/10.6084/m9.figshare.3761562
## ================================================================================

# library(magrittr)
start <- Sys.time()
library(shiny)
# library(rfigshare)
library(lubridate)
library(plotly)
library(highcharter)
library(dygraphs)
library(xts)
library(htmltools)
library(tidyverse)
library(shinyBS)
library(shinyjs)
loaded_libraries <- Sys.time()

library(oidnChaRts)
# source("oidnChaRts.R")
source("data-processing.R", local = T)

loaded_dataprocessing <- Sys.time()

print(paste("Libraries: ", loaded_libraries - start))
print(paste("Data processing: ", loaded_dataprocessing - start))

xts_ma <- function(xts_data = NA,
                   n = 28,
                   sides = 1) {
  xts_data <- xts_data
  xts_data[index(xts_data)] <-
    as.vector(stats::filter(as.vector(xts_data), rep(1 / n, n), sides = sides)) # http://stackoverflow.com/a/4862334/1659890
  xts_data
}

iLabour_branding <- function(x) {
  hc_credits(
    hc = x,
    text = 'Source: Online Labour Index',
    enabled = TRUE,
    href = 'http://ilabour.oii.ox.ac.uk/online-labour-index/',
    position = list(align = "right")
  )
}

custom_ts_selector <- function(x) {
  hc_rangeSelector(hc = x,
                   buttons = list(
                     list(
                       type = 'month',
                       count = 1,
                       text = '1mth'
                     ),
                     list(
                       type = 'month',
                       count = 3,
                       text = '3mth'
                     ),
                     list(
                       type = 'month',
                       count = 6,
                       text = '6mth'
                     ),
                     list(type = 'ytd',
                          text = 'YTD'),
                     list(
                       type = 'year',
                       count = 1,
                       text = '1y'
                     ),
                     list(type = 'all',
                          text = 'All')
                   ))
}

shinyServer(function(input, output, session) {
  population_pyramid_data <-
    eventReactive(c(input$pyramid_categories),
                  {
                    switch(input$pyramid_categories,
                           "country" = {
                             worker_data %>%
                               filter(timestamp == max(timestamp)) %>%
                               group_by(country) %>%
                               mutate(
                                 total.workers = sum(num_workers),
                                 total.projects = sum(num_projects)
                               ) %>%
                               ungroup() %>%
                               select(-occupation, -num_workers, -num_projects, -region) %>%
                               unique() %>%
                               mutate(
                                 total.workers = 100 * {
                                   total.workers / sum(total.workers)
                                 },
                                 total.projects = 100 * {
                                   total.projects / sum(total.projects)
                                 },
                                 workers.plus.projects = total.workers + total.projects
                               ) %>%
                               arrange(desc(workers.plus.projects)) %>%
                               rename(category = country) %>%
                               slice(1:20)
                           },
                           "region" = {
                             worker_data %>%
                               filter(timestamp == max(timestamp)) %>%
                               group_by(region) %>%
                               mutate(
                                 total.workers = sum(num_workers),
                                 total.projects = sum(num_projects)
                               ) %>%
                               ungroup() %>%
                               select(-occupation, -num_workers, -num_projects, -country) %>%
                               unique() %>%
                               mutate(
                                 total.workers = 100 * {
                                   total.workers / sum(total.workers)
                                 },
                                 total.projects = 100 * {
                                   total.projects / sum(total.projects)
                                 },
                                 workers.plus.projects = total.workers + total.projects
                               ) %>%
                               arrange(desc(workers.plus.projects)) %>%
                               rename(category = region)
                           })
                    
                  })
  
  output$population_pyramid_hc <- renderHighchart({
    population_pyramid_data <- population_pyramid_data()
    
    
    highchart() %>%
      hc_add_series(population_pyramid_data,
                    "bar",
                    hcaes(y = total.projects,
                          x = category),
                    name = "Projects") %>%
      hc_add_series(population_pyramid_data,
                    "bar",
                    hcaes(y = -1 * total.workers,
                          x = category),
                    name = "Workers") %>%
      hc_plotOptions(series = list(stacking = "normal"),
                     bar = list(minPointLength = 3)) %>%
      hc_xAxis(
        list(
          categories = population_pyramid_data$category,
          reversed = TRUE,
          opposite = FALSE
        ),
        list(
          opposite = TRUE,
          reversed = TRUE,
          categories = population_pyramid_data$category,
          linkedTo = 0
        )
      ) %>%
      hc_yAxis(labels = list(
        formatter = JS("function () {
                       return Math.abs(this.value) + '%';
  }")
  )) %>%
      hc_tooltip(
        formatter = JS(
          "function () {
          return '<b>' + this.series.name + ' in ' + this.point.category + '</b><br/>' +
          Highcharts.numberFormat(Math.abs(this.point.y), 2);
}"
)
        )
})
  
  
  occupation_barchart_data <-
    eventReactive(c(input$occupation_bar_value),
                  {
                    worker_data %>%
                      filter(timestamp == max(timestamp)) %>%
                      group_by(occupation, region) %>%
                      mutate(total.workers = sum(num_workers)) %>%
                      mutate(total.projects = sum(num_projects)) %>%
                      select(occupation, region, total.workers, total.projects) %>%
                      ungroup() %>%
                      unique()
                  })
  
  output$occupation_barchart_hc <- renderHighchart({
    occupation_barchart_data <- occupation_barchart_data()
    
    stacked_bar_chart(
      data = occupation_barchart_data,
      library = "highcharter",
      categories.column = ~ occupation,
      subcategories.column = ~ region,
      value.column = as.formula(input$occupation_bar_value),
      stacking.type = input$occupation_bar_stackby
    )
    
    
  })
  
  
  output$placeholder_choropleth_details_DT <- renderDataTable({
    
    data_to_chart <- worker_data %>%
      filter(timestamp == max(timestamp)) %>%
      group_by(occupation, region) %>%
      mutate(total.workers = sum(num_workers)) %>%
      mutate(total.projects = sum(num_projects)) %>%
      select(occupation, region, total.projects) %>%
      group_by(occupation) %>%
      unique() %>%
      mutate(calc = 100*{total.projects / sum(total.projects)}) %>%
      group_by(region) %>%
      select(-total.projects) %>%
      filter(calc == max(calc))
    
    data_to_chart
    
  })
  
  occupation_history_data <-
    eventReactive(c(input$occupation_stackedarea_value),
                  {
                    worker_data %>%
                      group_by(timestamp, occupation) %>%
                      mutate(total.workers = sum(num_workers)) %>%
                      mutate(total.projects = sum(num_projects)) %>%
                      select(timestamp, occupation, total.workers, total.projects) %>%
                      ungroup() %>%
                      unique()
                  })
  
  output$occupation_history_hc <- renderHighchart({
    occupation_history_data <- occupation_history_data()
    
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
  
  })