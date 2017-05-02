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
library(rfigshare)
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

# library(oidnChaRts)

source("oidnChaRts.R")
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

move_avg_hc_series <- function(data, window = 28) {
  ma_ts <-
    stats::filter(as.vector(xts(data$count, data$date)), rep(1 / window, window), sides = 1)
  ma_tibble <- tibble(count = as.vector(ma_ts),
                      date = data$date)
  ma_tibble %>%
    filter(!is.na(count))
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

  ## === Landing Tab
  output$landing_xts_highchart <- renderHighchart({
    
    if(is.null(input$landing_rollmean_k)){
      shinyjs::show(id = "loading-content", anim = TRUE, animType = "fade")
    } else {
      shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
    }
    
    
    selected_categories <- "Total"
    
    actual_labour_index <-
      gig_economy_by_occupation[gig_economy_by_occupation$occupation == "Total", ]
    
    ma_data <- move_avg_hc_series(actual_labour_index, window = as.numeric(input$landing_rollmean_k))
    
    # shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
    
    highchart(type = "stock") %>%
      hc_add_series(
        ma_data,
        "line",
        hcaes(x = date, y = count),
        name = "Online Labour Index"
      ) %>%
      hc_tooltip(valueDecimals = 1,  xDateFormat = "%d %b %Y") %>%
      hc_yAxis("opposite" = FALSE,
               title = list("text" = "Online Labour Index")) %>%
      custom_ts_selector %>%
      iLabour_branding
    
    
  })
  
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
    
    if(is.null(input$occupation_rollmean_k)){
      return()
    }
    
    all_occupations <- setdiff(unique(gig_economy_by_occupation$occupation), "Total")
    legend_order <- gig_economy_by_occupation %>%
      filter(occupation %in% all_occupations) %>%
      group_by(occupation) %>%
      mutate(total = sum(count)) %>%
      arrange(desc(total)) %>%
      select(occupation) %>%
      unique() %>%
      unlist(use.names = F)
    
    hc <- highchart(type = "stock")
    invisible(lapply(all_occupations,
                     function(x) {
                       filtered_data <- gig_economy_by_occupation %>%
                         filter(occupation == x) %>%
                         select(date, count)

                       ma_data <- move_avg_hc_series(filtered_data, window = as.numeric(input$occupation_rollmean_k))

                       hc <<- hc %>%
                         hc_add_series(
                           ma_data,
                           "line",
                           hcaes(x = date, y = count),
                           name = x,
                           index = which(legend_order == x) - 1
                         )
                     }))
    
    hc %>%
      hc_tooltip(valueDecimals = 1,  xDateFormat = "%d %b %Y") %>%
      hc_legend(enabled = TRUE, reverse = TRUE) %>%
      hc_yAxis(
        "opposite" = FALSE,
        title = list("text" = "Online Labour Index")
      ) %>%
      custom_ts_selector %>%
      iLabour_branding
    
    
  })
  
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
    
    if(is.null(input$region_rollmean_k)){
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
    
    tallied_boundaries <- gig_economy_by_boundary %>%
      group_by(country_group, timestamp) %>%
      mutate(total = sum(count)) %>%
      distinct(total) %>% # keep ONLY total and groups
      group_by(timestamp) %>%
      rename(date = timestamp) %>%
      mutate(total = total / sum(total))
    
    tallied_occuptation_total <- gig_economy_by_occupation %>%
      filter(occupation == "Total") %>%
      select(date, count) %>%
      filter(date %in% tallied_boundaries$date)
    
    tallied_boundaries <-
      left_join(tallied_occuptation_total, tallied_boundaries) %>%
      mutate(labour.index = count * total)
    
    legend_order <- tallied_boundaries %>%
      ungroup() %>%
      arrange(desc(labour.index)) %>%
      select(country_group) %>%
      unique() %>%
      unlist(use.names = F)
    
    tallied_boundaries
    
    hc <- highchart()
    
    hc <- highchart(type = "stock")
    invisible(lapply(selected_country_groups,
                     function(x) {
                       filtered_data <- tallied_boundaries %>%
                         filter(country_group == x) %>%
                         select(date, labour.index) %>%
                         rename(count = labour.index)
                       
                       
                       ma_data <- move_avg_hc_series(filtered_data, window = as.numeric(input$region_rollmean_k))

                       hc <<- hc %>%
                         hc_add_series(
                           ma_data,
                           "line",
                           hcaes(x = date, y = count),
                           name = x,
                           index = which(legend_order == x) - 1
                         )
                     }))
    
    hc %>%
      hc_tooltip(valueDecimals = 1,  xDateFormat = "%d %b %Y") %>%
      hc_legend(enabled = TRUE, reverse = TRUE) %>%
      hc_yAxis(
        "opposite" = FALSE,
        title = list("text" = "Online Labour Index")
      ) %>%
      custom_ts_selector %>%
      iLabour_branding
    
    
  })
  
  ## ==== Occupation x country
  ## ====
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
      selected = "Market share",
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
    } else
      subcategories_column <- "occupation"
    
    ## Sum by occupation and region
    
    switch (
      categories_column,
      "country" = {
        categories_column_order <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          group_by_(categories_column) %>%
          summarise(total = sum(count)) %>%
          arrange(desc(total)) %>%
          select_(categories_column) %>%
          unlist(use.names = FALSE) %>%
          .[1:20]
        
        prepared_data <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          group_by_(categories_column, subcategories_column) %>%
          summarise(total = sum(count)) %>% {
            foo <- .
            foo$total <- 100 * {
              foo$total / sum(foo$total)
            }
            as_data_frame(foo)
            
          } %>%
          filter(country %in% categories_column_order)
        
        subcategories_column_order <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp) &
                   country %in% categories_column_order) %>%
          group_by_(subcategories_column) %>%
          mutate(mean = mean(count)) %>%
          select(occupation, mean) %>%
          group_by_(subcategories_column) %>%
          arrange(desc(mean)) %>%
          select_(subcategories_column) %>%
          ungroup() %>%
          unique() %>%
          unlist(use.names = F)
        
        hc <- stacked_bar_chart(
          data = prepared_data,
          library = "highcharter",
          categories_column = categories_column,
          categories_order = categories_column_order,
          subcategories_column = subcategories_column,
          subcategories_order = subcategories_column_order,
          value_column = "total",
          stacking_type = input$global_trends_stack_by
        ) %>%
          hc_chart(zoomType = "x",
                   panning = TRUE,
                   panKey = 'shift') %>%
          hc_tooltip(
            formatter = JS(
              "function() {return '<b>'+ this.series.name +'</b>: '+ Highcharts.numberFormat(this.percentage, 0) +' %';}"
            ),
            shared = TRUE
          ) %>% iLabour_branding()
        
        if (input$global_trends_stack_by == "percent") {
          hc %>% hc_yAxis(max = 100)
        } else {
          hc
        }
      },
      "occupation" = {
        prepared_data <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          group_by_(categories_column, subcategories_column) %>%
          summarise(total = sum(count)) %>%
          ungroup() %>%
          mutate(total = 100 * total / sum(total))
        
        subcategories_column_order <- c(
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
        
        categories_column_order <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          group_by_(categories_column, subcategories_column) %>%
          summarise(total = sum(count)) %>%
          arrange(desc(total)) %>%
          select_(categories_column) %>%
          unique() %>%
          unlist(use.names = FALSE)
        
        hc <- stacked_bar_chart(
          data = prepared_data,
          library = "highcharter",
          categories_column = categories_column,
          categories_order = categories_column_order,
          subcategories_column = subcategories_column,
          subcategories_order = subcategories_column_order,
          value_column = "total",
          stacking_type = input$global_trends_stack_by
        ) %>%
          hc_chart(zoomType = "x",
                   panning = TRUE,
                   panKey = 'shift') %>%
          # hc_yAxis(max = 100) %>%
          hc_tooltip(
            formatter = JS(
              "function() {return '<b>'+ this.series.name +'</b>: '+ Highcharts.numberFormat(this.percentage, 0) +' %';}"
            ),
            shared = TRUE
          )  %>% iLabour_branding()
        
        if (input$global_trends_stack_by == "percent") {
          hc %>% hc_yAxis(max = 100)
        } else {
          hc
        }
        
        
        
      },
      "country_group" = {
        prepared_data <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          group_by_(categories_column, subcategories_column) %>%
          summarise(total = sum(count)) %>%
          ungroup() %>%
          # group_by_(categories_column) %>%
          mutate(total = 100 * total / sum(total))
        
        
        subcategories_column_order <- gig_economy_by_boundary %>%
          filter(timestamp == max(timestamp)) %>%
          group_by_(subcategories_column) %>%
          mutate(mean = mean(count)) %>%
          arrange(desc(mean)) %>%
          select_(subcategories_column) %>%
          ungroup() %>%
          unique() %>%
          unlist(use.names = F)
        
        categories_column_order <- c(
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
        
        hc <- stacked_bar_chart(
          data = prepared_data,
          library = "highcharter",
          categories_column = categories_column,
          categories_order = categories_column_order,
          subcategories_column = subcategories_column,
          subcategories_order = subcategories_column_order,
          value_column = "total",
          stacking_type = input$global_trends_stack_by
        ) %>%
          hc_chart(zoomType = "x",
                   panning = TRUE,
                   panKey = 'shift') %>%
          hc_tooltip(
            formatter = JS(
              "function() {console.log(this);
              return '<b>'+ this.series.name +'</b>: '+ Highcharts.numberFormat(this.percentage, 0) +' %';}"
            ),
            shared = TRUE
          ) %>% iLabour_branding()
        
        if (input$global_trends_stack_by == "percent") {
          hc %>% hc_yAxis(max = 100)
        } else {
          hc
        }
      }
    )
    
  })
  
})