## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: otto.kassi@oii.ox.ac.uk
## Data Source: https://dx.doi.org/10.6084/m9.figshare.3761562
## ================================================================================

# library("magrittr)
start <- Sys.time()
library("shiny")
library("rfigshare")
library("lubridate")
library("plotly")
library("highcharter")
library("dygraphs")
library("xts")
library("htmltools")
library("tidyverse")
library("shinyBS")
library("shinyjs")
library("forcats")
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
    if (is.null(input$landing_rollmean_k)) {
      shinyjs::show(id = "loading-content",
                    anim = TRUE,
                    animType = "fade")
    } else {
      shinyjs::hide(id = "loading-content",
                    anim = TRUE,
                    animType = "fade")
    }
    
    
    selected_categories <- "Total"
    
    actual_labour_index <-
      gig_economy_by_occupation[gig_economy_by_occupation$occupation == "Total",]
    
    actual_labour_index <- actual_labour_index %>%
      mutate(moving.average = rollmean(count, k = as.numeric(input$landing_rollmean_k), na.pad = TRUE, align = "right")) ## CORRECT WINDOWING
    
    highchart(type = "stock") %>%
      hc_add_series(data = actual_labour_index,
                    type = "line",
                    hcaes(x = date,
                          y = moving.average),
                    name = "Online Labour Index") %>%
      hc_tooltip(valueDecimals = 1,
                 xDateFormat = "%d %b %Y") %>%
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
    if (is.null(input$occupation_rollmean_k)) {
      return()
    }
    
    index_by_occupation <- gig_economy_by_occupation %>%
      filter(occupation != "Total") %>%
      group_by(occupation) %>%
      arrange(desc(date), occupation) %>%
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
    
    
    highchart(type = "stock") %>%
      hc_add_series(data = index_by_occupation,
                    type = "line",
                    hcaes(x = date,
                          y = moving.average,
                          group = occupation)) %>%
      hc_tooltip(valueDecimals = 1,
                 xDateFormat = "%d %b %Y") %>%
      hc_yAxis("opposite" = FALSE,
               title = list("text" = "Online Labour Index")) %>%
      custom_ts_selector %>%
      iLabour_branding %>%
      hc_legend(enabled = TRUE, reverse = TRUE)
    
    
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
      arrange(desc(date), country_group) %>%
      mutate(moving.average = rollmean(labour.index, k = as.numeric(input$region_rollmean_k), na.pad = TRUE, align = "right")) %>%
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
      hc_add_series(data = index_by_countrygroup,
                    type = "line",
                    hcaes(x = date,
                          y = moving.average,
                          group = country_group)) %>%
      hc_tooltip(valueDecimals = 1,
                 xDateFormat = "%d %b %Y") %>%
      hc_yAxis("opposite" = FALSE,
               title = list("text" = "Online Labour Index")) %>%
      custom_ts_selector %>%
      iLabour_branding %>%
      hc_legend(enabled = TRUE, reverse = TRUE)
    
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
          categories.column = as.formula(paste0("~", categories_column)),
          categories.order = categories_column_order,
          subcategories.column = as.formula(paste0("~", subcategories_column)),
          subcategories.order = subcategories_column_order,
          value.column = as.formula(paste0("~", "total")),
          stacking.type = input$global_trends_stack_by
        ) %>%
          hc_chart(zoomType = "x",
                   panning = TRUE,
                   panKey = 'shift') %>%
          iLabour_branding()
        
        if (input$global_trends_stack_by == "percent") {
          hc %>% hc_yAxis(max = 100) %>%
            hc_tooltip(
              formatter = JS(
                "function(){
                var subcat = '';
                $.each(this.points.reverse(),function(i, point){
                
                subcat += '<span style=\U0022' + 'color:' + this.point.series.color + '\U0022>\u25CF</span>' + ' ' + this.point.series.name + ': ' + Highcharts.numberFormat(this.point.percentage, 0) + '%<br/>';
                //console.log(this.point.series.name)
                });
                
                return this.x.name + ' employer occupational distribution' + '<br/>' +
                subcat;
        }"
),
shared = TRUE
              )
        } else {
          hc %>%
            hc_tooltip(
              formatter = JS(
                "function(){
                var subcat = '';
                $.each(this.points.reverse(),function(i, point){
                
                subcat += '<span style=\U0022' + 'color:' + this.point.series.color + '\U0022>\u25CF</span>' + ' ' + this.point.series.name + ': ' + Highcharts.numberFormat(this.point.stackY, 2) + '<br/>';
                //console.log(this.point.series.name)
                });
                
                return this.x.name + ' employer occupational distribution' + '<br/>' +
                subcat;
        }"
),
shared = TRUE
              )
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
          categories.column = as.formula(paste0("~", categories_column)),
          categories.order = categories_column_order,
          subcategories.column = as.formula(paste0("~", subcategories_column)),
          subcategories.order = subcategories_column_order,
          value.column = as.formula("~total"),
          stacking.type = input$global_trends_stack_by
        ) %>%
          hc_chart(zoomType = "x",
                   panning = TRUE,
                   panKey = 'shift') %>%
         iLabour_branding()
        
        if (input$global_trends_stack_by == "percent") {
          hc %>% hc_yAxis(max = 100) %>%
            hc_tooltip(
              formatter = JS(
                "function(){
                var subcat = '';
                $.each(this.points.reverse(),function(i, point){
                
                subcat += '<span style=\U0022' + 'color:' + this.point.series.color + '\U0022>\u25CF</span>' + ' ' + this.point.series.name + ': ' + Highcharts.numberFormat(this.point.percentage, 0) + '%<br/>';
                //console.log(this.point.series.name)
                });
                
                return this.x.name + ' employer occupational distribution' + '<br/>' +
                subcat;
        }"
              ),
              shared = TRUE
            )
        } else {
          hc %>% 
            hc_tooltip(
              formatter = JS(
                "function(){
                var subcat = '';
                $.each(this.points.reverse(),function(i, point){
                
                subcat += '<span style=\U0022' + 'color:' + this.point.series.color + '\U0022>\u25CF</span>' + ' ' + this.point.series.name + ': ' + Highcharts.numberFormat(this.point.stackY, 2) + '<br/>';
                //console.log(this.point.series.name)
                });
                
                return this.x.name + ' employer occupational distribution' + '<br/>' +
                subcat;
        }"
),
shared = TRUE
              )
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
          categories.column = as.formula(paste0("~", categories_column)),
          categories.order = categories_column_order,
          subcategories.column = as.formula(paste0("~", subcategories_column)),
          subcategories.order = subcategories_column_order,
          value.column = as.formula("~total"),
          stacking.type = input$global_trends_stack_by
        ) %>%
          hc_chart(zoomType = "x",
                   panning = TRUE,
                   panKey = 'shift') %>% 
          iLabour_branding()
        
        if (input$global_trends_stack_by == "percent") {
          hc %>% hc_yAxis(max = 100) %>%
            hc_tooltip(
              formatter = JS(
                "function(){
                var subcat = '';
                $.each(this.points.reverse(),function(i, point){
                
                subcat += '<span style=\U0022' + 'color:' + this.point.series.color + '\U0022>\u25CF</span>' + ' ' + this.point.series.name + ': ' + Highcharts.numberFormat(this.point.percentage, 0) + '%<br/>';
                //console.log(this.point.series.name)
                });
                
                return this.x.name + ' employer occupational distribution' + '<br/>' +
                subcat;
        }"
),
shared = TRUE
              )
  } else {
    hc %>%
      hc_tooltip(
        formatter = JS(
          "function(){
          console.log(this);
console.log(this.points[0].total);
          return Highcharts.numberFormat(this.points[0].total, 1) + '% of workers are in ' +
          this.points[0].key.name;
  }"
),
        shared = TRUE
      )
  }
}
            )
    
    })
  
  })