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
library(leaflet)
library(sf)
library(viridis)
library(rlang)
library(forcats)
# devtools::install_github('bhaskarvk/leaflet.extras')
library(leaflet.extras) ## Needed for background color of leaflet map
loaded_libraries <- Sys.time()

library(oidnChaRts)
# source("oidnChaRts.R")
source("data-processing.R", local = T)

loaded_dataprocessing <- Sys.time()

print(paste("Libraries: ", loaded_libraries - start))
print(paste("Data processing: ", loaded_dataprocessing - start))

iLabour_branding <- function(x) {
  hc_credits(
    hc = x,
    text = 'Source: Online Labour Index',
    enabled = TRUE,
    href = 'http://ilabour.oii.ox.ac.uk/online-labour-index/',
    position = list(align = "right")
  )
}


shinyServer(function(input, output, session) {
  output$occupation_barchart_hc <- renderHighchart({
    category_column <- sym(input$global_trends_group_by)
    
    if (category_column == "region") {
      subcategory_column <- sym("occupation")
    }
    
    if (category_column == "occupation") {
      subcategory_column <- sym("region")
    }
    
    if (category_column == "country") {
      subcategory_column <- sym("occupation")
    }
    
    
    occupation_barchart_data <- worker_data %>%
      filter(timestamp == max(timestamp)) %>%
      group_by(!!category_column, !!subcategory_column) %>%
      mutate(total.workers = sum(num_workers)) %>%
      mutate(total.projects = sum(num_projects)) %>%
      group_by(!!category_column) %>%
      select(!!category_column,!!subcategory_column,
             total.workers,
             total.projects) %>%
      ungroup() %>%
      unique() %>%
      mutate(
        category := !!category_column,
        subcategory := !!subcategory_column,
        percent.workers = 100 * total.workers / sum(total.workers)
      )
    
    if (category_column == "country") {
      top_20_countries <- occupation_barchart_data %>%
        group_by(category) %>%
        summarise(workers.in.category = sum(total.workers)) %>%
        arrange(desc(workers.in.category)) %>%
        slice(1:20) %>%
        select(category) %>%
        .[[1]]
      
      occupation_barchart_data <- occupation_barchart_data %>%
        filter(category %in% top_20_countries)
      
    }
    
    category_order <- occupation_barchart_data %>%
      group_by(!!category_column) %>%
      summarise(workers.in.occ = sum(total.workers)) %>%
      arrange(desc(workers.in.occ)) %>%
      select(!!category_column) %>%
      .[[1]]
    
    
    subcategory_order <- occupation_barchart_data %>%
      group_by(!!subcategory_column) %>%
      summarise(workers.in.occ = sum(total.workers)) %>%
      arrange(desc(workers.in.occ)) %>%
      select(!!subcategory_column) %>%
      .[[1]]
    
    hc <- stacked_bar_chart(
      data = occupation_barchart_data,
      library = "highcharter",
      categories.column = ~ category,
      subcategories.column = ~ subcategory,
      categories.order = category_order,
      subcategories.order = subcategory_order,
      value.column = ~ percent.workers,
      stacking.type = input$occupation_bar_stackby
    ) %>%
      hc_yAxis(labels = list(formatter = JS(
        "function(){
        return this.value + '%';
  }"
)))
    
    if (input$occupation_bar_stackby == "percent") {
      hc %>%
        hc_tooltip(
          formatter = JS(
            "function(){
            var subcat = '';


            $.each(this.points.reverse(),function(i, point){
            
            subcat += '<span style=\U0022' + 'color:' + this.point.series.color + '\U0022>\u25CF</span>' + ' ' + this.point.series.name + ': ' + Highcharts.numberFormat(this.point.percentage, 1) + '% of workers<br/>';
            //console.log(this.point.series.name)
            });
            
            return this.x.name + ' worker distribution:' + '<br/>' +
            subcat;
    }"
        ),
        shared = TRUE
          ) %>%
        iLabour_branding() %>%
        hc_yAxis(max = 100)
      } else {
        if (category_column == "occupation") {
          hc %>%
            hc_tooltip(
              formatter =  JS(
                "function(){
                console.log(this);
                return Highcharts.numberFormat(this.total, 1) + '% of workers are in the ' +
                this.key.name + ' category';
        }"
)
              )
      } else {
        hc %>%
          hc_tooltip(
            formatter =  JS(
              "function(){
              console.log(this);
              return Highcharts.numberFormat(this.total, 1) + '% of workers are in ' +
              this.key.name;
      }"
)
            )
      }
        
        
      }
    
      })
  
  source(file = "tab_worldmap.R", local = TRUE)$value
  
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