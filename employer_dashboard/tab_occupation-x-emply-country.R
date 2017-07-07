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
        iLabour_branding() %>%
        hc_yAxis(labels = list(formatter = JS(
          "function(){
          return this.value + '%';
    }"
)))
      
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
                return Highcharts.numberFormat(this.points[0].total, 1) + '% of vacancies are posted in ' +
                this.points[0].key.name;
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
    iLabour_branding() %>%
    hc_yAxis(labels = list(formatter = JS(
      "function(){
      return this.value + '%';
}"
)))
  
  if (input$global_trends_stack_by == "percent") {
    hc %>% hc_yAxis(max = 100) %>%
      hc_tooltip(
        formatter = JS(
          "function(){
          var subcat = '';
          $.each(this.points.reverse(),function(i, point){
          
          subcat += '<span style=\U0022' + 'color:' + this.point.series.color + '\U0022>\u25CF</span>' + ' ' + this.point.series.name + ': ' + Highcharts.numberFormat(this.point.percentage, 1) + '%<br/>';
          //console.log(this.point.series.name)
          });
          
          return this.x.name + ' employer country distribution' + '<br/>' +
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
            return Highcharts.numberFormat(this.points[0].total, 1) + '% of vacancies are posted in ' +
            this.points[0].key.name;
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
    iLabour_branding() %>%
    hc_yAxis(labels = list(formatter = JS(
      "function(){
      return this.value + '%';
}"
)))
  
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
            return Highcharts.numberFormat(this.points[0].total, 1) + '% of vacancies are posted in ' +
            this.points[0].key.name;
    }"
),
shared = TRUE
          )
    }
}
          )
  
              })