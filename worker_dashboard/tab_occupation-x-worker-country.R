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
  
  if(subcategory_column == "occupation"){
    the_colours <- occupation_colours %>%
      filter(occupation %in% category_order) %>%
      select(colour) %>%
      .[[1]]
  }
  
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
  
  
  if(subcategory_column == "occupation"){
    
    alphaetised_subcategory <- occupation_colours %>%
      filter(!is.na(occupation)) %>%
      select(occupation) %>%
      .[[1]]
    
    the_colours <- occupation_colours %>%
      filter(occupation %in% subcategory_order) %>%
      select(colour) %>%
      .[[1]]
    
    hc <- hc %>%
      hc_colors(the_colours[order(alphaetised_subcategory)])
  }
  
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