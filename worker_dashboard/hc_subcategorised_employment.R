subcategorised_employment <- function(employment_data, category, subcategory, value, top_n = NA){
  
  categories_sym <- sym(category)
  
  subcategories_sym <- sym(subcategory)
  
  value_sym <- sym(value)
  
  categorised_data <- employment_data %>%
    filter(timestamp == max(timestamp)) %>%
    group_by(!!categories_sym, !!subcategories_sym) %>%
    summarise(total = sum(!!value_sym)) %>%
    ungroup() %>%
    mutate(share = 100 * total / sum(total)) %>%
    arrange(share) %>%
    select(-total)
  
  categorised_data
  
  if(!is.na(top_n)){

    top_n_categories <- employment_data %>%
      filter(timestamp == max(timestamp)) %>%
      group_by(!!categories_sym) %>%
      summarise(total = sum(!!value_sym)) %>%
      arrange(desc(total)) %>%
      select(!! categories_sym) %>%
      .[[1]] %>%
      .[1:top_n]


    categorised_data <- categorised_data %>%
      filter(!!categories_sym %in% top_n_categories)

  }

  categorised_data %>%
    group_by(!!categories_sym) %>%
    spread(!!subcategories_sym, share, fill = 0) %>%
    gather(!!subcategories_sym, share, 2:ncol(.)) %>%
    group_by(!!categories_sym) %>%
    mutate(category.total = sum(share)) %>%
    ungroup() %>%
    arrange(desc(category.total)) %>%
    group_by(!!subcategories_sym) %>%
    mutate(subcategory.total = sum(share)) %>%
    ungroup()
  
}


hc_subcategorised_employment <- function(employment_data, category, subcategory, stacking){
  
  categories_sym <- sym(category)
  
  subcategories_sym <- sym(subcategory)
  
  hc_categorised_employment <- employment_data %>%
    rename(category = !!categories_sym,
           subcategory = !!subcategories_sym) %>%
    hchart(
      type = "bar",
      hcaes(
        x = fct_reorder(category, category.total),
        y = share,
        group = fct_reorder(subcategory, subcategory.total),
        name = subcategory
      )
    ) %>%
    hc_plotOptions(series = list(stacking = stacking)) %>%
    hc_chart(
      zoomType = "x",
      panning = TRUE,
      panKey = "shift"
    ) %>%
    iLabour_branding() %>%
    hc_yAxis(labels = list(formatter = JS(
      "function(){
      return this.value + '%';
}"
    ))) %>%
    hc_legend(reversed = TRUE) %>%
    hc_xAxis(title = list(text = ""))
  
  if (stacking == "percent") {
    hc_categorised_employment %>%
      hc_yAxis(max = 100) %>%
      hc_tooltip(
        formatter = JS(
          "function(){
          var subcat = '';
          $.each(this.points.reverse(),function(i, point){
          
          subcat += '<span style=\U0022' + 'color:' + this.point.series.color + '\U0022>\u25CF</span>' + ' ' + this.point.series.name + ': ' + Highcharts.numberFormat(this.point.percentage, 0) + '%<br/>';
          //console.log(this.point.series.name)
          });
          
          return this.points[0].key + ' employer occupational distribution' + '<br/>' +
          subcat;
  }"
        ),
        shared = TRUE
      )
  } else {
    hc_categorised_employment %>%
      hc_tooltip(
        formatter = JS(
          "function(){
            console.log(this);
            return Highcharts.numberFormat(this.percentage, 1) + '% of vacancies in ' +
            this.key + ' are posted within ' + this.point.subcategory;
            
    }"
        ),
        shared = FALSE
      )
  }
  
}

hc_theme_onlinelabourindex_workers <-
  function(hc,
           employment_data,
           category,
           subcategory,
           value,
           top_n) {
    
    categories_sym <- sym(category)
    subcategories_sym <- sym(subcategory)
    
    categorised_data <- worker_data %>%
      subcategorised_employment(category,
                                subcategory,
                                value,
                                top_n)
    
    if(subcategory == "occupation"){
      
      ordered_colours <- categorised_data  %>%
        arrange(subcategory.total) %>%
        select(!! subcategories_sym) %>%
        unique() %>%
        left_join(occupation_colours)
      
      hc %>%
        hc_colors(ordered_colours$colour)
    } else {
      
      ordered_colours <- categorised_data %>%
        select(!! subcategories_sym, subcategory.total) %>%
        unique() %>%
        left_join(region_colours) %>%
        arrange(subcategory.total)
      
      hc %>%
        hc_colors(ordered_colours$colour)
      
    }
    
  }



