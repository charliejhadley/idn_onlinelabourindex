## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: otto.kassi@oii.ox.ac.uk
## Data Source: https://dx.doi.org/10.6084/m9.figshare.3761562
## ================================================================================

## =========================== Load file (temp location) ========================
## ==============================================================================

fs_deposit_id <- 3761562
deposit_details <- fs_details(article_id = 3761562, mine = FALSE, session = NULL)

deposit_details <- deposit_details$files

worker_data <- deposit_details %>%
  filter(grepl("worker_countrydata_", name)) %>%
  select(download_url) %>%
  .[[1]] %>%
  read_csv()

region_labels <- deposit_details %>%
  filter(name == "Countries_continents_regions.txt") %>%
  select(download_url) %>%
  .[[1]] %>%
  read_delim(delim = ";")

country_continents <- deposit_details %>%
  filter(grepl("Countries_continents", name)) %>%
  select(download_url) %>%
  .[[1]] %>%
  read_delim(delim = ";")

worker_data <- worker_data %>%
  mutate(
    continent = plyr::mapvalues(country, from = country_continents$Country, to = country_continents$Continent)
  )


load("data/world_shapefiles.rdata")

occupation_colours <- read_csv("data/occupation-colours.csv")

region_colours <- tribble(
  ~region, ~colour,
    "Asia", "#434348",
    "Europe", "#90ed7d", 
    "North America", "#f7a35c",
    "Africa", "#7cb5ec",
    "South America", "#f15c80",
    "Oceania", "#8085e9"
)

worker_data <- worker_data %>%
  mutate(
    region = plyr::mapvalues(
      country,
      country_continents$Country,
      country_continents$Continent,
      warn_missing = FALSE
    ),
    colour = plyr::mapvalues(
      occupation,
      occupation_colours$occupation,
      occupation_colours$colour,
      warn_missing = FALSE
    )
  )

##====== Countries/Continents
