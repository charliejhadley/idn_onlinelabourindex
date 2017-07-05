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
deposit_details <- fs_details(fs_deposit_id)

deposit_details <- unlist(deposit_details$files)
deposit_details <-
  data.frame(split(deposit_details, names(deposit_details)), stringsAsFactors = F)

deposit_details %>%
  filter(grepl("worker_countrydata_", name)) %>%
  select(download_url) %>%
  .[[1]] %>%
  read_csv()



worker_data <- deposit_details %>%
  filter(grepl("worker_countrydata_", name)) %>%
  select(download_url) %>%
  .[[1]] %>%
  read_csv()

deposit_details %>% 
  filter(name == "Countries_continents_regions.txt") %>%
  select(download_url) %>%
  .[[1]] %>%
  read_delim(delim = ";")

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

worker_data <- worker_data %>%
  mutate(
    region = plyr::mapvalues(country, country_continents$Country, country_continents$Continent),
    colour = plyr::mapvalues(
      occupation,
      occupation_colours$occupation,
      occupation_colours$colour
    )
  )

##====== Countries/Continents




