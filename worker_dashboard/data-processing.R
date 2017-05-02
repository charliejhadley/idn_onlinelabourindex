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

# fs_deposit_id <- 3761562
# deposit_details <- fs_details(fs_deposit_id)
# 
# deposit_details <- unlist(deposit_details$files)
# deposit_details <- data.frame(split(deposit_details, names(deposit_details)),stringsAsFactors = F)
# 

worker_data <- read_csv("http://linux.oii.ox.ac.uk/~otto.kassi/OLI/worker_countrydata.txt")

region_labels <- read_csv("data/regions.csv")


# 
# 
# region_import <- read_csv(deposit_details[grepl("bcountrydata_",deposit_details$name),"download_url"])
# 
# region_df <- region_import %>%
#   select(country, country_group) %>%
#   unique()
# 
worker_data <- worker_data %>%
  mutate(region = plyr::mapvalues(country, region_labels$country, region_labels$region))






