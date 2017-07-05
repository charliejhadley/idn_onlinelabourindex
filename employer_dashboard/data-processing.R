## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: otto.kassi@oii.ox.ac.uk
## Data Source: https://dx.doi.org/10.6084/m9.figshare.3761562
## ================================================================================

## =========================== Load file ========================================
## ==============================================================================

fs_deposit_id <- 3761562
deposit_details <- fs_details(fs_deposit_id)

deposit_details <- unlist(deposit_details$files)
deposit_details <-
  data.frame(split(deposit_details, names(deposit_details)), stringsAsFactors = F)


occupation_import <- read_csv(deposit_details[grepl("OLIdata_",deposit_details$name),"download_url"])

occupation_import$date <- as.Date(occupation_import$date)
occupation_import$count <- as.numeric(occupation_import$count)
## Make symbol for visualising:
gig_economy_by_occupation <- occupation_import

## Extract only new jobs
gig_economy_by_occupation <- gig_economy_by_occupation %>%
  filter(status == "new")



region_import <- read_csv(deposit_details[grepl("bcountrydata_",deposit_details$name),"download_url"])
region_import$timestamp <- as.Date(region_import$timestamp)
## Make symbol for visualising:
gig_economy_by_boundary <- region_import
