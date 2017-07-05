# Obtained maps from http://www.naturalearthdata.com/downloads/50m-cultural-vectors/
download.file(url = "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
              destfile = "data/world-shape-files.zip")
unzip("data/world-shape-files.zip", exdir = "data/world-shape-files")

library("sf")

## Load shapefiles
world_shapefiles <- st_read("data/world-shape-files/")

world_shapefiles <- world_shapefiles %>%
  mutate_if(is.factor, as.character) %>%
  select(name, name_long, type, continent, region_un, subregion, 
         sovereignt,subunit, postal)

save(world_shapefiles, file = "data/world_shapefiles.rdata")

unlink("data/world-shape-files", recursive = TRUE)
file.remove("data/world-shape-files.zip")

