library(raster)
library(tidyverse)
library(sf)
library(ggplot2)
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

# loading file
raster = raster("Datasheets/cwd2011apr.asc")

# initial plot
plot(raster)

class(raster)

crs(raster) <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

states_sf <- get_urbn_map("states", sf = TRUE) %>% 
  filter(state_name == "California")

ggplot(states_sf, aes()) +
  geom_sf(fill = "grey", color = "#ffffff")

plot(raster) +
  geom_sf(data = states_sf, fill = "grey", color = "#ffffff")

ecoregions <- read_sf(dsn = "Datasheets/ecoregion", layer = "Climate_eco_v2")
crs(ecoregions)

plot(raster)
plot(ecoregions,
     add = TRUE)

plot(ecoregions)




