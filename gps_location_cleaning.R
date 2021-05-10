library(tidyverse)
library(sf)

# loading and cleaning gps points from Devyn
courner_points <- sf::read_sf("/Users/User_2/github/Tejon_Functional_Traits/Datasheets/Tejon_Plots.kml")

center_points <- courner_points %>% 
  mutate(Longitude = sf::st_coordinates(.)[,1],
         Latitude = sf::st_coordinates(.)[,2])  %>%
  group_by(Name) %>%
  summarise_all(.funs = mean) %>%
  separate(Name, c("climate", "block", "treatment")) %>%
  mutate(block = ifelse(block == "B1" & climate == "Arid", "A",
                        ifelse(block == "B2" & climate == "Arid", "B",
                               ifelse(block == "B3" & climate == "Arid", "C", block)))) %>%
  mutate(block = ifelse(block == "B1" & climate == "Interm", "D",
                        ifelse(block == "B2" & climate == "Interm", "E",
                               ifelse(block == "B3" & climate == "Interm", "F", block)))) %>%
  mutate(block = ifelse(block == "B1" & climate == "Mesic", "G",
                        ifelse(block == "B2" & climate == "Mesic", "H",
                               ifelse(block == "B3" & climate == "Mesic", "I", block)))) %>%
  mutate(climate = ifelse(climate == "Interm", "Intermeidate", climate)) %>%
  dplyr::select(Latitude, Longitude, climate, block, treatment)

# saving center of each plots
# write_csv(center_points, "/Users/User_2/github/Tejon_Functional_Traits/Datasheets/plot_center_gps_points.csv")

# saving center of each block
center_block <- center_points %>% 
  group_by(block) %>% 
  summarise_all(.funs = mean) %>% 
  select(Latitude, Longitude, block)

# write_csv(center_block, "/Users/User_2/github/Tejon_Functional_Traits/Datasheets/block_center_gps_points.csv")

# saving the center point for each climate treatment
center_climate <- center_points %>% 
  group_by(climate) %>% 
  summarise_all(.funs = mean) %>% 
  select(Latitude, Longitude, climate)

# write_csv(center_climate, "/Users/User_2/github/Tejon_Functional_Traits/Datasheets/climate_center_gps_points.csv")
