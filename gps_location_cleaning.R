library(tidyverse)
library(sf)

# loading and cleaning gps points from Devyn
corner_points <- sf::read_sf("/Users/User_2/github/Tejon_Functional_Traits/Datasheets/Tejon_Plots.kml") %>% 
  mutate(lat = sf::st_coordinates(.)[,1],
         long = sf::st_coordinates(.)[,2]) %>% 
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
  dplyr::select(-Description)

write_csv(corner_points, "/Users/User_2/github/Tejon_Functional_Traits/Datasheets/gps_points.csv")
