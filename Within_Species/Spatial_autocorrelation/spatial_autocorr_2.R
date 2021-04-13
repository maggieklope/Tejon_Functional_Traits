# loading packages
library(tidyverse)
library(sf)

# loading data
# block lat/long
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
  mutate(climate = ifelse(climate == "Interm", "Intermediate", climate)) %>% 
  rename(Block = block)

points <- corner_points %>% 
  unite(climate:treatment, col = "plot", sep = "-") %>% 
  dplyr::select(plot, lat, long)

st_geometry(points) <- NULL

# trait data
ss_sla <- traits %>% 
  dplyr::select(climate, new_block, treatment, species, sla) %>%
  filter(species == "Bromus diandrus") %>%
  unite(climate:treatment, col = "plot", sep = "-") %>%
  left_join(points, by = "plot")

# making distance dataframes
point.dist <- dist(cbind(sla_cwm$long, sla_cwm$lat))
sla.dist <- dist(cbind(sla_cwm$weighted_sla))

# running mantel test
mantel.rtest(point.dist,sla.dist, nrepet = 9999)
# p < 0.05 = we reject null hypothosis that the two matrices are unrelated
# R = 0.2100288 suggests that they are positively associated
# smaller differences in sla are seen among pairs of blocks that are close to each other than far from each other
