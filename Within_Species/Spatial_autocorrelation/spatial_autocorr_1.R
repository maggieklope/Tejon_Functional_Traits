# testing spatial auto-correlation
library(ncf)
library(tidyverse)
library(sf)

# 1. plot residuals against groupings?
# model <- glmmTMB(data = cwm_traits_adj, weighted_la ~ climate * treatment + (1|Block), family = gaussian, na.action = na.fail)
model <- glm(data = cwm_traits_adj, weighted_la ~ climate * treatment, family = gaussian, na.action = na.fail)

resid <- simulateResiduals(model)

cwm_traits_adj$climate <- as.factor(cwm_traits_adj$climate)

boxplot(resid$scaledResiduals ~ cwm_traits_adj$climate, ylab = "Scaled Residuals")
kruskal.test(x = resid$scaledResiduals, g = cwm_traits_adj$climate) # p > 0.05 = differences between medians not statistically significant

# 2. get lat/long points
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
  rename(Block = block)

# 3. refit residuals?
refit <- recalculateResiduals(resid, group = as.character(cwm_traits_adj$climate))
refit$long <- aggregate(corner_points$long, list(as.factor(as.character(cwm_traits_adj$Block))), mean)$x
refit$lat <- aggregate(corner_points$lat, list(as.factor(as.character(cwm_traits_adj$Block))), mean)$x

plot(spline.correlog(refit$long, refit$lat, refit$scaledResiduals, 
                     latlon = T, resamp = 100, xmax = 25))
