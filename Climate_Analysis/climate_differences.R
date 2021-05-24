library(DHARMa)
library(ggridges)
library(car)
library(tidyverse)
library(ggridges)
library(car)


# prism 30-year normal non-interpolated
climate_plot <- read_csv("/Users/User_2/github/Tejon_Functional_Traits/Datasheets/PRISM/PRISM_plotcenter_30yr_normal_800m.csv", skip = 10) %>% 
  separate(Name, c("climate", "block", "treatment"), sep = "_" ) %>%
  mutate(climate = ifelse(climate == "A" , "Arid",
                          ifelse(climate == "Int", "Intermediate",
                                 ifelse(climate == "M", "Mesic", climate)))) %>%
  mutate(treatment = ifelse(treatment == "Parti", "Partial", treatment)) %>%
  group_by(climate, block, treatment) %>%
  summarise_all(.funs = mean)

# prism 30-year normal interpolated
climate_plot_int <- read_csv("/Users/User_2/github/Tejon_Functional_Traits/Datasheets/PRISM/PRISM_plotcenter_30yr_normal_800m_interpolated.csv", skip = 10) %>% 
  separate(Name, c("climate", "block", "treatment"), sep = "_" ) %>%
  mutate(climate = ifelse(climate == "A" , "Arid",
                          ifelse(climate == "Int", "Intermediate",
                                 ifelse(climate == "M", "Mesic", climate)))) %>%
  mutate(treatment = ifelse(treatment == "Parti", "Partial", treatment)) %>%
  group_by(climate, block, treatment) %>%
  summarise_all(.funs = mean)

# climate_plot_int <- read_csv("/Users/User_2/github/Tejon_Functional_Traits/Datasheets/PRISM/PRISM_center_of_plot_interpolated.csv", skip = 10) %>%
#   separate(Name, c("climate", "block", "treatment"), sep = "_" ) %>%
#   mutate(climate = ifelse(climate == "A" , "Arid",
#                         ifelse(climate == "Int", "Intermediate",
#                                ifelse(climate == "M", "Mesic", climate)))) %>%
#   mutate(treatment = ifelse(treatment == "Parti", "Partial", treatment)) %>%
#   group_by(climate, block, treatment) %>%
#   summarise_all(.funs = mean)

# precipitation by climate
ggplot(climate_plot_int, aes(x = ppt_mm, y=climate))+
  geom_density_ridges(aes(fill=climate), alpha=0.5)+
  guides(fill=F)

# data is normal
shapiro.test(climate_plot_int[climate_plot_int$climate == "Arid",]$ppt_mm) # normal
shapiro.test(climate_plot_int[climate_plot_int$climate == "Intermediate",]$ppt_mm) # normal
shapiro.test(climate_plot_int[climate_plot_int$climate == "Mesic",]$ppt_mm) # normal

# variances are equal
leveneTest(data = climate_plot_int, ppt_mm ~ climate) # variances are equal

aov_ppt <- aov(data = climate_plot_int, ppt_mm ~ climate)
simulateResiduals(aov_ppt, plot = T) # ok
shapiro.test(residuals(aov_ppt)) # residuals are normal

summary(aov_ppt)

TukeyHSD(aov_ppt) # mesic and intermediate are the same

# mean temperature by climate
ggplot(climate_plot_int, aes(x = tmean_C, y = climate))+
  geom_density_ridges(aes(fill = climate), alpha = 0.5)+
  guides(fill=F)

shapiro.test(climate_plot_int[climate_plot_int$climate == "Arid",]$tmean_C) # normal
shapiro.test(climate_plot_int[climate_plot_int$climate == "Intermediate",]$tmean_C) # normal
shapiro.test(climate_plot_int[climate_plot_int$climate == "Mesic",]$tmean_C) # normal

leveneTest(data = climate_plot_int, tmean_C ~ climate) # variances are equal

aov_tmean <- aov(data = climate_plot_int, tmean_C ~ climate)
simulateResiduals(aov_tmean, plot = T) # plots look good
shapiro.test(residuals(aov_tmean)) # residuals are normal

summary(aov_tmean) # precipitation is significantly different

TukeyHSD(aov_tmean) # mesic and intermediate are the same


# what if I average by block first, and then look at effect of climate?
climate_block_int <- read_csv("/Users/User_2/github/Tejon_Functional_Traits/Datasheets/PRISM/PRISM_center_of_plot_interpolated.csv", skip = 10) %>% 
  separate(Name, c("climate", "block", "treatment"), sep = "_" ) %>% 
  mutate(climate = ifelse(climate == "A" , "Arid",
                          ifelse(climate == "Int", "Intermediate",
                                 ifelse(climate == "M", "Mesic", climate)))) %>% 
  mutate(treatment = ifelse(treatment == "Parti", "Partial", treatment)) %>% 
  group_by(climate, block) %>%
  summarise_all(.funs = mean)

# data is normal
shapiro.test(climate_block_int[climate_block_int$climate == "Arid",]$ppt_mm) # normal
shapiro.test(climate_block_int[climate_block_int$climate == "Intermediate",]$ppt_mm) # normal
shapiro.test(climate_block_int[climate_block_int$climate == "Mesic",]$ppt_mm) # normal

# variances are equal
leveneTest(data = climate_block_int, ppt_mm ~ climate) # variances are equal

aov_ppt <- aov(data = climate_block_int, ppt_mm ~ climate)
simulateResiduals(aov_ppt, plot = T) # plots look good
shapiro.test(residuals(aov_ppt)) # residuals are normal

summary(aov_ppt)

TukeyHSD(aov_ppt) # mesic and intermediate are the same

# what about using 1997-2017 data (its 4km, but interpolated)
climate_plot_int_2 <- read_csv("/Users/User_2/github/Tejon_Functional_Traits/Datasheets/PRISM/PRISM_plot_center_int_4km_1997_2017.csv", skip = 10) %>% 
  separate(Name, c("climate", "block", "treatment"), sep = "_" ) %>% 
  mutate(climate = ifelse(climate == "A" , "Arid",
                          ifelse(climate == "Int", "Intermediate",
                                 ifelse(climate == "M", "Mesic", climate)))) %>% 
  mutate(treatment = ifelse(treatment == "Parti", "Partial", treatment)) %>% 
  group_by(climate, block, treatment) %>% 
  na.omit() %>% # removing blank rows
  summarise_all(.funs = mean)

climate_plot_averages <- climate_plot_int_2%>% 
  group_by(climate) %>% 
  summarise_all(.funs = mean)

write_csv(climate_plot_int_2, "/Users/User_2/github/Tejon_Functional_Traits/Datasheets/plot_climate_data.csv")

# precipitation by climate
ggplot(climate_plot_int_2, aes(x = ppt_mm, y=climate))+
  geom_density_ridges(aes(fill=climate), alpha = 0.5)+
  guides(fill=F)

ggplot(climate_plot_int_2, aes(y = ppt_mm, x = climate))+
  geom_boxplot()

# data is normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Arid",]$ppt_mm) # not normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Intermediate",]$ppt_mm) # normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Mesic",]$ppt_mm) # normal

# variances are equal
leveneTest(data = climate_plot_int_2, ppt_mm ~ climate) # variances are equal

aov_ppt <- aov(data = climate_plot_int_2, ppt_mm ~ climate)
simulateResiduals(aov_ppt, plot = T) # plots look good
shapiro.test(residuals(aov_ppt)) # residuals are normal

summary(aov_ppt)

TukeyHSD(aov_ppt) # all are different from one another


# mean temperature by climate
ggplot(climate_plot_int_2, aes(x = tmean_C, y = climate))+
  geom_density_ridges(aes(fill = climate), alpha = 0.5)+
  guides(fill=F)

ggplot(climate_plot_int_2, aes(y = tmean_C, x = climate))+
  geom_boxplot()

shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Arid",]$tmean_C) # not normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Intermediate",]$tmean_C) # normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Mesic",]$tmean_C) # normal

leveneTest(data = climate_plot_int_2, tmean_C ~ climate) # variances are not equal

aov_tmean <- aov(data = climate_plot_int_2, tmean_C ~ climate)
simulateResiduals(aov_tmean, plot = T) # bad
shapiro.test(residuals(aov_tmean)) # not normal

climate_plot_int_2 <- climate_plot_int_2 %>% 
  mutate(log_tmean = log(tmean_C)) %>% 
  mutate(sqrt_tmean = sqrt(tmean_C)) %>% 
  mutate(neg_sq_tmean = tmean_C^(-2))

# shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Arid",]$log_tmean) # not normal
# shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Intermediate",]$log_tmean) # normal
# shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Mesic",]$log_tmean) # normal
# 
# leveneTest(data = climate_plot_int_2, log_tmean ~ climate) # variances are not equal
# 
# aov_tmean <- aov(data = climate_plot_int_2, log_tmean ~ climate)
# simulateResiduals(aov_tmean, plot = T) # bad
# shapiro.test(residuals(aov_tmean)) # not normal
# 
# shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Arid",]$sqrt_tmean) # not normal
# shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Intermediate",]$sqrt_tmean) # normal
# shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Mesic",]$sqrt_tmean) # normal
# 
# leveneTest(data = climate_plot_int_2, sqrt_tmean ~ climate) # variances are not equal
# 
# aov_tmean <- aov(data = climate_plot_int_2, sqrt_tmean ~ climate)
# simulateResiduals(aov_tmean, plot = T) # bad
# shapiro.test(residuals(aov_tmean)) # not normal

shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Arid",]$neg_sq_tmean) # not normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Intermediate",]$neg_sq_tmean) # normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Mesic",]$neg_sq_tmean) # normal

leveneTest(data = climate_plot_int_2, neg_sq_tmean ~ climate) # variances are equal

aov_tmean <- aov(data = climate_plot_int_2, neg_sq_tmean ~ climate)
simulateResiduals(aov_tmean, plot = T) # ok!
shapiro.test(residuals(aov_tmean)) # normal!

TukeyHSD(aov_tmean) # significantly different!

# max vapor pressure deficit

ggplot(climate_plot_int_2, aes(y = vpdmax_hPa, x = climate))+
  geom_boxplot()

shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Arid",]$vpdmax_hPa) # not normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Intermediate",]$vpdmax_hPa) # normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Mesic",]$vpdmax_hPa) # not normal

leveneTest(data = climate_plot_int_2, vpdmax_hPa ~ climate) # variances are equal

aov_vpd_max <- aov(data = climate_plot_int_2, vpdmax_hPa ~ climate)
simulateResiduals(aov_vpd_max, plot = T) # not good
shapiro.test(residuals(aov_vpd_max)) # not normal

mod <- lm(data = climate_plot_int_2, vpdmax_hPa ~ climate)
boxCox(mod)

climate_plot_int_2 <- climate_plot_int_2 %>% 
  mutate(neg_sq_vpdmax_hPa = vpdmax_hPa^(-2))

shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Arid",]$neg_sq_vpdmax_hPa) # not normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Intermediate",]$neg_sq_vpdmax_hPa) # normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Mesic",]$neg_sq_vpdmax_hPa) # notnormal

leveneTest(data = climate_plot_int_2, neg_sq_vpdmax_hPa ~ climate) # variances are equal

aov_vpd_max <- aov(data = climate_plot_int_2, neg_sq_vpdmax_hPa ~ climate)
simulateResiduals(aov_vpd_max, plot = T) # ok
shapiro.test(residuals(aov_vpd_max)) # normal

TukeyHSD(aov_vpd_max) # arid is different thatn int and mesic, int and mesic area almost different

# min vapor pressure deficit

ggplot(climate_plot_int_2, aes(y = vpdmin_hPa, x = climate))+
  geom_boxplot()

shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Arid",]$vpdmin_hPa) # normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Intermediate",]$vpdmin_hPa) # normal
shapiro.test(climate_plot_int_2[climate_plot_int_2$climate == "Mesic",]$vpdmin_hPa) # not normal

leveneTest(data = climate_plot_int_2, vpdmin_hPa ~ climate) # variances are equal

aov_vpd_min <- aov(data = climate_plot_int_2, vpdmin_hPa ~ climate)
simulateResiduals(aov_vpd_max, plot = T) # ok
shapiro.test(residuals(aov_vpd_min)) # normal

TukeyHSD(aov_vpd_min) # all are significantly different


