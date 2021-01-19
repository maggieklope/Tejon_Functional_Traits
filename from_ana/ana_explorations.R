library(glmmTMB)

sp_traits_19 <- traits_19 %>%
  mutate(Treatment = ifelse(Treatment == "open", "Open", Treatment)) %>%
  filter(Species %in% c("Bromus diandrus", "Bromus hordeaceus",
                        "Bromus tectorum", 
                        "Galium aparine")) %>%
  filter(!is.na(LA))

sp_traits_19 %>%
  group_by(Climate, Treatment, Species) %>%
  summarise(presence = n()) %>%
  group_by(Species) %>%
  summarise(total = n())

m1 <- glmmTMB(LA ~ Climate * Treatment + (1|Species), 
              data = sp_traits_19)
dredge(m1)

m2 <- glmmTMB(LA ~ Climate + (1|Species), 
              data = sp_traits_19)

summary(m2)

em_clim <- emmeans(m2, ~ "Climate")

pairs(em_clim)
ggplot(sp_traits_19, aes(x = Treatment, y = LA, fill = Climate)) +
  geom_boxplot() +
  scale_y_log10()

ggplot(sp_traits_19, aes(x = Treatment, y = LA)) +
  geom_boxplot() +
  scale_y_log10()

ggplot(sp_traits_19, aes(x = Climate, y = LA)) +
  geom_boxplot() +
  scale_y_log10()

summary(m1)

cwm_traits_adj %>%
  pivot_longer(cols = weighted_la:weighted_n,
               names_to = "traits",
               values_to = "value") %>%
  ggplot(aes(x = climate, y = value, fill = treatment)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~traits, scales = "free")

cwm_traits %>%
  pivot_longer(cols = leaf_area:leaf_n,
               names_to = "traits",
               values_to = "value") %>%
  ggplot(aes(x = climate, y = value, fill = treatment)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~traits, scales = "free")

md %>%
  filter(treatment %in% c("Control", "Cattle Exclosure", "Full Exclosure")) %>%
  mutate(species_name = ifelse(species_name == "medicago polymorpha", "Medicago polymorpha", 
                               species_name)) %>%
  mutate(treatment = factor(treatment, levels = c("Control", "Cattle Exclosure", "Full Exclosure"))) %>%
  filter(species_name %in% c("Acmison sp", "Acmispon wrangelianus", "Avena fatua",
                             "Bromus diandrus", "Bromus hordeaceus",
                             "Bromus madritensis ssp. Ruben", "Bromus tectorum", 
                             "Calandrinia menziesii","Cerastium glomeratum", 
                             "Ericameria nauseosa", "Erodium brachycarpum",
                             "Gayophytum diffusum ssp. Parviflorum",
                             "Erodium cicutarium", "Festuca myuros",
                             "Galium asparine", "Hordeum murinum",
                             "Hypochaeris glabra", "Medicago polymorpha", 
                             "Melica imperfecta", "Plagiobothrys nothofulvus",
                             "Quercus douglasii", "Quercus lobata",
                             "Ranunculus californicus", "Ribes sp.",
                             "Symphoricarpos mollis", "Trifolium microcephalum",
                             "Triteleia ixioides")) %>%
  ggplot(aes(x = climate, y = sla, fill = treatment)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~species_name, scales = "free")
