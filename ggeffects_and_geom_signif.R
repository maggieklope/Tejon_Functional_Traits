library(tidyverse)
library(palmerpenguins)
library(glmmTMB)
library(ggeffects)
library(emmeans)
library(ggsignif)

# removing NA value from penguin data
penguins <- penguins %>% 
  drop_na()

# GLMM model for two variable in model
penguin_model <- glmmTMB(data = penguins, bill_length_mm ~ species * sex + (1|island), family = gaussian, na.action = na.fail)

predict <- ggpredict(penguin_model, terms = c("sex", "species")) # ggpredict computes the estimated marginal means (predicted values) of a model
test <- as.data.frame(predict) # saves the prediction as a dataframe

# creating the data fame that contains the text annotation used by geom_signif
anno <- as.data.frame(summary(emmeans(penguin_model, specs = pairwise ~ species * sex))$contrasts) %>% # this pulls the differences between groups and the p-value
  filter(p.value <= 0.05) %>% # filtering for only significant values %>% 
  # doing same re-arranging of data
  separate(col = contrast, into = c("contrast_1", "contrast_2"), sep = " - ") %>%  # this splits up first column at the dash%>% 
  separate(col = contrast_1, into = c("species_1", "sex_1"), sep = ",") %>% # splits up the first column by the comma
  separate(col = contrast_2, into = c("species_2", "sex_2"), sep = ",") %>% 
  # %>% # splits up the second comma by the comma
  filter(species_1 == species_2) %>% # filtering where species are the same
  rename(group = species_1, start = sex_1, end = sex_2) %>%
  # creating a new column for labeling of significance
  mutate(label = ifelse(p.value > .05, "*", 
                        ifelse(p.value <= 0.01, "**",
                               ifelse(p.value <= 0.001, "***", "****")))) %>% 
  dplyr::select(group, start, end, label)

# change colors of plot here:
ggeffects_colors <- c("#f46d43", "#fee090", "#74add1")

# plottign the data
ggplot(test, aes(x, predicted, color = group)) +
  geom_point() + # adding the points
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .25, size = .75) + # adding error bar
  scale_color_manual(values = ggeffects_colors) + # chanigng the color or points and bar
  # changes to the theme
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray90"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "gray90"),
    axis.line = element_line(size = 0.5, linetype = "solid",
                             colour = "gray85"),
    strip.background = element_rect(fill = "gray85"),
    strip.text = element_text(colour = 'gray20')
  ) +
  labs(title = "Predicted Penguin Bill Length by Species and Sex" , y = "Predicted Bill Length (mm)", x = "Penguin Species") +
  # adding the significance comparisons
  geom_signif(
    data = anno, # uses the annotation data to add the text for significance levels
    aes(xmin = start, xmax = end, annotations = label, y_position = c(42, 55, 53)), # y-positions changes the height of the line
    color = "black",
    manual = TRUE) +
  facet_grid(~group)+
  theme(legend.position = "none") # removing legend
