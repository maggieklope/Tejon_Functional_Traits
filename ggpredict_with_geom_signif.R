library(emmeans)
library(ggeffects)
library(ggsignif)
library(tidyverse)
library(ggplot2)

# making function that uses ggpredict to plot model results with geom_signif to show differences in significance

graphs <- function(model_name, model_terms, model_formula, plot_title, y_label, x_label, y_positions = 20) {
  # model_name ex: LDMC_CWM
  # model_terms as a list ex: c("treatment", "climate")
  # model_formula ex: "pairwise ~ climate * treatment"
  
  predict <- ggpredict(model_name, terms = model_terms)
  test <- as.data.frame(predict)
  
  # creating annotation data frame that will be used by geom_signif to get around the facet_wrap() problem
  anno <- as.data.frame(summary(emmeans(model_name, specs = model_formula))$contrasts) %>% 
    filter(p.value <= 0.05) %>% 
    separate(col = contrast, into = c("plot_1", "plot_2"), sep = " - ") %>% 
    separate(col = plot_1, into = c("plot_1_climate", "plot_1_treatment"), sep = ",") %>% 
    separate(col = plot_2, into = c("plot_2_climate", "plot_2_treatment"), sep = ",") %>% 
    filter(plot_1_climate == plot_2_climate) %>% 
    rename(group = plot_1_climate, start = plot_1_treatment, end = plot_2_treatment) %>% 
    mutate(label = ifelse(p.value > .05, "*", 
                          ifelse(p.value <= 0.01, "**",
                                 ifelse(p.value <= 0.001, "***", "****")))) %>% 
    dplyr::select(group, start, end, label)
  
  ggplot(test, aes(x, predicted, color = group)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .25, size = .75) +
    scale_color_manual(values = ggeffects_colors) +
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
    labs(title = "Title", y = "unit", x = "Herbivore Treatment") +
    geom_signif(
      data = anno,
      aes(xmin = start, xmax = end, annotations = label, y_position = c(50, 55)),
      color = "black",
      manual = TRUE) +
    facet_grid(~group)
}


# testing
predict <- ggpredict(LA_CWM, terms = c("treatment", "climate"))
test <- as.data.frame(predict)

# anno <- data.frame(
#   group = c("Arid", "Intermediate"),
#   start = c("Open", "Partial"),
#   end = c("Partial", "Total"),
#   y = c(50, 70),
#   label = c("***", "**")
# )
# 
# anno
# 
# ggplot(test, aes(x, predicted, color = group)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .25, size = .75) +
#   scale_color_manual(values = ggeffects_colors) +
#   theme(
#     panel.background = element_rect(fill = "white",
#                                     colour = "white",
#                                     size = 0.5, linetype = "solid"),
#     panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                     colour = "gray90"),
#     panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                     colour = "gray90"),
#     axis.line = element_line(size = 0.5, linetype = "solid",
#                              colour = "gray85"),
#     strip.background = element_rect(fill = "gray85"),
#     strip.text = element_text(colour = 'gray20')
#   ) +
#   labs(title = "Title", y = "unit", x = "Herbivore Treatment") +
#   geom_signif(
#     data = anno,
#     aes(xmin = start, xmax = end, annotations = label, y_position = y),
#     color = "black",
#     manual = TRUE) +
#   facet_grid(~group)

anno <- as.data.frame(summary(emmeans(LA_CWM, specs = pairwise ~ climate * treatment))$contrasts) %>%
  filter(p.value <= 0.05) %>%
  separate(col = contrast, into = c("plot_1", "plot_2"), sep = " - ") %>%
  separate(col = plot_1, into = c("plot_1_climate", "plot_1_treatment"), sep = ",") %>%
  separate(col = plot_2, into = c("plot_2_climate", "plot_2_treatment"), sep = ",") %>%
  filter(plot_1_climate == plot_2_climate) %>%
  rename(group = plot_1_climate, start = plot_1_treatment, end = plot_2_treatment) %>%
  mutate(label = ifelse(p.value > .05, "*",
                        ifelse(p.value <= 0.01, "**",
                               ifelse(p.value <= 0.001, "***", "****")))) %>%
  dplyr::select(group, start, end, label)

anno

ggplot(test, aes(x, predicted, color = group)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .25, size = .75) +
  scale_color_manual(values = ggeffects_colors) +
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
  labs(title = "Title", y = "unit", x = "Herbivore Treatment") +
  geom_signif(
    data = anno,
    aes(xmin = start, xmax = end, annotations = label, y_position = c(50, 55)),
    color = "black",
    manual = TRUE) +
  facet_grid(~group)

  
graphs(model_name = LDMC_CWM,
       model_terms = c("treatment", "climate"),
       model_formula = "pairwise ~ climate * treatment",
       plot_title = "Title",
       y_label = "label",
       x_label = "label",
       y_positions = c(50, 60, 45))

graphs(model_name = LA_CWM, 
       model_terms = c("treatment", "climate"), 
       model_formula = "pairwise ~ climate * treatment", 
       plot_title = "Title", 
       y_label = "label", 
       x_label = "label", 
       y_positions = c(50, 60, 45))


