library(tidyverse)
library(emmeans)

test <- as.data.frame(pairs(emmeans(FRic, ~ climate * treatment))) %>% 
  separate(contrast, c("A", "B"), sep = " - ") %>% 
  select(A, B, estimate, p.value)

test2 <- test %>%
  rename(temp = "A") %>%
  rename(A = "B") %>%
  rename(B = "temp")

test <- rbind(test, test2) %>%
  select(A, B, estimate, p.value)

diff <- as.matrix(test %>%
  select(A, B, estimate) %>% 
  pivot_wider(names_from = B, values_from = estimate) %>% 
  replace(is.na(.), 0) %>% 
  column_to_rownames("A"))

p_val <- as.matrix(test %>%
  select(A, B, p.value) %>% 
  pivot_wider(names_from = B, values_from = p.value) %>% 
  replace(is.na(.), 0) %>% 
    column_to_rownames("A"))

ggcorrplot(diff) + 
  scale_fill_gradient2(limits=c(-6, 6))

ggcorrplot(diff, hc.order = TRUE, type = "lower",
           outline.col = "white",
           p.mat = p_val,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           insig = "blank")+
  scale_fill_gradient2(limits=c(-6, 6))




# '%ni%' <- Negate('%in%')
# 
# filter <- test %>% 
#   distinct() %>% 
#   filter(A != "Arid,Open") %>% 
#   filter(B != "Mesic,Total") %>% 
#   filter(!(A == "Arid,Partial" & B != "Arid,Open")) %>% 
#   filter(!(A == "Arid,Total" & B %ni% c("Arid,Open","Arid,Partial"))) %>% 
#   filter(!(A == "Intermediate,Open" & B %ni% c("Arid,Open", "Arid,Partial","Arid,Total"))) %>% 
#   filter(!(A == "Intermediate,Partial" & B %in% c("Intermediate,Total", "Mesic,Open", "Mesic,Partial"))) %>% 
#   filter(!(A == "Intermediate,Total" & B %in% c("Mesic,Open", "Mesic,Partial"))) %>% 
#   filter(!(A == "Mesic,Open" & B == "Mesic,Partial"))
#   
# point <- filter %>% 
#   filter(p.value <= 0.05)
# 
# ggplot(filter)+
#   geom_raster(aes(x = B, y = A, fill = estimate)) + 
#   scale_fill_gradient2(low = "blue", high = "red", na.value="black", name = "" ) +
#   #geom_point(aes(x = A, y = B, size= as.numeric(na)))+
#   #geom_text(aes(x = A, y = B, label = round(p.value, 4))) +
#   geom_point(data = point, aes(x = B, y = A, size = as.numeric(p.value))) + 
#   theme_bw()
#  


###online example###

# # Compute a correlation matrix
# data(mtcars)
# corr <- round(cor(mtcars), 1)
# head(corr[, 1:6])
# 
# class(corr)
# 
# # Compute a matrix of correlation p-values
# p.mat <- cor_pmat(mtcars)
# head(p.mat[, 1:4])
# 
# class(p.mat)
# 
# library(ggcorrplot)
# 
# ?ggcorrplot()
# 
# ggcorrplot(corr, hc.order = TRUE, type = "lower",
#            outline.col = "white",
#            p.mat = p.mat,
#            ggtheme = ggplot2::theme_gray,
#            colors = c("#6D9EC1", "white", "#E46726"),
#            insig = "blank")

