test <- as.data.frame(pairs(emmeans(log_n_CWM, ~ climate + treatment))) %>% 
  separate(contrast, c("A", "B"), sep = " - ")

test2 <- test %>% 
  rename(temp = "A") %>% 
  rename(A = "B") %>% 
  rename(B = "temp")

test <- rbind(test, test2)

'%ni%' <- Negate('%in%')

filter <- test %>% 
  distinct() %>% 
  filter(A != "Arid,Open") %>% 
  filter(B != "Mesic,Total") %>% 
  filter(!(A == "Arid,Partial" & B != "Arid,Open")) %>% 
  filter(!(A == "Arid,Total" & B %ni% c("Arid,Open","Arid,Partial"))) %>% 
  filter(!(A == "Intermediate,Open" & B %ni% c("Arid,Open", "Arid,Partial","Arid,Total"))) %>% 
  filter(!(A == "Intermediate,Partial" & B %in% c("Intermediate,Total", "Mesic,Open", "Mesic,Partial"))) %>% 
  filter(!(A == "Intermediate,Total" & B %in% c("Mesic,Open", "Mesic,Partial"))) %>% 
  filter(!(A == "Mesic,Open" & B == "Mesic,Partial"))
  
point <- filter %>% 
  filter(p.value <= 0.05)

ggplot(filter)+
  geom_raster(aes(x = B, y = A, fill = estimate)) + 
  scale_fill_gradient2(low = "blue", high = "red", na.value="black", name = "" ) +
  #geom_point(aes(x = A, y = B, size= as.numeric(na)))+
  #geom_text(aes(x = A, y = B, label = round(p.value, 4))) +
  geom_point(data = point, aes(x = B, y = A, size = as.numeric(p.value))) + 
  theme_bw()
 
