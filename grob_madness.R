library(grid)
library(gtable)


fills <- rep(c("#FFF2CC","#E2F0D9", "#DEEBF7"), 3)

p <- plot(funct_sla_emm, by = NULL, CIs = FALSE, comparisons = TRUE, horizontal = FALSE, colors = "black") +
  facet_grid(. ~ functional_group + climate, scales = "free", space = "free") +
  theme(panel.margin = unit(0, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

g <- ggplot_gtable(ggplot_build(p))

strip_both <- which(grepl('strip-', g$layout$name))
stript <- grep("strip", g$layout$name)

grid_cols <- sort(unique(g$layout[stript,]$l))
t_vals <- rep(sort(unique(g$layout[stript,]$t)), each = length(grid_cols)/3) # top values
l_vals <- rep(grid_cols[seq_along(grid_cols) %% 3 == 1], length = length(t_vals)) # left values
r_vals <- rep(grid_cols[seq_along(grid_cols) %% 3 == 0], length = length(t_vals))
labs   <- levels(as.factor(p$data$functional_group))

for(i in seq_along(labs)) {
  filler <- rectGrob(y = 0.7, height = 0.6, gp = gpar(fill = "white", col = "black"))
  tg <- textGrob(label = labs[i], y = 0.75, gp = gpar(cex = 0.8))
  g <- gtable_add_grob(g, filler, t = t_vals[i], l = l_vals[i], r = r_vals[i], 
                           name = paste0("filler", i))
  g <- gtable_add_grob(g, tg, t = t_vals[i], l = l_vals[i], r = r_vals[i], 
                           name = paste0("textlab", i))
}

grid.newpage()
grid.draw(g)

k <- 1

for (i in strip_both) {
  
  # to remove first row of labels
  g$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$label <- "" # first set of strip labels
  g$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- NA # first set of strip labels
  
  # g$grobs[[i]]$grobs[[2]]$children[[2]]$children[[1]]$label <- "" # second set of strip labels
  # g$grobs[[i]]$grobs[[2]]$children[[1]]$gp$fill <- NA # second set of strip labels
  
  # to change color of second row of strip labe.s
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[2]]$children[[j]]$gp$fill <- fills[k]
  g$grobs[[i]]$grobs[[2]]$children[[j]]$gp$col <- "black"
  
  k <- k+1
  
}

grid.draw(g)

