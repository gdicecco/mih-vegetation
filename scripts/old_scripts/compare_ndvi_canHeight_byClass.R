#### Get majority land cover type and proportion land cover for BBS routes

library(tidyverse)
library(ggplot2)
library(cowplot)

nlcd <- read.csv("\\\\Bioark.bio.unc.edu\\HurlbertLab\\DiCecco\\data\\fragmentation_indices_nlcd_simplified.csv")

ndvi_nbcd <- read.csv("data/ndvi_ncbd.csv")

newcode <- data.frame(code = seq(1,9), 
                      legend = c("Open water", "Urban", "Barren", "Forest", "Shrubland", 
                                 "Agricultural", "Grasslands", "Wetlands", "Perennial ice, snow"))

nlcd2001 <- nlcd %>%
  filter(year == 2001) %>%
  left_join(newcode, by = c("class" = "code")) %>%
  group_by(stateroute) %>%
  filter(prop.landscape == max(prop.landscape)) %>%
  dplyr::select(year, stateroute, class, total.area, prop.landscape, legend)

write.csv(nlcd2001, "data/bbs_nlcd2001.csv", row.names = F)

## Max proportion landscape > 0.25
nlcd_25 <- nlcd2001 %>%
  left_join(ndvi_nbcd) %>%
  filter(prop.landscape > 0.25)

ndvi25 <- ggplot(nlcd_25, aes(x = legend, y = ndvi.mean)) + 
  geom_jitter(color = "black", height = 0, width = 0.1, alpha= 0.3) +
  geom_violin(aes(color = legend), alpha = 0.5, fill = NA, cex = 2, draw_quantiles = c(0.5)) + 
  theme(legend.position = "none") +
  labs(x = "Class of max proportion of landscape", y = "Mean NDVI")

nbcd25 <- ggplot(nlcd_25, aes(x = legend, y = nbcd.mean)) + 
  geom_jitter(color = "black", height = 0, width = 0.1, alpha= 0.3) +
  geom_violin(aes(color = legend), alpha = 0.5, fill = NA, cex = 2, draw_quantiles = c(0.5)) + 
  theme(legend.position = "none") +
  labs(x = "Class of max proportion of landscape", y = "Mean canopy height")

p <- plot_grid(ndvi25, nbcd25)

title <- ggdraw() + draw_label("Max proportion of landscape > 0.25", fontface='bold')

plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
ggsave("Figures/ndvi_canHeight_by_class_p25.pdf", units = 'in', height = 8, width = 16)

## Max proportion landscape > 0.5
nlcd_50 <- nlcd2001 %>%
  left_join(ndvi_nbcd) %>%
  filter(prop.landscape > 0.5, legend != "Open water", legend != "Barren") %>%
  group_by(legend) %>%
  mutate(med_ndvi = median(ndvi.mean, na.rm = T))

ndvi50 <- ggplot(nlcd_50, aes(x = fct_reorder(legend, med_ndvi), y = ndvi.mean)) + 
  geom_jitter(color = "black", height = 0, width = 0.1, alpha= 0.3) +
  geom_violin(aes(color = legend), alpha = 0.5, fill = NA, cex = 2, draw_quantiles = c(0.5)) + 
  theme(legend.position = "none") +
  labs(x = "Class of max proportion of landscape", y = "Mean NDVI")

nbcd50 <- ggplot(nlcd_50, aes(x = fct_reorder(legend, med_ndvi), y = nbcd.mean)) + 
  geom_jitter(color = "black", height = 0, width = 0.1, alpha= 0.3) +
  geom_violin(aes(color = legend), alpha = 0.5, fill = NA, cex = 2, draw_quantiles = c(0.5)) + 
  theme(legend.position = "none") +
  labs(x = "Class of max proportion of landscape", y = "Mean canopy height")

p <- plot_grid(ndvi50, nbcd50)

title <- ggdraw() + draw_label("Max proportion of landscape > 0.50", fontface='bold')

grid <- plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
ggsave("Figures/ndvi_canHeight_by_class_p50.pdf", plot = p, units = 'in', height = 6, width = 16)

## Max proportion landscape > 0.75
nlcd_75 <- nlcd2001 %>%
  left_join(ndvi_nbcd) %>%
  filter(prop.landscape > 0.75)

ndvi75 <- ggplot(nlcd_75, aes(x = legend, y = ndvi.mean)) + 
  geom_jitter(color = "black", height = 0, width = 0.1, alpha= 0.3) +
  geom_violin(aes(color = legend), alpha = 0.5, fill = NA, cex = 2, draw_quantiles = c(0.5)) + 
  theme(legend.position = "none") +
  labs(x = "Class of max proportion of landscape", y = "Mean NDVI")

nbcd75 <- ggplot(nlcd_75, aes(x = legend, y = nbcd.mean)) + 
  geom_jitter(color = "black", height = 0, width = 0.1, alpha= 0.3) +
  geom_violin(aes(color = legend), alpha = 0.5, fill = NA, cex = 2, draw_quantiles = c(0.5)) + 
  theme(legend.position = "none") +
  labs(x = "Class of max proportion of landscape", y = "Mean canopy height")

p <- plot_grid(ndvi75, nbcd75)

title <- ggdraw() + draw_label("Max proportion of landscape > 0.75", fontface='bold')

plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
ggsave("Figures/ndvi_canHeight_by_class_p75.pdf", units = 'in', height = 8, width = 16)

