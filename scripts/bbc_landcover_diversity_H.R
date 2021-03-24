### BBC landcover diversity (Shannon H)

library(tidyverse)
theme_set(theme_classic(base_size = 15))

### Shannon-diversity for land cover classes
## Use prop.landscape.core to make separate classes for forest edge and forest interior

landcover_nonforest <- read.csv('data/bbc_fragstats.csv', stringsAsFactors = F) %>%
  filter(!(class %in% c(41:43))) %>%
  dplyr::select(siteID, class, prop.landscape) %>%
  mutate(class = as.character(class))

landcover_forest <- read.csv('data/bbc_fragstats.csv', stringsAsFactors = F) %>%
  filter(class %in% c(41:43)) %>%
  dplyr::select(siteID, class, prop.landscape, prop.landscape.core) %>%
  mutate(prop.landscape.edge = prop.landscape - prop.landscape.core) %>%
  dplyr::select(-prop.landscape) %>%
  gather(landscape, prop.landscape, 3:4) %>%
  mutate(land = word(landscape, 3, sep = fixed("."))) %>%
  mutate(class = paste(class, land, sep = "_")) %>%
  dplyr::select(-landscape, -land)

landcover_edges <- bind_rows(landcover_nonforest, landcover_forest) %>%
  group_by(siteID) %>%
  summarize(shannonH = -sum(prop.landscape*log(prop.landscape), na.rm = T))

bbc_ndvi <- read.csv("data/bbc_sites_ndvi.csv", stringsAsFactors = F) %>%
  group_by(siteID) %>%
  summarize(meanNDVI = mean(NDVI))

bbc_plot <- landcover_edges %>%
  left_join(bbc_ndvi, by = "siteID")

ggplot(bbc_plot, aes(x = meanNDVI, y = shannonH)) + geom_point() + geom_smooth(method = "lm", se = F) + 
  labs(x = "NDVI", y = "Landscape diversity (H)", title = "BBC")

summary(lm(shannonH ~ meanNDVI, data = bbc_plot))

bbc_spp_rich <- read.csv("data/env_bbc_rich.csv", stringsAsFactors = F)

bbc_env_rich <- bbc_plot %>%
  left_join(bbc_spp_rich)

summary(lm(nSpp ~ shannonH, data = bbc_env_rich)) # R2 = 0.04
