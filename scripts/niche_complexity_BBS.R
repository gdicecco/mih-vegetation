## Niche complexity measure for BBS routes

library(tidyverse)
library(raster)
library(sf)
library(gimms)

bbs_subset <- read.csv("data/final_bbs_subset.csv", stringsAsFactors = F)

nbcd <- read.csv("data/ndvi_nbcd.csv", stringsAsFactors = F)

newcode <- data.frame(code = seq(1,9), 
                      legend = c("Open water", "Urban", "Barren", "Forest", "Shrubland", 
                                 "Agricultural", "Grasslands", "Wetlands", "Perennial ice, snow"))

forestcode <- data.frame(code = seq(41,43),
                         legend = c("deciduous", "evergreen", "mixed"))

frags <- read.csv("data/fragmentation_indices_nlcd_simplified.csv", stringsAsFactors = F) %>%
  filter(year == 2001) %>%
  left_join(newcode, by = c('class' = 'code'))

### 1 km buffered data
frags_all <- read.csv('data/fragmentation_indices_nlcd_2001.csv', stringsAsFactors = F) %>%
  filter(class == 41 | class == 42 | class == 43) %>%
  left_join(forestcode, by = c("class" = "code"))

troph_guild <- read.csv("data/Troph_guilds.csv", header = TRUE)
bbs_sub1 <- read.csv("data/final_bbs_subset.csv", header = TRUE)
tax_code <- read.csv("data/Bird_Taxonomy.csv", header = TRUE) %>%
  dplyr::select(AOU_OUT, CRC_SCI_NAME) %>%
  unique() %>% na.omit()

tax_code1 = tax_code[-grep("/", tax_code$CRC_SCI_NAME),] 
tax_code2 = tax_code1[-grep("sp.", tax_code1$CRC_SCI_NAME),]


troph_AOU <- left_join(troph_guild, tax_code2, by = c("Species" = "CRC_SCI_NAME"))
troph_AOU$Species = gsub('Dendroica','Setophaga', troph_AOU$Species)

bbs_sub2 <- filter(bbs_sub1, aou %in% tax_code2$AOU_OUT)
bbs_troph <- left_join(bbs_sub2, troph_AOU, by = c("aou" = "AOU_OUT"))

### Shannon-diversity for land cover classes
## Use prop.landscape.core to make separate classes for forest edge and forest interior

landcover_nonforest <- read.csv('data/fragmentation_indices_nlcd_2001.csv', stringsAsFactors = F) %>%
  filter(year == 2001, !(class %in% c(41:43))) %>%
  dplyr::select(stateroute, class, prop.landscape) %>%
  mutate(class = as.character(class))

landcover_forest <- read.csv('data/fragmentation_indices_nlcd_2001.csv', stringsAsFactors = F) %>%
  filter(year == 2001, class %in% c(41:43)) %>%
  dplyr::select(stateroute, class, prop.landscape, prop.landscape.core) %>%
  mutate(prop.landscape.edge = prop.landscape - prop.landscape.core) %>%
  dplyr::select(-prop.landscape) %>%
  gather(landscape, prop.landscape, 3:4) %>%
  mutate(land = word(landscape, 3, sep = fixed("."))) %>%
  mutate(class = paste(class, land, sep = "_")) %>%
  dplyr::select(-landscape, -land)

landcover_edges <- bind_rows(landcover_nonforest, landcover_forest) %>%
  group_by(stateroute) %>%
  summarize(shannonH = -sum(prop.landscape*log(prop.landscape), na.rm = T))

sppRich_H <- bbs_troph %>%
  ungroup() %>%
  dplyr::select(stateroute, spRich) %>%
  distinct() %>%
  left_join(nbcd) %>%
  dplyr::select(stateroute, spRich, ndvi.mean) %>%
  left_join(landcover_edges)

mod_h <- lm(spRich ~ shannonH, data = sppRich_H) # r2 = 0.246
mod_ndvi <- lm(spRich ~ ndvi.mean, data = sppRich_H) # r2 = 0.452
mod_both <- lm(spRich ~ shannonH + ndvi.mean, data = sppRich_H) # r2 = 0.519

  
### Measure of niche complexity for each BBS route

forest_ed <- frags %>%
  group_by(stateroute, year) %>%
  mutate(sum.area = sum(total.area)) %>%
  filter(legend == "Forest") %>%
  group_by(stateroute, year) %>%
  summarize(ED = total.edge/sum.area,
            propForest = prop.landscape,
            meanPatchArea = mean.patch.area) %>%
  mutate(edge = ifelse(ED >= 0.1 & propForest >= 0.1, 1, 0))

forest_type <- frags_all %>%
  dplyr::select(stateroute, prop.landscape, legend) %>%
  spread(legend, prop.landscape)

n_habs <- frags %>%
  group_by(stateroute, year, legend) %>%
  summarize(sum.area = sum(total.area)) %>%
  filter(sum.area > 10000) %>%
  filter(legend != "Open water", legend != "Barren", legend != "Perennial ice, snow") %>%
  group_by(stateroute) %>%
  summarize(n_habs = n_distinct(legend))

n_layers <- nbcd %>%
  group_by(stateroute) %>%
  summarize(nbcd.mean = mean(nbcd.mean),
            nbcd.var = mean(nbcd.var)) %>%
  mutate(can_height = nbcd.mean/10,
         can_var = nbcd.var/10,
         can_sd = sqrt(can_var)) %>%
  mutate(n_layers = case_when(can_height == 0 ~ 0,
                              can_height > 0 & can_height <= 5 ~ 1,
                              can_height >5 & can_height <= 15 ~ 2,
                              can_height > 15 ~ 3))

niche_complex <- n_layers %>%
  left_join(n_habs, by = "stateroute") %>%
  left_join(forest_ed, by = "stateroute") %>%
  left_join(forest_type, by = "stateroute") %>%
  filter(!is.na(n_habs)) %>%
  mutate(n_niche = edge + n_layers + n_habs) %>%
  replace_na(list(ED = 0, propForest = 0, deciduous = 0, evergreen = 0, mixed = 0, edge = 0))

### Niche complexity vs. number foraging guilds, number spp

nSpp <- bbs_troph %>%
  filter(stateroute %in% niche_complex$stateroute) %>%
  group_by(stateroute) %>%
  summarize(nSpp = n_distinct(aou),
            nGuilds = n_distinct(Trophic.guild)) %>%
  left_join(niche_complex, by = "stateroute") %>%
  left_join(nbcd, by = "stateroute")

theme_set(theme_classic())
ggplot(nSpp, aes(x = n_niche, y = nSpp)) +
  geom_point() + labs(x = "Niche complexity", y = "Species")
ggsave("Figures/richness_niche_complexity.pdf")

ggplot(nSpp, aes(x = n_niche, y = nSpp, group = n_niche)) +
  geom_violin(draw_quantiles = c(0.5)) + labs(x = "Niche complexity", y = "Species")

niche_mod <- lm(nSpp ~ n_niche, data = nSpp)
ndvi_mod <- lm(nSpp ~ ndvi.mean, data = nSpp)
both_mod <- lm(nSpp ~ n_niche + ndvi.mean, data = nSpp)

summary(niche_mod)$r.squared
summary(ndvi_mod)$r.squared
summary(both_mod)$r.squared

ggplot(nSpp, aes(x = ndvi.mean, y = n_niche)) + geom_point()

ggplot(nSpp, aes(x = n_niche, y = nGuilds)) + geom_point(alpha = 0.1) +
  labs(x = "Niche complexity", y = "Foraging guilds")
ggsave("Figures/guilds_niche_complexity.pdf")

ggplot(nSpp, aes(x = ndvi.mean, y = nGuilds)) + geom_point(alpha = 0.1)

ggplot(nSpp, aes(x = n_niche, y = nGuilds, group = n_niche)) +
  geom_violin(draw_quantiles = c(0.5)) + labs(x = "Niche complexity", y = "Foraging guilds")


## NDVI and presence of forest edge explain 53% variation in species richness

summary(lm(nSpp ~ ndvi.mean + n_habs + n_layers + edge, data = nSpp)) # add almost 7 species when edge is present

summary(lm(nGuilds ~ ndvi.mean + n_habs + n_layers + edge, data = nSpp)) # nHabs -0.2 foraging guilds per additional habitat
# add one foraging guild for edge presence

## NDVI vs. number of layers predicting species richness and foraging guild richness

layer_mod <- lm(nSpp ~ n_layers, data = nSpp)
layer_ndvi_mod <- lm(nSpp ~ n_layers + ndvi.mean, data = nSpp)

summary(layer_mod)$r.squared
summary(ndvi_mod)$r.squared
summary(layer_ndvi_mod)$r.squared


layer_g_mod <- lm(nGuilds ~ n_layers, data = nSpp)
ndvi_g_mod <- lm(nGuilds ~ ndvi.mean, data = nSpp)
layer_ndvi_g_mod <- lm(nGuilds ~ n_layers + ndvi.mean, data = nSpp)

summary(layer_g_mod)$r.squared
summary(ndvi_g_mod)$r.squared
summary(layer_ndvi_g_mod)$r.squared

### Model selection - models including habitat structure and complexity have higher R2 and lower AIC than NDVI only models

mod <- lm(nSpp ~ can_height + can_var + ED + evergreen + deciduous + mixed + n_habs, data = nSpp)
moda <- lm(nSpp ~ can_height + can_var + ED + deciduous + n_habs, data = nSpp)
modb <- lm(nSpp ~ can_height + can_var + ED + ndvi.mean + n_habs, data = nSpp)

mod2 <- lm(nSpp ~ ndvi.mean, data = nSpp)

AIC(mod, mod2)
summary(mod)$r.squared
summary(mod2)$r.squared

mod3 <- lm(nGuilds ~ can_height + can_var + ED + evergreen + deciduous + mixed + n_habs, data = nSpp)

mod4 <- lm(nGuilds ~ ndvi.mean, data = nSpp)

AIC(mod3, mod4)
summary(mod3)$r.squared
summary(mod4)$r.squared

cor_matrix <- nSpp %>%
  dplyr::select(can_height, can_var, ED, evergreen, deciduous, mixed, n_habs, ndvi.mean) %>%
  as.matrix()

cor(cor_matrix)

### Where routes are

routes <- read.csv("\\\\Bioark.bio.unc.edu\\hurlbertlab\\Databases\\BBS\\2017\\bbs_routes_20170712.csv")
routes$stateroute <- routes$statenum*1000 + routes$route

nSpp_latlon <- nSpp %>%
  left_join(routes, by = c("stateroute")) %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  dplyr::select(stateroute, nSpp, nGuilds)
sf::write_sf(nSpp_latlon, "data/niche_complexity_mod_routes.shp")

plot(nSpp_latlon)

### Env variability model: ShannonH, variance in NDVI, elevational heterogeneity

# 1 km route paths
bbs_route_paths <- read_sf("data/bbsroutes_1km_buffer.shp")

# ShannonH + birds
sppRich_H

# Elevation data 
elev <- raster("\\\\BioArk\\HurlbertLab\\GIS\\DEM\\USA1_msk_alt.grd")
# punch out 1 km buffer paths, variance(elevation)

bbs_sf_transf <- st_transform(bbs_route_paths, st_crs(elev))
bbs_elev <- extract(elev, bbs_sf_transf, fun = var, na.rm = T, df = T)
bbs_elev_df <- cbind(bbs_elev, stateroute = bbs_sf_transf$rteno) %>%
  rename(elev.var = "USA1_msk_alt")

# Get variance in breeding season NDVI for each site/year
# bbs_years <- c(2000:2004)
# 
# gimms_files <- list.files("\\\\BioArk\\HurlbertLab\\GIS\\gimms\\")
# 
# gimms_df <- data.frame(file_name = gimms_files[-1], year = as.numeric(substr(gimms_files[-1], 15, 18)))
# 
# bbs_ndvi <- data.frame(siteID = c(), year = c(), NDVI = c())
# 
# setwd("\\\\BioArk\\HurlbertLab\\GIS\\gimms\\")
# for(yr in bbs_years) {
#   files <- filter(gimms_df, year == yr)
#   
#   gimms_jan <- rasterizeGimms(as.character(files$file_name)[1])
#   gimms_jul <- rasterizeGimms(as.character(files$file_name)[2])
#   
#   gimms_breeding <- stack(c(gimms_jan[[11:12]], gimms_jul[[1]]))
#   
#   bbs_transf <- st_transform(bbs_sf_transf, st_crs(gimms_jan))
#   
#   ndvi <- extract(gimms_breeding, bbs_sf_transf, fun = var, na.rm = T)
#   ndvi.vars <- rowMeans(ndvi)
#   
#   bbs_ndvi <- rbind(bbs_ndvi, 
#                     data.frame(stateroute = bbs_transf$rteno, year = yr, NDVI = c(ndvi.vars)))
# }
# setwd("C:/Users/gdicecco/Desktop/git/mih-vegetation")
# write.csv(bbs_ndvi, "data/bbs_sites_ndvi_var.csv", row.names = F)

bbs_ndvi <- read.csv("data/bbs_sites_ndvi_var.csv", stringsAsFactors = F) %>%
  group_by(stateroute) %>%
  summarize(ndvi.var = mean(NDVI, na.rm = T))

# ENV heterogeneity model

bbs_env_het <- sppRich_H %>%
  left_join(bbs_ndvi) %>%
  left_join(bbs_elev_df)
# write.csv(bbs_env_het, "data/bbs_site_env_heterogeneity.csv", row.names = F)

bbs_env_het <- read.csv("data/bbs_site_env_heterogeneity.csv")

env_het_mod <- lm(spRich ~ shannonH + ndvi.var + elev.var, data = bbs_env_het)
ndvi_mod <- lm(spRich ~ ndvi.mean, data = bbs_env_het)
all_env_mod <- lm(spRich ~ shannonH + ndvi.var + elev.var + ndvi.mean, data = bbs_env_het)

ggplot(bbs_env_het, aes(x = ndvi.mean, y = shannonH)) + 
  geom_point() + geom_smooth(method = "lm", se = F) + 
  theme_classic(base_size = 15) + labs(x = "NDVI", y = "Landscape diversity (H)", title = "BBS")

summary(lm(shannonH ~ ndvi.mean, data = bbs_env_het)) # r2 = .45
