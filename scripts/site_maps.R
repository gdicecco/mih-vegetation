# Script to create Figure 1 maps #
# have to run BBC script first #
library(tidyverse)
library(ggplot2)
library(cowplot)
library(tmap)
library(sf)
library(rgdal)
library(raster)


us_routes <- read_sf("/Volumes/hurlbertlab/Databases/BBS/GPS_stoplocations/bbsrte_2012_alb/bbsrte_2012_alb.shp")
us <- tm_shape(us_routes) + tm_borders() + tm_fill(col = "light gray")

# Read in BBS
final.counts <- read.csv("data/final_bbs_subset.csv", header = TRUE) 
bbs_plot_rtes <- filter(us_routes, rteno %in% final.counts$stateroute)

# Read in BBC data
bbc_censuses <- read.csv("data/bbc_censuses.csv", stringsAsFactors = F)
bbc_counts <- read.csv("data/bbc_counts.csv", stringsAsFactors = F)
bbc_sites <- read.csv("data/bbc_sites.csv", stringsAsFactors = F)

# Get species list - diurnal land birds
species_list <- read.csv("data/species_list.csv", stringsAsFactors = F)

# Match species common names to BBS species list
fix_spp <- list(new_species = c("Sage Sparrow" = "Sagebrush Sparrow", 
                                "Western Scrub Jay" = "California Scrub Jay", 
                                "Sharp-tailed Sparrow" = "Nelson's Sparrow", 
                                "Common Crackle" = "Common Grackle", 
                                "Three-toed Woodpecker" = "American Three-toed Woodpecker", 
                                "Yellow-rumped Warbler" = "(unid. Myrtle/Audubon's) Yellow-rumped Warbler", 
                                "Rock Dove" = "Rock Pigeon", 
                                "Northern Oriole" = "Baltimore Oriole", 
                                "Plain Titmouse" = "unid. Oak Titmouse / Juniper Titmouse", 
                                "Scrub Jay" = "California Scrub Jay", 
                                "Northern Flicker" = "(unid. Red/Yellow Shafted) Northern Flicker", 
                                "Western Flycatcher" = "unid. Cordilleran / Pacific-slope Flycatcher", 
                                "Solitary Vireo" = "unid. Cassin's Vireo / Blue-headed Vireo", 
                                "Dark-eyed Junco" = "(unid. race) Dark-eyed Junco", 
                                "Brown Towhee" = "California Towhee", 
                                "Rufous-sided Towhee" = "unid. Spotted Towhee / Eastern Towhee"))

new_spp_names <- as.data.frame(fix_spp)
new_spp_names$common_names <- row.names(new_spp_names)

# Most recent Census for each site
site_census <- bbc_censuses %>%
  group_by(siteID) %>%
  filter(year == max(year)) %>%
  dplyr::select(siteID, year)

sites_distinct <- bbc_sites %>%
  dplyr::select(siteID, sitename, latitude, longitude) %>%
  distinct()

# Filter dataset to relevant species, most recent census year
bbc <- bbc_counts %>%
  left_join(bbc_censuses, by = c("siteID", "year")) %>%
  left_join(sites_distinct, by = c("siteID", "sitename")) %>%
  right_join(site_census, by = c("siteID", "year")) %>%
  mutate(count_sub = as.numeric(count),
         count_repl = replace_na(count_sub, 0.25),
         nTerritory = as.numeric(count_repl)*4)

new_species <- c()
for(spp in bbc$species) {
  if (spp %in% new_spp_names$common_names) {
    new_species <- c(new_species, as.character(new_spp_names[new_spp_names$common_names == spp, 1]))
  } else {
    new_species <- c(new_species, spp)
  }
}

bbc$new_species <- new_species

bbc <- bbc %>%
  left_join(species_list, by = c("new_species" = "english_common_name")) %>%
  filter(!(is.na(aou)))

# Map of census locations
bbc_sf <- bbc_censuses %>%
  left_join(sites_distinct, by = c("siteID", "sitename")) %>%
  dplyr::select(siteID, sitename, latitude, longitude, year, area, richness) %>%
  right_join(site_census, by = c("siteID", "year")) %>%
  mutate_at("longitude", .funs = ~{.*-1}) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  group_by(siteID, area) %>%
  summarize(nCensus = length(unique(year)), 
            censusYear = max(year),
            lengthCensus = case_when(min(year) > 1999 & max(year) < 2010 ~ "2000s",
                                     min(year) > 1989 & max(year) < 2000 ~ "1990s",
                                     max(year) < 1990 ~ "1980s",
                                     min(year) < 1990 & max(year) < 2000 ~ "1980s-1990s",
                                     min(year) < 1990 & max(year) > 1999 ~ "1980s-2000s",
                                     min(year) > 1989 & max(year) > 2000 ~ "1990s-2000s"
            ))


box <- c(xmin = -150, xmax = -65, ymin = 15, ymax = 50)

northAM <- read_sf("data/ne_50m_admin_1_states_provinces_lakes.shp") %>%
  st_make_valid()
eNA <- st_crop(northAM, box)
niche_compx_routes <- read_sf("data/niche_complexity_mod_routes.shp")
  
#### Fig 1 map ####
us <- tm_shape(eNA) + tm_borders() + tm_fill(col = "#d9d9d9")
bbc_map <- us + tm_shape(bbc_sf) + 
  tm_dots(col = "nCensus", alpha = 1, size = 1, palette = "GnBu", title = "Number of Censuses")

point_map <- us + tm_shape(bbs_plot_rtes) + tm_lines(lwd = 2, col = "black") + 
  tm_shape(bbc_sf) + tm_dots(col = "springgreen3", size = 0.4)+ 
  tm_add_legend(type = c("line"), labels = c("BBS"),lwd = 2, size = 0.5, col = "black") +
  tm_add_legend(type = c("symbol"), labels = c(" BBC"), shape = 16, col = "springgreen3", size = 0.5) +
  tm_layout(legend.text.size = 1)
tmap_save(point_map, "Figures/Figure2.pdf", height= 4, width = 6, units = "in")

## species richness

theme_set(theme_classic(base_size = 15))

env_bbs_rich <- read.csv("data/env_bbs_rich.csv", header = TRUE)
bbs <- ggplot(env_bbs_rich, aes(x = ndvi.mean, y = spRich)) + 
  geom_point(col = "black", shape=16, size = 2) + 
  geom_smooth(method = "lm", se = FALSE, lwd =1.25) + 
  annotate("text", x = .5, y = 105, label = "BBS", size =6) +
  xlab(" ")+ ylab("Species richness")
# ggsave("Figures/rich_ndvi.pdf", height = 8, width = 12)

summary(lm(spRich ~ ndvi.mean, data = env_bbs_rich))

## Subsample to BBC sample size
bbs_samp_rich <- purrr::map_dfr(c(1:999), ~{
  bbs_sample <- sample_n(env_bbs_rich, 393)
  
  p <- summary(lm(spRich ~ ndvi.mean, data = bbs_sample))$coefficients[2,4]
  slope <- summary(lm(spRich ~ ndvi.mean, data = bbs_sample))$coefficients[2,1]
  
  data.frame(pval = p, slope = slope)
})

boxplot(bbs_samp_rich, main = "NDVI")

## BBC
sppRich <- read.csv("data/env_bbc_rich.csv", header = TRUE)
bbc <- ggplot(sppRich, aes(x = NDVI, y = nSpp)) +
  geom_point(size = 2, col = "black") +
  labs(x = " ", y = " ") +
  geom_smooth(method = "lm", se = FALSE, lwd =1.25) + 
  annotate("text", x = .5, y = 55, label = "BBC", size =6)
# ggsave("Figures/spp_rich_ndvi.pdf")

# summary(lm(nSpp ~ NDVI, data = sppRich))

## landscape diversity


#BBS
bbs_env_het <- read.csv("data/bbs_site_env_heterogeneity.csv")

summary(lm(spRich ~ ndvi.mean, data = bbs_env_het)) # pos sig pred, r2 = 0.44
summary(lm(spRich ~ shannonH, data = bbs_env_het)) # pos sig pred, r2 = 0.26
summary(lm(spRich ~ ndvi.mean + shannonH, data = bbs_env_het)) # pos sig pred, r2 = 0.48

## subsample test
bbs_samp_het <- purrr::map_dfr(c(1:999), ~{
  bbs_sample <- sample_n(bbs_env_het, 393)
  
  p <- summary(lm(spRich ~ shannonH, data = bbs_sample))$coefficients[2,4]
  slope <- summary(lm(spRich ~ shannonH, data = bbs_sample))$coefficients[2,1]
  
  data.frame(pval = p, slope = slope)
})

boxplot(bbs_samp_het, main = "Landscape diversity")

## fig
bbs_land <- ggplot(bbs_env_het, aes(x = ndvi.mean, y = shannonH)) + 
  geom_point(size = 2) + 
  labs(x = "Mean NDVI", y = "Landscape diversity (H')") +
  geom_smooth(method = "lm", se = FALSE, lwd =1.25) 

#BBC
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

bbc_sppRich <- sppRich %>%
  left_join(landcover_edges)

summary(lm(nSpp ~ NDVI, data = bbc_sppRich)) # positive sig pred, r2 = 0.02
summary(lm(nSpp ~ shannonH + NDVI, data = bbc_sppRich)) # positive sig pred, r2 = 0.045
summary(lm(nSpp ~ shannonH + NDVI, data = bbc_sppRich)) # positive sig pred, r2 = 0.039

bbc_plot <- landcover_edges %>%
  left_join(bbc_ndvi, by = "siteID")

bbc_land <- ggplot(bbc_plot, aes(x = meanNDVI, y = shannonH)) + 
  geom_point(size = 2) + 
  labs(x = "Mean NDVI", y = " ") 

## cowplot 
plot_grid(bbs + theme(legend.position="none"),
          bbc + theme(legend.position="none"),
          bbs_land + theme(legend.position = "none"),
          bbc_land + theme(legend.position = "none"),
          nrow = 2,
          align = 'v',
          labels = c("A","B", "C", "D"),
          label_size = 14)
ggsave("Figures/cowplot_sppRich.pdf", units = "in", width = 8.5, height = 7)
