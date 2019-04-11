### BBC Data: EDA and population density analysis

### Data from Weecology repo bbc-data-rescue

library(tidyverse)
library(ggplot2)
library(cowplot)
library(tmap)
library(sf)
library(spData)
library(raster)
library(gimms)
library(mobr)
library(broom)

bbc_censuses <- read.csv("data/bbc_censuses.csv", stringsAsFactors = F)
bbc_counts <- read.csv("data/bbc_counts.csv", stringsAsFactors = F)
bbc_sites <- read.csv("data/bbc_sites.csv", stringsAsFactors = F)

sites_distinct <- bbc_sites %>%
  dplyr::select(siteID, sitename, latitude, longitude) %>%
  distinct()

bbc <- bbc_counts %>%
  left_join(bbc_censuses, by = c("siteID", "year")) %>%
  left_join(sites_distinct, by = c("siteID", "sitename")) %>%
  filter(status == "breeder") 

bbc_sf <- bbc_censuses %>%
  left_join(sites_distinct, by = c("siteID", "sitename")) %>%
  dplyr::select(siteID, sitename, latitude, longitude, year, area, richness) %>%
  mutate_at("longitude", .funs = ~{.*-1}) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(us_states))

us_states

us <- tm_shape(us_states) + tm_borders() + tm_fill(col = "gray")
bbc_map <- us + tm_shape(bbc_sf) + tm_dots(col = "richness", alpha = 0.5, size = 2, palette = "BuGn") +
  tm_layout(main.title = "BBC locations 2003-2009")
tmap_save(bbc_map, "Figures/BBC_locations_2003-2009.pdf")

# Get mean breeding season NDVI for each site location, average across 2003-2009

#bbc_sf_transf <- st_transform(bbc_sf, "+proj=utm +zone=42N +datum=WGS84 +units=km")
#buffers <- (bbc_sf$area/100)/2
#bbc_buffers <- st_buffer(bbc_sf_transf, dist = buffers)


#gimms_files <- list.files("\\\\BioArk\\HurlbertLab\\GIS\\gimms\\")

#gimms_df <- data.frame(file_name = gimms_files[-1], year = as.numeric(substr(gimms_files[-1], 15, 18))) %>%
#  filter(year >= 2003, year <= 2009)

#bbc_ndvi <- data.frame(siteID = c(), year = c(), NDVI = c())

#setwd("\\\\BioArk\\HurlbertLab\\GIS\\gimms\\")
#for(yr in c(2003:2009)) {
#  files <- filter(gimms_df, year == yr)
  
#  gimms_jan <- rasterizeGimms(as.character(gimms_df$file_name)[1])
#  gimms_jul <- rasterizeGimms(as.character(gimms_df$file_name)[2])
  
#  gimms_breeding <- stack(c(gimms_jan[[9:12]], gimms_jul[[1:2]]))
  
#  sites <- filter(bbc_buffers, year == yr)
  
#  ndvi <- extract(gimms_breeding, sites, fun = mean, na.rm = T)
#  ndvi.means <- rowMeans(ndvi)
#  
#  bbc_ndvi <- rbind(bbc_ndvi, 
#                    data.frame(siteID = sites$siteID, year = yr, NDVI = c(ndvi.means)))
#}

#write.csv(bbc_ndvi, "data/bbc_sites_ndvi.csv", row.names = F)
bbc_ndvi <- read.csv("data/bbc_sites_ndvi.csv", stringsAsFactors = F)

# For each species, population density (count/area - breeding pairs/hectare) vs. NDVI

nsites_spp <- bbc %>%
  group_by(species) %>%
  nest() %>%
  mutate(nsites = map_dbl(data, ~{
    df <- .
    length(unique(df$siteID))
  })) %>%
  filter(nsites > 5)

bbc_popdens <- bbc %>%
  filter(species %in% nsites_spp$species) %>%
  left_join(bbc_ndvi, by = c("siteID", "year")) %>%
  mutate(popdens = as.numeric(count)/as.numeric(area)) %>% # NAs introduced by counts that are pluses
  group_by(siteID, species) %>%
  summarize(meanNDVI = mean(NDVI, na.rm = T),
            meanPopDens = mean(popdens, na.rm = T)) %>%
  filter(!is.na(meanPopDens), !is.na(meanNDVI)) %>%
  group_by(species) %>%
  nest() %>%
  mutate(pop_lm = map(data, ~{
    df <- .
    mod <- lm(meanPopDens ~ meanNDVI, data = df)
    tidy(mod)
  }),
  nSites = map_dbl(data, ~{nrow(.)})) %>%
  filter(nSites > 5) %>%
  dplyr::select(species, pop_lm, nSites) %>%
  unnest() %>%
  filter(term == "meanNDVI")

bbc_popdens_comm <- bbc %>%
  left_join(bbc_ndvi, by = c("siteID", "year")) %>%
  group_by(siteID, NDVI, year) %>%
  summarize(popdens = sum(as.numeric(count), na.rm = T)/mean(as.numeric(area))) %>% # NAs introduced by counts that are pluses
  group_by(siteID, NDVI) %>%
  summarize(meanPopDens = mean(popdens))

ggplot(bbc_popdens, aes(x = fct_reorder(species, estimate), y = estimate, col = nSites)) +
  geom_point() + 
  geom_errorbar(aes(ymin= estimate - 1.96*std.error, ymax = estimate + 1.96*std.error)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, lty = 2) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Slope estimate: pop. density vs. NDVI", col = "Number of BBC sites")
ggsave("Figures/slope_estimates_popDensity.pdf", units = "in", height = 8, width = 16)

bbc_popdens_all <- bbc %>%
  filter(species %in% nsites_spp$species) %>%
  left_join(bbc_ndvi, by = c("siteID", "year")) %>%
  mutate(popdens = as.numeric(count)/as.numeric(area)) %>% # NAs introduced by counts that are pluses
  group_by(siteID, species) %>%
  summarize(meanNDVI = mean(NDVI, na.rm = T),
            meanPopDens = mean(popdens, na.rm = T)) %>%
  filter(!is.na(meanPopDens), !is.na(meanNDVI))

ggplot(bbc_popdens_all, aes(x =meanNDVI, y = meanPopDens, group = species)) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  labs(x = "NDVI", y = "Population density (breeding pairs/hectare)")
ggsave("Figures/popDensity_NDVI_bbc.pdf")


### Rarefaction curves for BBC sites

nindiv <- bbc %>%
  group_by(siteID, species) %>%
  summarize(meanIndiv = mean(2*as.numeric(count), na.rm = T)) %>%
  na.omit() %>%
  group_by(siteID) %>%
  nest() %>%
  mutate(rarefy = map(data, ~{
    df <- .
    rarefaction(df$meanIndiv, method = "indiv")
  })) %>%
  dplyr::select(-data) %>%
  unnest() %>%
  group_by(siteID) %>%
  mutate(obsIndiv = row_number())

ggplot(nindiv, aes(x = obsIndiv, y = rarefy, group = factor(siteID))) +
  geom_line() +
  labs(x = "Observed number of individuals", y = "E(S)", color = "Site") +
  geom_vline(xintercept = 100, lty = 2)
ggsave("Figures/rarefaction_curves_BBC.pdf")

## E(S) vs. NDVI 

raref_ndvi <- nindiv %>%
  filter(obsIndiv == 80) %>%
  left_join(bbc_ndvi) %>%
  group_by(siteID, rarefy) %>%
  summarize(meanNDVI = mean(NDVI, na.rm = T))

ggplot(raref_ndvi, aes(x = meanNDVI, y = rarefy)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point() +
  labs(x = "NDVI", y = "E(S)")
ggsave("Figures/estS_ndvi_bbc.pdf")
