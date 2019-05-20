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

# Get species list - diurnal land birds
species_list <- read.csv("data/species_list.csv", stringsAsFactors = F)

fix_spp <- list(new_species = c("Sage Sparrow" = "Sagebrush Sparrow", 
                                "Western Scrub Jay" = "California Scrub Jay", 
                                "Sharp-tailed Sparrow" = "Nelson's Sparrow", 
                                "Common Crackle" = "Common Grackle", 
                                "Three-toed Woodpecker" = "American Three-toed Woodpecker", 
                                "Yellow-rumped Warbler" = "(unid. Myrtle/Audubon's) Yellow-rumped Warbler", 
                                "Rock Dove" = "Rock Pigeon", "Northern Oriole" = "Baltimore Oriole", 
                                "Plain Titmouse" = "unid. Oak Titmouse / Juniper Titmouse", 
                                "Scrub Jay" = "California Scrub Jay", 
                                "Northern Flicker" = "(unid. Red/Yellow Shafted) Northern Flicker", 
                                "Western Flycatcher" = "unid. Cordilleran / Pacific-slope Flycatcher", 
                                "Solitary Vireo" = "unid. Cassin's Vireo / Blue-headed Vireo", 
                                "Dark-eyed Junco" = "(unid. race) Dark-eyed Junco", 
                                "Brown Towhee" = "California Towhee", 
                                "Rufous-sided Towhee" = "unid. Spotted Towhee / Eastern Towhee"))

new_spp_names <- as.data.frame(fix_spp)
new_spp_names$species <- row.names(new_spp_names)

# Read in BBC data
bbc_censuses <- read.csv("data/bbc_censuses.csv", stringsAsFactors = F)
bbc_counts <- read.csv("data/bbc_counts.csv", stringsAsFactors = F)
bbc_sites <- read.csv("data/bbc_sites.csv", stringsAsFactors = F)

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
         nTerritory = as.numeric(count_repl)*4) %>%
  mutate(new_species = ifelse(species %in% new_spp_names$species, 
                              as.character(filter(new_spp_names, species == species)$new_species), species)) %>%
  left_join(species_list, by = c("new_species" = "english_common_name")) %>%
  filter(!(is.na(aou)))

# Map of census locations
bbc_sf <- bbc_censuses %>%
  left_join(sites_distinct, by = c("siteID", "sitename")) %>%
  dplyr::select(siteID, sitename, latitude, longitude, year, area, richness) %>%
  mutate_at("longitude", .funs = ~{.*-1}) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(us_states)) %>%
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

us_states

us <- tm_shape(us_states) + tm_borders() + tm_fill(col = "gray")
bbc_map <- us + tm_shape(bbc_sf) + 
  tm_dots(col = "nCensus", alpha = 0.5, size = 1, palette = "GnBu", title = "Number of Censuses")

bbc_length <- us + tm_shape(bbc_sf) + 
  tm_dots(col = "lengthCensus", alpha = 0.5, size = 1, palette = "cat", title = "Length of Censuses")

bbc_year <- us + tm_shape(bbc_sf) + 
  tm_dots(col = "censusYear", alpha = 0.5, size = 1, palette = "GnBu", title = "Most recent census")

bbc_panels <- tmap_arrange(bbc_map, bbc_length, bbc_year)
tmap_save(bbc_panels, "Figures/BBC_locations.pdf")

# Get mean breeding season NDVI for each site/year

bbc_sf_transf <- st_transform(bbc_sf, "+proj=utm +zone=42N +datum=WGS84 +units=km")
buffers <- (bbc_sf$area/100)/2
bbc_buffers <- st_buffer(bbc_sf_transf, dist = buffers)
bbc_years <- unique(bbc_sf$censusYear)

gimms_files <- list.files("\\\\BioArk\\HurlbertLab\\GIS\\gimms\\")

gimms_df <- data.frame(file_name = gimms_files[-1], year = as.numeric(substr(gimms_files[-1], 15, 18)))

bbc_ndvi <- data.frame(siteID = c(), year = c(), NDVI = c())

setwd("\\\\BioArk\\HurlbertLab\\GIS\\gimms\\")
for(yr in bbc_years) {
  files <- filter(gimms_df, year == yr)
  
  gimms_jan <- rasterizeGimms(as.character(files$file_name)[1])
  gimms_jul <- rasterizeGimms(as.character(files$file_name)[2])
  
  gimms_breeding <- stack(c(gimms_jan[[9:12]], gimms_jul[[1:2]]))
  
  sites <- filter(bbc_buffers, censusYear == yr)
  
  ndvi <- extract(gimms_breeding, sites, fun = mean, na.rm = T)
  ndvi.means <- rowMeans(ndvi)
  
  bbc_ndvi <- rbind(bbc_ndvi, 
                    data.frame(siteID = sites$siteID, year = yr, NDVI = c(ndvi.means)))
}

write.csv(bbc_ndvi, "data/bbc_sites_ndvi.csv", row.names = F)
bbc_ndvi <- read.csv("data/bbc_sites_ndvi.csv", stringsAsFactors = F)

# Spp Rich vs NDVI - breeders and visitors

sppRich <- bbc %>%
  left_join(bbc_ndvi, by = c("siteID", "year")) %>%
  group_by(siteID, NDVI, status) %>%
  summarize(nSpp = n_distinct(new_species))

ggplot(sppRich, aes(x = NDVI, y = nSpp, col = status)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(y = "S")
ggsave("Figures/spp_rich_ndvi.pdf")

# For each species, population density (count/area - breeding pairs/hectare) vs. NDVI

bbc_popdens <- bbc %>%
  filter(status == "breeder") %>%
  left_join(bbc_ndvi, by = c("siteID", "year")) %>%
  group_by(siteID) %>%
  summarize(NDVI = mean(NDVI),
            CommDens = sum(nTerritory)/mean(as.numeric(area)))

ggplot(bbc_popdens, aes(x = NDVI, y = CommDens)) + geom_point(size = 2) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Mean NDVI", y = "Community territory density")
ggsave("Figures/community_density_vs_NDVI.pdf")

### Rarefaction curves for BBC sites

nindiv <- bbc %>%
  filter(status == "breeder") %>%
  left_join(bbc_ndvi, by = c("siteID", "year")) %>%
  group_by(siteID, NDVI) %>%
  nest() %>%
  mutate(rarefy = map(data, ~{
    df <- .
    rarefaction(df$nTerritory, method = "indiv")
  })) %>%
  dplyr::select(-data) %>%
  unnest() %>%
  group_by(siteID) %>%
  mutate(obsIndiv = row_number())

ggplot(nindiv, aes(x = obsIndiv, y = rarefy, group = factor(siteID), color = NDVI)) +
  geom_line() +
  scale_color_viridis_c()+ 
  labs(x = "Observed number of individuals", y = "E(S)") +
  geom_vline(xintercept = 175, lty = 2)
ggsave("Figures/rarefaction_curves_BBC.pdf")

## E(S) vs. NDVI 

siteArea <- bbc_censuses %>%
  group_by(siteID) %>%
  summarize(area = mean(as.numeric(area), na.rm = T))

raref_ndvi <- nindiv %>%
  filter(obsIndiv == 175) %>%
  left_join(bbc_ndvi) %>%
  group_by(siteID, rarefy) %>%
  summarize(meanNDVI = mean(NDVI, na.rm = T)) %>%
  left_join(siteArea, by = "siteID")

ggplot(raref_ndvi, aes(x = meanNDVI, y = rarefy)) +
  geom_smooth(method = "lm", color = "black", se = F) +
  geom_point() +
  labs(x = "NDVI", y = "E(S)")
ggsave("Figures/estS_ndvi_bbc.pdf")

ggplot(raref_ndvi, aes(x = area, y = rarefy)) + geom_point() + geom_smooth(method = "lm", se = F) +
  labs(x = "Site area (ha)", y = "E(S)")
ggsave("Figures/estS_vs_area.pdf")
