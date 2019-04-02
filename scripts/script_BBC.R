### BBC Data: EDA and population density analysis

### Data from Weecology repo bbc-data-rescue

library(tidyverse)
library(ggplot2)
library(cowplot)
library(tmap)
library(sf)
library(spData)

bbc_censuses <- read.csv("data/bbc_censuses.csv", stringsAsFactors = F)
bbc_counts <- read.csv("data/bbc_counts.csv", stringsAsFactors = F)
bbc_sites <- read.csv("data/bbc_sites.csv", stringsAsFactors = F)

sites_distinct <- bbc_sites %>%
  dplyr::select(siteID, sitename, latitude, longitude) %>%
  distinct()

bbc <- bbc_counts %>%
  left_join(bbc_censuses, by = c("siteID", "year")) %>%
  left_join(sites_distinct, by = c("siteID", "sitename")) %>%
  filter(status == "breeder") %>%
  filter(year > 2000)

bbc_sf <- bbc_censuses %>%
  left_join(sites_distinct, by = c("siteID", "sitename")) %>%
  filter(year > 2000) %>% 
  dplyr::select(siteID, sitename, latitude, longitude, year, area, richness) %>%
  mutate_at("longitude", .funs = ~{.*-1}) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(us_states))

us_states

us <- tm_shape(us_states) + tm_borders() + tm_fill(col = "gray")
bbc_map <- us + tm_shape(bbc_sf) + tm_dots(col = "richness", alpha = 0.5, size = 2, palette = "BuGn") +
  tm_layout(main.title = "BBC locations 2003-2009")
tmap_save(bbc_map, "Figures/BBC_locations_2003-2009.pdf")
