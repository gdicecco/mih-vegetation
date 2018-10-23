## Plots of abundance vs. NDVI and mean canopy height for different trait groups

library(tidyverse)

# Environmental data
env <- read.csv("data/ndvi_ncbd.csv", stringsAsFactors = F)

# BBS data
routes <- read.csv("/Volumes/hurlbertlab/Databases/BBS/2017/bbs_routes_20170712.csv")
counts <- read.csv("/Volumes/hurlbertlab/Databases/BBS/2017/bbs_counts_20170712.csv")
species <- read.csv("/Volumes/hurlbertlab/Databases/BBS/2017/bbs_species_20170712.csv")
weather <- read.csv("/Volumes/hurlbertlab/Databases/BBS/2017/bbs_weather_20170712.csv")

landbirds <- species %>%
  filter(aou > 2880) %>%
  filter(aou < 3650 | aou > 3810) %>%
  filter(aou < 3900 | aou > 3910) %>%
  filter(aou < 4160 | aou > 4210) %>%
  filter(aou != 7010)

# Observations with RT=1, in BCRs of interest, 1990-present, diurnal land birds
counts.subs <- counts %>%
  filter(stateroute %in% routes.short$stateroute) %>%
  filter(year > 1994, year < 2011) %>%
  filter(aou %in% landbirds$aou)

## Traits

traits <- read.csv("data/spp_traits.csv", stringsAsFactors = F)

### Plots


