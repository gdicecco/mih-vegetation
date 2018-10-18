## Plots of abundance vs. NDVI and mean canopy height for different trait groups

library(tidyverse)
library(traits)

# Environmental data
env <- read.csv("ndvi_ncbd.csv", stringsAsFactors = F)

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

## Get number of habitats
#setwd("\\\\BioArk\\hurlbertlab\\DiCecco\\LTER_birdabund_seasonal\\")
setwd("/Volumes/hurlbertlab/DiCecco/data/")

## BirdLife checklist (ID numbers)
checklist <- read.csv("BirdLife_Checklist_V_9.1.csv", header = TRUE, stringsAsFactors = F)

checklist.subs <- checklist %>%
  select(Common_name, Scientific_name, Synonyms, Alt_common_names, SISRecID) %>%
  mutate(genus = word(Scientific_name, 1),
         species = word(Scientific_name, 2)) %>%
  right_join(landbirds, by = c("genus", "species")) %>% 
  replace_na(list(Common_name = "unknown")) %>%
  filter(aou %in% unique(counts.subs$aou), Common_name != "")

# Check which species didn't match up
checklist.unid <- checklist.subs[is.na(checklist.subs$SISRecID), ] #NAs for SISRecID
checklist.nas <- checklist.unid[!grepl("unid.", checklist.unid$english_common_name), ] # Omit the ones that are NA because they are unid.

# Manually entered species (taxonomic changes in spp_taxon_changes.csv)
setwd("C:/Users/gdicecco/Desktop/git/NLCD_fragmentation/traits/")
missingspp <- read.csv("spp_missing_ids.csv", stringsAsFactors = F)

checklist.subs$SISRecID[match(missingspp$aou, checklist.subs$aou)] <- missingspp$SISRecID

# Get habitat data
IUCNids <- na.omit(unique(checklist.subs$SISRecID))
finescale_habitats <- matrix(nrow = 1, ncol = 5) 
colnames(finescale_habitats) <- c("id", "habitat1", "habitat2", "importance", "occurrence")

for(i in 1:length(IUCNids)) {
  id <- IUCNids[i]
  habitat <- birdlife_habitat(id)
  colnames(habitat) <- c("id", "habitat1", "habitat2", "importance", "occurrence")
  finescale_habitats <- rbind(finescale_habitats, habitat)
}

habitats <- finescale_habitats %>%
  filter(occurrence == "breeding" | occurrence == "resident") %>%
  group_by(id) %>%
  summarize(nHabitats1 = length(unique(habitat1)), nHabitats2 = n())

### Plots


