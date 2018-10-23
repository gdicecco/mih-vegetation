# Note - 'landbirds' is what I called my BBS table and 'missingspp' you can read in from my repo at https://raw.githubusercontent.com/gdicecco/NLCD_fragmentation/master/traits/spp_missing_ids.csv

library(stringr)
library(traits)
## Get number of habitats
setwd("//bioark/HurlbertLab/DiCecco/LTER_birdabund_seasonal")

## BirdLife checklist (ID numbers)
checklist <- read.csv("BirdLife_Checklist_V_9.1.csv", header = TRUE, stringsAsFactors = F)
species <- read.csv("//bioark/HurlbertLab/Databases/BBS/2017/bbs_species_20170712.csv", header = TRUE, stringsAsFactors = FALSE)

landbirds <- species %>%
  filter(aou > 2880) %>%
  filter(aou < 3650 | aou > 3810) %>%
  filter(aou < 3900 | aou > 3910) %>%
  filter(aou < 4160 | aou > 4210) %>%
  filter(aou != 7010)

checklist.subs <- checklist %>%
  select(Common_name, Scientific_name, Synonyms, Alt_common_names, SISRecID) %>%
  mutate(genus = word(Scientific_name, 1),
         species = word(Scientific_name, 2)) %>%
  right_join(landbirds, by = c("genus", "species")) %>% # join with BBS data
  replace_na(list(Common_name = "unknown")) %>%
  filter(Common_name != "")

# Check which species didn't match up
checklist.unid <- checklist.subs[is.na(checklist.subs$SISRecID), ] #NAs for SISRecID
checklist.nas <- checklist.unid[!grepl("unid.", checklist.unid$english_common_name), ] # Omit the ones that are NA because they are unid.

# Manually entered species that don't match between checklists (taxonomic changes in spp_taxon_changes.csv)
missingspp <- read.csv("https://raw.githubusercontent.com/gdicecco/NLCD_fragmentation/master/traits/spp_missing_ids.csv", stringsAsFactors = F)

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
