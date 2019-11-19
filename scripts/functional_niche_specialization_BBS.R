### Functional niche measures

library(ade4)
library(funrar)
library(tidyverse)
library(purrr)

species_list <- read.csv("data/species_list.csv", stringsAsFactors = F)

BL_checklist <- read.csv("\\\\BioArk//HurlbertLab//DiCecco//data//BirdLife_Checklist_V_9.1.csv", stringsAsFactors = F)

# EltonTraits 1.0

birdfunc <- read.table("\\\\BioArk//HurlbertLab//Databases//Elton Traits//BirdFuncDat.txt", sep = "\t", header = T, quote = "\"", stringsAsFactors = F)

bbs_func <- species_list %>%
  filter(!grepl("unid", english_common_name)) %>%
  mutate(binomial = paste(genus, species, sep = " ")) %>%
  left_join(birdfunc, by = c("binomial" =  "Scientific"))

mismatched_spp <- bbs_func %>%
  filter(is.na(Diet.5Cat))

## Match common names with old BirdLife Checklist (V3) to consolidate mismatches from genera; tricolored vs. tricoloured blackbird

BL_spp <- species_list %>%
  mutate(binomial = paste(genus, species, sep = " ")) %>%
  filter(!grepl("unid", english_common_name)) %>%
  left_join(BL_checklist, by = c("binomial" = "Scientific_name"))

find_synonyms <- function(species) {
  BL_checklist %>%
  filter(str_detect(species, Synonyms))
}

possibly_find_synonyms <- possibly(find_synonyms, NA)


