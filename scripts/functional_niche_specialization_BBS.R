### Functional niche measures

library(ade4)
library(funrar)
library(tidyverse)
library(purrr)

species_list <- read.csv("data/species_list.csv", stringsAsFactors = F)

BL_checklist <- read.csv("\\\\BioArk//HurlbertLab//DiCecco//data//BirdLife_Checklist_V_9.1.csv", stringsAsFactors = F)

# EltonTraits 1.0

birdfunc <- read.table("\\\\BioArk//HurlbertLab//Databases//Elton Traits//BirdFuncDat.txt", sep = "\t", header = T, quote = "\"", stringsAsFactors = F)

family_species_join <- birdfunc %>%
  mutate(species = word(Scientific, 2, sep = " ")) %>%
  right_join(species_list, by = c("BLFamilyLatin" = "family", "species" = "species"))

binomial_join <- bbs_func %>%
  filter(is.na(Diet.5Cat)) %>%
  filter(!grepl("unid.", english_common_name)) %>%
  dplyr::select(genus, species, english_common_name, aou) %>%
  mutate(binomial = paste(genus, species, sep = " ")) %>%
  left_join(birdfunc, by = c("binomial" = "Scientific"))

common_name_join <- mismatched_spp %>%
  filter(is.na(Diet.5Cat)) %>%
  dplyr::select(genus, species, english_common_name, aou, binomial) %>%
  left_join(birdfunc, by = c("english_common_name" = "English"))
# Anything that's left is subspecies or Eastern Yellow Wagtail (Asian spp found in AK)

# BBS species traits with AOUs

family_spp_matches <- family_species_join %>%
  filter(!is.na(Diet.5Cat)) %>%
  mutate(binomial = paste(genus, species, sep = " "))

binomial_matches <- binomial_join %>%
  filter(!is.na(Diet.5Cat))

common_name_matches <- binomial_join %>%
  filter(!is.na(Diet.5Cat))

bbs_func <- family_spp_matches %>%
  dplyr::select(-french_common_name, -spanish_common_name, -species_id, -sporder) %>%
  bind_rows(binomial_matches) %>%
  bind_rows(common_name_matches)
