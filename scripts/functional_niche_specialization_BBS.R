### Functional niche measures

library(ade4)
library(funrar)
library(FD)
library(tidyverse)
library(purrr)

species_list <- read.csv("data/species_list.csv", stringsAsFactors = F)

# EltonTraits 1.0

birdfunc <- read.table("\\\\BioArk//HurlbertLab//Databases//Elton Traits//BirdFuncDat.txt", sep = "\t", header = T, quote = "\"", stringsAsFactors = F)

binomial_join <- species_list %>%
  filter(!grepl("unid.", english_common_name)) %>%
  dplyr::select(genus, species, english_common_name, aou) %>%
  mutate(binomial = paste(genus, species, sep = " ")) %>%
  left_join(birdfunc, by = c("binomial" = "Scientific"))

common_name_join <- binomial_join %>%
  filter(is.na(Diet.5Cat)) %>%
  dplyr::select(genus, species, english_common_name, aou, binomial) %>%
  left_join(birdfunc, by = c("english_common_name" = "English"))

# family_species_join <- common_name_join %>%
#   filter(is.na(Diet.5Cat)) %>%
#   mutate(species = word(Scientific, 2, sep = " ")) %>%
#   left_join(birdfunc, by = c("BLFamilyLatin" = "family", "species" = "species"))

# Anything that's left is subspecies or Eastern Yellow Wagtail (Asian spp found in AK)

# BBS species traits with AOUs

# family_spp_matches <- family_species_join %>%
#   filter(!is.na(Diet.5Cat)) %>%
#   mutate(binomial = paste(genus, species, sep = " "))

binomial_matches <- binomial_join %>%
  filter(!is.na(Diet.5Cat))

common_name_matches <- binomial_join %>%
  filter(!is.na(Diet.5Cat))

bbs_func <- binomial_matches %>%
  bind_rows(common_name_matches)

## Functional niche measurement analysis

# Species distance matrix
# Keep diet composition, foraging height, diurnality, body size
bbs_traits <- bbs_func %>%
  dplyr::select(aou, Diet.Inv, Diet.Vend, Diet.Vect, Diet.Vfish, Diet.Vunk, Diet.Scav, Diet.Fruit, Diet.Nect, Diet.Seed, Diet.PlantO,
                ForStrat.watbelowsurf, ForStrat.wataroundsurf, ForStrat.ground, ForStrat.understory, ForStrat.midhigh, ForStrat.canopy,
                ForStrat.aerial, Nocturnal, BodyMass.Value) %>%
  distinct()

row.names(bbs_traits) <- bbs_traits$aou

# Euclidean
bbs_dist <- quasieuclid(as.dist(bbs_traits[, -1]))
pcoa <- dudi.pco(bbs_dist, scannf = F, nf = 3)

# Gower distances
dist_mat <- compute_dist_matrix(bbs_traits[, -1])

# Breadth of niches (diet and foraging strata are fuzzy: proportions of categories)
bbs_fuzzy <- bbs_traits %>%
  dplyr::select(-Nocturnal, -BodyMass.Value, -aou)

bbs_fuzzy_prep <- prep.fuzzy(bbs_fuzzy, c(10, 7), labels = c("diet", "forage"))

fuzzy_niche_breadth <- bbs_traits %>%
  dplyr::select(-Nocturnal, -BodyMass.Value) %>%
  gather(key = "trait", value = "proportion", 2:18) %>%
  filter(proportion > 0) %>%
  mutate(category = word(trait, 1, 1, sep = "[.]")) %>%
  group_by(aou, category) %>%
  summarize(nTraits = n_distinct(trait))

niche_breadth_plot <- fuzzy_niche_breadth %>%
  left_join(species_list) %>%
  spread(key = category, value = nTraits)

ggplot(niche_breadth_plot, aes(x = Diet, y = ForStrat)) + geom_bin2d() + scale_fill_viridis_c()

# Specialization, max pairwise functional distance, funrar
# At a given site, max pairwise functional distance of gower distances matrix
# Steps would be: get trait distance matrix for species at a site, what is max value in matrix

# Niche packing, mean functional distance from centroid, FDisp (FD package)

gow_bbs <- gowdis(bbs_traits[, -1])

# fdisp(gower, site x species)

# How does niche packing change with NDVI?

# How does niche specialization change with NDVI?

# Use niche specialization and niche packing for null model instead of number of forgaging guilds
