### Functional niche measures

library(ade4)
library(funrar)
library(FD)
library(tidyverse)
library(purrr)

theme_set(theme_classic(base_size = 15))

species_list <- read.csv("data/species_list.csv", stringsAsFactors = F)

# EltonTraits 1.0

birdfunc <- read.table("\\\\BioArk//HurlbertLab//Databases//Elton Traits//BirdFuncDat.txt", sep = "\t", header = T, quote = "\"", stringsAsFactors = F)

# BBS species traits with AOUs

binomial_join <- species_list %>%
  filter(!grepl("unid.", english_common_name)) %>%
  dplyr::select(genus, species, english_common_name, aou) %>%
  mutate(binomial = paste(genus, species, sep = " ")) %>%
  left_join(birdfunc, by = c("binomial" = "Scientific"))

common_name_join <- binomial_join %>%
  filter(is.na(Diet.5Cat)) %>%
  dplyr::select(genus, species, english_common_name, aou, binomial) %>%
  left_join(birdfunc, by = c("english_common_name" = "English"))

# Anything that's left is subspecies or Eastern Yellow Wagtail (Asian spp found in AK)

binomial_matches <- binomial_join %>%
  filter(!is.na(Diet.5Cat))

common_name_matches <- binomial_join %>%
  filter(!is.na(Diet.5Cat))

bbs_func <- binomial_matches %>%
  bind_rows(common_name_matches)

# Read in BBS subset

bbs_subs <- read.csv("data/final_bbs_subset.csv", stringsAsFactors = F)

# Read in ENV data

ndvi_nbcd <- read.csv("data/ndvi_ncbd.csv", stringsAsFactors = F)
nlcd2001 <- read.csv("data/bbs_nlcd2001.csv", stringsAsFactors = F)
bbs_env_het <- read.csv("data/bbs_site_env_heterogeneity.csv", stringsAsFactors = F)

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

# bbs_fuzzy_prep <- prep.fuzzy(bbs_fuzzy, c(10, 7), labels = c("diet", "forage"))

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

# Niche packing, mean functional distance from centroid, FDisp (FD package)
# fdisp(gower, site x species)

# Restructure BBS: rows= site, cols = mean abund of species

spp_list <- bbs_subs %>%
  filter(aou %in% bbs_traits$aou) %>%
  dplyr::select(aou) %>%
  distinct()

bbs_wide <- bbs_subs %>%
  filter(aou %in% bbs_traits$aou) %>%
  group_by(stateroute, aou) %>%
  summarize(meanAbund = mean(speciestotal)) %>%
  dplyr::select(stateroute, aou, meanAbund) %>%
  spread(key = aou, value = meanAbund)

bbs_wide[is.na(bbs_wide)] <- 0

bbs_traits_subs <- bbs_traits %>%
  filter(aou %in% spp_list$aou) %>%
  arrange(aou)

row.names(bbs_traits_subs) <- bbs_traits_subs$aou

gow_bbs <- gowdis(bbs_traits_subs[, -1])

bbs_fdisp <- fdisp(gow_bbs, as.matrix(bbs_wide[, -1]))

# How does niche packing change with NDVI?

sppRich <- bbs_subs %>%
  filter(aou %in% bbs_traits$aou) %>%
  group_by(stateroute) %>%
  summarize(nSpp = n_distinct(aou))

bbs_wide$FDis <- bbs_fdisp$FDis

bbs_niche_pack <- bbs_wide %>%
  dplyr::select(stateroute, FDis) %>%
  left_join(sppRich) %>%
  left_join(ndvi_nbcd)

ggplot(bbs_niche_pack, aes(x = ndvi.mean, y = FDis)) + geom_point() + geom_smooth(method = "lm")

# Community specialization, max pairwise functional distance, funrar
# At a given site, max pairwise functional distance of gower distances matrix
# Steps would be: get trait distance matrix for species at a site, what is max value in matrix

bbs_niche_special <- bbs_subs %>%
  filter(aou %in% bbs_traits$aou) %>%
  group_by(stateroute, aou) %>%
  summarize(meanAbund = mean(speciestotal)) %>%
  group_by(stateroute) %>%
  nest() %>%
  mutate(maxPWD = map_dbl(data, ~{
    df <- .
    spp <- df$aou
    
    traits <- bbs_traits_subs %>%
      filter(aou %in% spp)
    
    gow_matrix <- compute_dist_matrix(traits[, -1])
    
    max(gow_matrix)
  })) %>%
  dplyr::select(-data) %>%
  left_join(sppRich) %>%
  left_join(ndvi_nbcd) %>%
  left_join(dplyr::select(bbs_env_het, -ndvi.mean, -spRich))

# How does niche specialization change with NDVI?

ggplot(bbs_niche_special, aes(x = ndvi.mean, y = maxPWD)) + geom_point() + geom_smooth(method = "lm")
ggplot(bbs_niche_special, aes(x = nbcd.mean, y = maxPWD)) + geom_point() + geom_smooth(method = "lm")

summary(lm(maxPWD ~ shannonH + ndvi.var + elev.var, data = bbs_niche_special))

# Use niche specialization and niche packing for null model instead of number of forgaging guilds
