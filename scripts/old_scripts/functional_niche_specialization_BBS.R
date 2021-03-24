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
birdfunc <- read.table("/Volumes/HurlbertLab/Databases/Elton Traits/BirdFuncDat.txt", sep = "\t", header = T, quote = "\"", stringsAsFactors = F)

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

# EltonTraits summaries

diet_summaries <- bbs_func %>%
  group_by(BLFamilyLatin) %>%
  count(Diet.5Cat)

ggplot(diet_summaries, aes(x = BLFamilyLatin, y = n, fill = Diet.5Cat)) + 
  geom_col(position = "stack") + coord_flip() + scale_fill_viridis_d() + labs(x = "", y = "Species")
ggsave("Figures/diet5cat_summary.pdf", height = 6, width = 8, units = "in")

# Read in BBS subset

bbs_subs <- read.csv("data/final_bbs_subset.csv", stringsAsFactors = F)

# Read in ENV data

ndvi_nbcd <- read.csv("data/ndvi_ncbd.csv", stringsAsFactors = F)
nlcd2001 <- read.csv("data/bbs_nlcd2001.csv", stringsAsFactors = F)
bbs_env_het <- read.csv("data/bbs_site_env_heterogeneity.csv", stringsAsFactors = F)

## Functional niche measurement analysis

# Restructure BBS: rows= site, cols = mean abund of species

spp_list <- bbs_subs %>%
  filter(aou %in% bbs_func$aou) %>%
  dplyr::select(aou) %>%
  distinct()

bbs_wide <- bbs_subs %>%
  filter(aou %in% bbs_func$aou) %>%
  group_by(stateroute, aou) %>%
  summarize(meanAbund = mean(speciestotal)) %>%
  dplyr::select(stateroute, aou, meanAbund) %>%
  spread(key = aou, value = meanAbund)

bbs_wide[is.na(bbs_wide)] <- 0

# Species distance matrix
# Keep diet composition, foraging height, diurnality, body size
bbs_traits <- bbs_func %>%
  filter(aou %in% spp_list$aou) %>%
  arrange(aou) %>%
  dplyr::select(aou, Diet.Inv, Diet.Vend, Diet.Vect, Diet.Vfish, Diet.Vunk, Diet.Scav, Diet.Fruit, Diet.Nect, Diet.Seed, Diet.PlantO,
                ForStrat.watbelowsurf, ForStrat.wataroundsurf, ForStrat.ground, ForStrat.understory, ForStrat.midhigh, ForStrat.canopy,
                ForStrat.aerial, Nocturnal, BodyMass.Value) %>%
  distinct() %>%
  mutate_at(.fun = log10, .vars = "BodyMass.Value")

row.names(bbs_traits) <- bbs_traits$aou

# Gower distances matrix

bbs_fuzzy <- bbs_traits %>%
  dplyr::select(-Nocturnal, -BodyMass.Value, -aou)

bbs_fuzzy_df <- prep.fuzzy(bbs_fuzzy, c(10, 7), labels = c("diet", "forage"))

bbs_nominal <- bbs_traits %>%
  dplyr::select(Nocturnal)

bbs_quant <- bbs_traits %>%
  dplyr::select(BodyMass.Value)

bbs_trait_list <- list(bbs_fuzzy_df, bbs_nominal, bbs_quant)

bbs_ktab <- ktab.list.df(bbs_trait_list)

bbs_dist <- dist.ktab(bbs_ktab, type = c("F", "D", "Q"))
bbs_dist_mat <- as.matrix(bbs_dist)

# Breadth of niches (diet and foraging strata are fuzzy: proportions of categories)

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
ggsave("Figures/fuzzy_traits.pdf")

niche_summaries <- niche_breadth_plot %>%
  group_by(family) %>%
  summarize(nSpp = n_distinct(aou), 
            meanForage = mean(ForStrat),
            sdForage = sd(ForStrat, na.rm = T),
            sdDiet = sd(Diet, na.rm = T),
            meanDiet = mean(Diet))

ggplot(niche_summaries, aes(x = family, y = meanForage, col = nSpp)) + geom_point(size = 2) + 
  geom_errorbar(aes(ymin = meanForage - 1.96*sdForage, ymax = meanForage + 1.96*sdForage), cex = 1) + coord_flip() +
  labs(x = "", y = "ForStrat", col = "Species")
ggsave("Figures/forstrat_families.pdf", height = 6, width = 8, units = "in")

ggplot(niche_summaries, aes(x = family, y = meanDiet, col = nSpp)) + geom_point(size = 2) + 
  geom_errorbar(aes(ymin = meanDiet - 1.96*sdDiet, ymax = meanDiet + 1.96*sdDiet), cex = 1) + coord_flip() +
  labs(x = "", y = "Diet", col = "Species")
ggsave("Figures/diet_families.pdf", height = 6, width = 8, units = "in")


# Niche packing, mean functional distance from centroid, FDisp (FD package)
# fdisp(gower, site x species)

bbs_fdisp <- fdisp(bbs_dist, as.matrix(bbs_wide[, -1]))

# How does niche packing change with NDVI?

sppRich <- bbs_subs %>%
  filter(aou %in% bbs_traits$aou) %>%
  group_by(stateroute) %>%
  summarize(nSpp = n_distinct(aou))

bbs_niche_pack <- bind_cols(bbs_wide, FDis = bbs_fdisp$FDis) %>%
  dplyr::select(stateroute, FDis) %>%
  left_join(sppRich) %>%
  left_join(ndvi_nbcd)

ggplot(bbs_niche_pack, aes(x = ndvi.mean, y = FDis)) + geom_point() + geom_smooth(method = "lm")
ggsave("Figures/functional_niche_specialization.pdf")

# Community specialization, max pairwise functional distance
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
    spp <- as.character(df$aou)
    
    mat <- bbs_dist_mat[spp, spp]
    
    max(mat)
    
  })) %>%
  dplyr::select(-data) %>%
  left_join(sppRich) %>%
  left_join(ndvi_nbcd) %>%
  left_join(dplyr::select(bbs_env_het, -ndvi.mean, -spRich))

# How does niche specialization change with NDVI?

ggplot(bbs_niche_special, aes(x = ndvi.mean, y = maxPWD)) + geom_point() + geom_smooth(method = "lm")
ggsave("Figures/functional_niche_maxPWD.pdf")

ggplot(bbs_niche_special, aes(x = nbcd.mean, y = maxPWD)) + geom_point() + geom_smooth(method = "lm")

summary(lm(maxPWD ~ shannonH + ndvi.var + elev.var, data = bbs_niche_special))
summary(lm(maxPWD ~ ndvi.mean, data = bbs_niche_special))

# Community niche breadth - convex hull

bbs_FD <- dbFD(bbs_dist, as.matrix(bbs_wide[, -1]), m = 8)

bbs_niche_breadth <- bind_cols(bbs_wide, FRic = bbs_FD$FRic) %>%
  dplyr::select(stateroute, FRic) %>%
  left_join(sppRich) %>%
  left_join(ndvi_nbcd) %>%
  mutate(FRic_scaled = FRic/nSpp)

ggplot(bbs_niche_breadth, aes(x = ndvi.mean, y = FRic_scaled)) + geom_point() + geom_smooth(method = "lm")
ggsave("Figures/functional_niche_breadth.pdf")
