## Functional niche specialization with BBC data

library(ade4)
library(FD)
library(tidyverse)
library(purrr)

theme_set(theme_classic(base_size = 15))

# BBS species list

species_list <- read.csv("data/species_list.csv", stringsAsFactors = F)

# EltonTraits 1.0

birdfunc <- read.table("\\\\BioArk//HurlbertLab//Databases//Elton Traits//BirdFuncDat.txt", sep = "\t", header = T, quote = "\"", stringsAsFactors = F)
birdfunc <- read.table("/Volumes/HurlbertLab/Databases/Elton Traits/BirdFuncDat.txt", sep = "\t", header = T, quote = "\"", stringsAsFactors = F)

# Join BBS and EltonTraits species lists

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

# Get BBC data, match to BBS species list

# Match species common names to BBS species list
fix_spp <- list(new_species = c("Sage Sparrow" = "Sagebrush Sparrow", 
                                "Western Scrub Jay" = "California Scrub Jay", 
                                "Sharp-tailed Sparrow" = "Nelson's Sparrow", 
                                "Common Crackle" = "Common Grackle", 
                                "Three-toed Woodpecker" = "American Three-toed Woodpecker", 
                                "Yellow-rumped Warbler" = "(unid. Myrtle/Audubon's) Yellow-rumped Warbler", 
                                "Rock Dove" = "Rock Pigeon", 
                                "Northern Oriole" = "Baltimore Oriole", 
                                "Plain Titmouse" = "unid. Oak Titmouse / Juniper Titmouse", 
                                "Scrub Jay" = "California Scrub Jay", 
                                "Northern Flicker" = "(unid. Red/Yellow Shafted) Northern Flicker", 
                                "Western Flycatcher" = "unid. Cordilleran / Pacific-slope Flycatcher", 
                                "Solitary Vireo" = "unid. Cassin's Vireo / Blue-headed Vireo", 
                                "Dark-eyed Junco" = "(unid. race) Dark-eyed Junco", 
                                "Brown Towhee" = "California Towhee", 
                                "Rufous-sided Towhee" = "unid. Spotted Towhee / Eastern Towhee"))

new_spp_names <- as.data.frame(fix_spp)
new_spp_names$common_names <- row.names(new_spp_names)

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
  mutate_at(.vars = c("area"), .funs = ~{case_when(area >= 100 ~ area/10,
                                                   TRUE ~ area)}) %>%
  mutate(count_sub = as.numeric(count),
         count_repl = replace_na(count_sub, 0.25),
         nTerritory = as.numeric(count_repl)*4)

new_species <- c()
for(spp in bbc$species) {
  if (spp %in% new_spp_names$common_names) {
    new_species <- c(new_species, as.character(new_spp_names[new_spp_names$common_names == spp, 1]))
  } else {
    new_species <- c(new_species, spp)
  }
}

bbc$new_species <- new_species

bbc <- bbc %>%
  left_join(species_list, by = c("new_species" = "english_common_name")) %>%
  filter(!(is.na(aou)))

# Read in ENV data

bbc_ndvi <- read.csv("data/bbc_sites_ndvi.csv", stringsAsFactors = F)

# Species distance matrix

spp_list <- bbc %>%
  filter(status == "breeder") %>%
  filter(aou %in% bbs_func$aou) %>%
  dplyr::select(aou) %>%
  distinct()

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

# set up BBC data in species x site format

bbc_wide <- bbc %>%
  filter(status == "breeder") %>%
  filter(aou %in% spp_list$aou) %>%
  dplyr::select(siteID, year, nTerritory, aou) %>%
  group_by(siteID, aou) %>%
  summarize(meanAbund = mean(nTerritory)) %>%
  spread(key = aou, value = meanAbund)

bbc_wide[is.na(bbc_wide)] <- 0

# Niche packing, mean functional distance from centroid, FDisp (FD package)
# fdisp(gower, site x species)

bbc_fdisp <- fdisp(bbs_dist, as.matrix(bbc_wide[, -1]))

sppRich <- bbc %>%
  filter(status == "breeder") %>%
  filter(aou %in% spp_list$aou) %>%
  group_by(siteID) %>%
  summarize(nSpp = n_distinct(aou))

ndvi <- bbc_ndvi %>%
  group_by(siteID) %>%
  summarize(meanNDVI = mean(NDVI))

bbc_niche_pack <- bind_cols(bbc_wide, FDis = bbc_fdisp$FDis) %>%
  dplyr::select(siteID, FDis) %>%
  left_join(sppRich) %>%
  left_join(ndvi)

ggplot(bbc_niche_pack, aes(x = meanNDVI, y = FDis)) + geom_point() + geom_smooth(method = "lm")
ggsave("Figures/functional_niche_specialization_bbc.pdf")

# Community specialization, max pairwise functional distance
# At a given site, max pairwise functional distance of gower distances matrix
# Steps would be: get trait distance matrix for species at a site, what is max value in matrix

bbc_niche_special <- bbc %>%
  filter(status == "breeder") %>%
  filter(aou %in% spp_list$aou) %>%
  group_by(siteID, aou) %>%
  summarize(meanAbund = mean(nTerritory)) %>%
  group_by(siteID) %>%
  nest() %>%
  mutate(maxPWD = map_dbl(data, ~{
    df <- .
    spp <- as.character(df$aou)
    
    mat <- bbs_dist_mat[spp, spp]
    
    max(mat)
    
  })) %>%
  dplyr::select(-data) %>%
  left_join(sppRich) %>%
  left_join(ndvi)

ggplot(bbc_niche_special, aes(x = meanNDVI, y = maxPWD)) + geom_point() + geom_smooth(method = "lm")
ggsave("Figures/functional_niche_maxPWD_bbc.pdf")

# Community niche breadth - convex hull

bbc_FD <- dbFD(bbs_dist, as.matrix(bbc_wide[, -1]), m = 12)
# Lower confidence in this dimensionality reduction - 0.68 instead of 0.94

bbc_niche_breadth <- bind_cols(bbc_wide, FRic = bbc_FD$FRic) %>%
  dplyr::select(siteID, FRic) %>%
  left_join(sppRich) %>%
  left_join(ndvi) %>%
  mutate(FRic_scaled = FRic/nSpp)

ggplot(bbc_niche_breadth, aes(x = meanNDVI, y = FRic_scaled)) + geom_point() + geom_smooth(method = "lm")
ggsave("Figures/functional_niche_breadth_bbc.pdf")
