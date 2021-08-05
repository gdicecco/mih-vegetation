## MODIS sensitivity

library(tidyverse)
library(cowplot)
library(lubridate)

theme_set(theme_classic(base_size = 15))

## MODIS NDVI/EVI

bbs_ndvi <- read_csv("data/bbs_modis_ndvi.csv") %>%
  mutate(year = year(calendar_date)) %>%
  filter(value > 0) %>%
  mutate(ndvi = value/10000) %>%
  group_by(site) %>%
  summarize(mean_ndvi = mean(ndvi))
  
bbs_evi <- read_csv("data/bbs_modis_evi.csv") %>%
  mutate(year = year(calendar_date)) %>%
  filter(value > 0) %>%
  mutate(evi = value/10000) %>%
  group_by(site) %>%
  summarize(mean_evi = mean(evi))

bbc_ndvi <- read_csv("data/bbc_modis_ndvi.csv") %>%
  mutate(year = year(calendar_date)) %>%
  filter(value > 0) %>%
  mutate(ndvi = value/10000) %>%
  group_by(site, year) %>%
  summarize(mean_ndvi = mean(ndvi))

bbc_evi <- read_csv("data/bbc_modis_evi.csv") %>%
  mutate(year = year(calendar_date)) %>%
  filter(value > 0) %>%
  mutate(evi = value/10000) %>%
  group_by(site, year) %>%
  summarize(mean_evi = mean(evi))

bbs_modis <- bbs_evi %>%
  left_join(bbs_ndvi) 

bbc_modis <- bbc_evi %>%
  left_join(bbc_ndvi)

## Gimms ndvi BBS and BBC

bbc_gimms <- read_csv('data/bbc_sites_ndvi.csv')

bbs_gimms <- read_csv("data/gimms_ndvi_bbs_data.csv") %>%
  filter(month %in% c("may", "jun", "jul"), year %in% c(2000:2004)) %>%
  group_by(site_id) %>%
  summarize(ndvi = mean(ndvi))

## 1:1 correlation of data - ndvi vs ndvi, ndvi vs evi

bbc_all <- bbc_modis %>%
  left_join(bbc_gimms, by = c("site" = "siteID", "year")) 

length(unique(bbc_all$site))
length(unique(bbc_all$site[!is.na(bbc_all$NDVI)])) # only 42 sites in post 2000 time period

plot(bbc_all$mean_ndvi, bbc_all$NDVI)
cor(bbc_all, use = "pairwise.complete")

modis <- ggplot(bbc_all, aes(x = mean_evi, y = mean_ndvi)) + geom_point() +
  labs(x = "MODIS EVI", y = "MODIS NDVI") + geom_abline(intercept = 0, slope = 1) + 
  annotate(geom = "text", x = 0.6, y = 0.1, label = "r = 0.93")

evi <- ggplot(bbc_all, aes(x = mean_evi, y = NDVI)) + geom_point() +
  labs(x = "MODIS EVI", y = "GIMMS NDVI")+ geom_abline(intercept = 0, slope = 1) +
  annotate(geom = "text", x = 0.6, y = 0.1, label = "r = 0.82")

ndvi <- ggplot(bbc_all, aes(x = mean_ndvi, y = NDVI)) + geom_point() +
  labs(x = "MODIS NDVI", y = "GIMMS NDVI") + geom_abline(intercept = 0, slope = 1) +
  annotate(geom = "text", x = 0.6, y = 0.1, label = "r = 0.84")

plot_grid(modis, evi, ndvi, nrow = 2)
ggsave('Figures/BBC_ndvi_comparison.pdf')

# BBS comparison
bbs_all <- bbs_modis %>%
  left_join(bbs_gimms, by = c("site" = "site_id")) 

cor(bbs_all, use = "pairwise.complete")

modis <- ggplot(bbs_all, aes(x = mean_evi, y = mean_ndvi)) + geom_point() +
  labs(x = "MODIS EVI", y = "MODIS NDVI") + geom_abline(intercept = 0, slope = 1) + 
  annotate(geom = "text", x = 0.6, y = 0.1, label = "r = 0.96")

evi <- ggplot(bbs_all, aes(x = mean_evi, y = ndvi)) + geom_point() +
  labs(x = "MODIS EVI", y = "GIMMS NDVI")+ geom_abline(intercept = 0, slope = 1) +
  annotate(geom = "text", x = 0.6, y = 0.1, label = "r = 0.84")

ndvi <- ggplot(bbs_all, aes(x = mean_ndvi, y = ndvi)) + geom_point() +
  labs(x = "MODIS NDVI", y = "GIMMS NDVI") + geom_abline(intercept = 0, slope = 1) +
  annotate(geom = "text", x = 0.6, y = 0.1, label = "r = 0.89")

plot_grid(modis, evi, ndvi, nrow = 2)
ggsave('Figures/BBS_ndvi_comparison.pdf')

## BBC data

# Get species list - diurnal land birds
species_list <- read.csv("data/species_list.csv", stringsAsFactors = F)

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


bbc_sppRich <- bbc %>%
  left_join(bbc_gimms, by = c("siteID", "year")) %>%
  group_by(siteID, NDVI, status, area, year) %>%
  summarize(nSpp = n_distinct(new_species),
            territories = sum(nTerritory)) %>%
  filter(status == "breeder") %>%
  group_by(siteID) %>%
  filter(year == max(year))

## BBS data

counts <- read_csv("data/final_bbs_subset.csv")

## Species richness vs ndvi

# BBS plots
bbs_env <- counts %>%
  left_join(bbs_all, by = c("stateroute" = "site"))

summary(lm(spRich ~ mean_evi, bbs_env))
summary(lm(spRich ~ mean_ndvi, bbs_env))
summary(lm(spRich ~ ndvi, bbs_env))

bbs_evi <- ggplot(bbs_env, aes(x = mean_evi, y = spRich)) + geom_point() +
  labs(x = "MODIS EVI") +
  annotate(geom = "text", x = 0.55, y = 25,
                label = c("R2 = 0.25"))
bbs_ndvi <- ggplot(bbs_env, aes(x = mean_ndvi, y = spRich)) + geom_point() +
  labs(x = "MODIS NDVI") +
  annotate(geom = "text", x = 0.75, y = 25,
                label = c("R2 = 0.27"))
bbs_gimms <- ggplot(bbs_env, aes(x = ndvi, y = spRich)) + geom_point() +
  labs(x = "GIMMS NDVI") +
  annotate(geom = "text", x = 0.75, y = 25,
                label = c("R2 = 0.32"))  

plot_grid(bbs_evi, bbs_ndvi, bbs_gimms, nrow = 2)
ggsave("Figures/BBS_vegind_spRich.pdf")

# BBC plots

bbc_env <- bbc_sppRich %>%
  left_join(bbc_modis, by = c("siteID" = "site", "year"))

summary(lm(nSpp ~ mean_evi, bbc_env))
summary(lm(nSpp ~ mean_ndvi, bbc_env))
summary(lm(nSpp ~ NDVI, bbc_env))

bbc_evi <- ggplot(bbc_env, aes(x = mean_evi, y = nSpp)) + geom_point() +
  labs(x = "MODIS EVI") +
  annotate(geom = "text", x = 0.1, y = 50,
           label = c("R2 < 0.001"))

bbc_ndvi <- ggplot(bbc_env, aes(x = mean_ndvi, y = nSpp)) + geom_point() +
  labs(x = "MODIS NDVI") +
  annotate(geom = "text", x = 0.1, y = 50,
           label = c("R2 = 0.004"))

bbc_gimms <- ggplot(bbc_env, aes(x = NDVI, y = nSpp)) + geom_point() +
  labs(x = "GIMMS NDVI") +
  annotate(geom = "text", x = 0.2, y = 50,
           label = c("R2 = 0.022"))  

plot_grid(bbc_evi, bbc_ndvi, bbc_gimms, nrow = 2)
ggsave("Figures/BBC_vegind_spRich.pdf")
