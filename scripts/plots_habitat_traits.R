## Plots of abundance vs. NDVI and mean canopy height for different trait groups

library(tidyverse)

# Environmental data
env <- read.csv("data/ndvi_ncbd.csv", stringsAsFactors = F)

# BBS data
routes <- read.csv("\\\\Bioark.bio.unc.edu\\hurlbertlab\\Databases\\BBS\\2017\\bbs_routes_20170712.csv")
counts <- read.csv("\\\\Bioark.bio.unc.edu\\hurlbertlab\\Databases\\BBS\\2017\\bbs_counts_20170712.csv")
species <- read.csv("\\\\Bioark.bio.unc.edu\\hurlbertlab\\Databases\\BBS\\2017\\bbs_species_20170712.csv")
weather <- read.csv("\\\\Bioark.bio.unc.edu\\hurlbertlab\\Databases\\BBS\\2017\\bbs_weather_20170712.csv")

routes$stateroute <- routes$statenum*1000 + routes$route
weather$stateroute <-weather$statenum*1000 + weather$route
RT1 <- subset(weather, runtype == 1, select = c("stateroute", "year"))
RT1.routes <- merge(RT1, routes[ , c("statenum", "stateroute", "latitude", "longitude","bcr")], by = "stateroute", all.x = TRUE)
counts$stateroute <- counts$statenum*1000 + counts$route

landbirds <- species %>%
  filter(aou > 2880) %>%
  filter(aou < 3650 | aou > 3810) %>%
  filter(aou < 3900 | aou > 3910) %>%
  filter(aou < 4160 | aou > 4210) %>%
  filter(aou != 7010)

# Observations with RT=1, in BCRs of interest, 1990-present, diurnal land birds
counts.subs <- counts %>%
  filter(stateroute %in% RT1.routes$stateroute) %>%
  filter(year > 1994, year < 2011) %>%
  filter(aou %in% landbirds$aou)

## Traits

traits <- read.csv("data/spp_traits.csv", stringsAsFactors = F)
MO_correlates <- read.csv("data/Master_RO_Correlates_20110610.csv")

traits %<>% left_join(select(MO_correlates, AOU, Trophic.Group), by = c("aou" = "AOU")) %>%
  mutate(habStatus = ifelse(nHabitats1 > 2, "generalist", "specialist")) %>%
  mutate(dietStatus = ifelse(Trophic.Group == "omnivore" | Trophic.Group == "insct/om", "generalist", "specialist"))

### Plots

# Abundance at each route by habitat specialization
hab_counts <- counts.subs %>%
  left_join(traits) %>%
  group_by(stateroute, habStatus) %>%
  summarize(nIndiv = sum(speciestotal)) %>%
  left_join(env)

# N individuals vs. NDVI, nHabitats1 == 1 | 2 vs. nHabitats1 > 2
theme_set(theme_classic())
hab_ndvi <- filter(hab_counts, !is.na(habStatus)) %>%
  ggplot(aes(x = ndvi.mean, y = nIndiv, color = habStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean NDVI") + ylab("Number of individuals") + labs(color = "Habitat")
hab_ndvi

# N individuals vs. canopy height mean, nHabitats1 == 1 | 2 vs. nHabitats1 > 2

hab_nbcd <- filter(hab_counts, !is.na(habStatus)) %>%
  ggplot(aes(x = nbcd.mean, y = nIndiv, color = habStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean Canopy Height") + ylab("Number of individuals") + labs(color = "Habitat")
hab_nbcd

# Abundance at each route by diet specialization

diet_counts <- counts.subs %>%
  left_join(traits) %>%
  group_by(stateroute, dietStatus) %>%
  summarize(nIndiv = sum(speciestotal)) %>%
  left_join(env)

# N individuals vs. NDVI, omnivore | insct/om

diet_ndvi <- filter(diet_counts, !is.na(dietStatus)) %>%
  ggplot(aes(x = ndvi.mean, y = nIndiv, color = dietStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean NDVI") + ylab("Number of individuals") + labs(color = "Diet")
diet_ndvi

# N individuals vs. canopy height mean, omnivore | insct/om

diet_nbcd <- filter(diet_counts, !is.na(dietStatus)) %>%
  ggplot(aes(x = nbcd.mean, y = nIndiv, color = dietStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean Canopy Height") + ylab("Number of individuals") + labs(color = "Diet")
diet_nbcd

library(cowplot)
plot_grid(hab_ndvi, hab_nbcd, diet_ndvi, diet_nbcd, nrow = 2)

ggsave("figures/traits_indiv.pdf", width = 11, height = 8.5)
