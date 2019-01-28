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

traits %<>% left_join(select(MO_correlates, AOU, AbundClass, Trophic.Group), by = c("aou" = "AOU")) %>%
  mutate(habStatus = ifelse(nHabitats1 > 2, "generalist", "specialist")) %>%
  mutate(dietStatus = ifelse(Trophic.Group == "omnivore" | Trophic.Group == "insct/om", "generalist", "specialist"))

table(traits$habStatus, traits$dietStatus)
hab_test <- table(traits$habStatus, traits$AbundClass)[, -1]
diet_test <- table(traits$dietStatus, traits$AbundClass)[, -1]

chisq.test(hab_test)
chisq.test(diet_test)

### Plots

# productivity vs. canopy height

theme_set(theme_classic())

ndvi_nbcd <- ggplot(env, aes(x = ndvi.mean, y = nbcd.mean)) + geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = F, cex = 1.5) +
  labs(x = "Mean NDVI", y = "Mean Canopy Height")
ndvi_nbcd
ggsave("figures/ndvi_nbcd.pdf")

summary(lm(nbcd.mean ~ ndvi.mean, data = env))

# Abundance at each route by habitat specialization
hab_counts <- counts.subs %>%
  left_join(traits) %>%
  group_by(stateroute, habStatus, year) %>%
  summarize(nIndiv = sum(speciestotal), nSpp = n()) %>% 
  group_by(stateroute, habStatus) %>%
  summarize(meanIndiv = mean(nIndiv), meanS = mean(nSpp)) %>%
  left_join(env)

# N individuals vs. NDVI, nHabitats1 == 1 | 2 vs. nHabitats1 > 2
theme_set(theme_classic())
hab_ndvi <- filter(hab_counts, !is.na(habStatus)) %>%
  ggplot(aes(x = ndvi.mean, y = meanIndiv, color = habStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean NDVI") + ylab("Number of individuals") + labs(color = "Habitat")
hab_ndvi

habGen_mod_ndvi <- lm(meanIndiv ~ ndvi.mean, data = filter(hab_counts, habStatus == "generalist"))
habSp_mod_ndvi <- lm(meanIndiv ~ ndvi.mean, data = filter(hab_counts, habStatus == "specialist"))

summary(habGen_mod_ndvi)
summary(habSp_mod_ndvi)

# N individuals vs. canopy height mean, nHabitats1 == 1 | 2 vs. nHabitats1 > 2

hab_nbcd <- filter(hab_counts, !is.na(habStatus)) %>%
  ggplot(aes(x = nbcd.mean, y = meanIndiv, color = habStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean Canopy Height") + ylab("Number of individuals") + labs(color = "Habitat")
hab_nbcd

habGen_mod <- lm(meanIndiv ~ nbcd.mean, data = filter(hab_counts, habStatus == "generalist"))
habSp_mod <- lm(meanIndiv ~ nbcd.mean, data = filter(hab_counts, habStatus == "specialist"))

summary(habGen_mod)
summary(habSp_mod)

# Abundance at each route by diet specialization

diet_counts <- counts.subs %>%
  left_join(traits) %>%
  group_by(stateroute, dietStatus, year) %>%
  summarize(nIndiv = sum(speciestotal), nSpp = n()) %>%
  group_by(stateroute, dietStatus) %>%
  summarize(meanIndiv = mean(nIndiv), meanS = mean(nSpp)) %>%
  left_join(env)

# N individuals vs. NDVI, omnivore | insct/om

diet_ndvi <- filter(diet_counts, !is.na(dietStatus)) %>%
  ggplot(aes(x = ndvi.mean, y = meanIndiv, color = dietStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean NDVI") + ylab("Number of individuals") + labs(color = "Diet")
diet_ndvi

dietGen_mod_ndvi <- lm(meanIndiv ~ ndvi.mean, data = filter(diet_counts, dietStatus == "generalist"))
dietSp_mod_ndvi <- lm(meanIndiv ~ ndvi.mean, data = filter(diet_counts, dietStatus == "specialist"))

summary(dietGen_mod_ndvi)
summary(dietSp_mod_ndvi)

# N individuals vs. canopy height mean, omnivore | insct/om

diet_nbcd <- filter(diet_counts, !is.na(dietStatus)) %>%
  ggplot(aes(x = nbcd.mean, y = meanIndiv, color = dietStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean Canopy Height") + ylab("Number of individuals") + labs(color = "Diet")
diet_nbcd

dietGen_mod <- lm(meanIndiv ~ nbcd.mean, data = filter(diet_counts, dietStatus == "generalist"))
dietSp_mod <- lm(meanIndiv ~ nbcd.mean, data = filter(diet_counts, dietStatus == "specialist"))

summary(dietGen_mod)
summary(dietSp_mod)

# N individuals vs. NDVI, abundance class

abund_counts <- counts.subs %>%
  left_join(traits) %>%
  group_by(stateroute, AbundClass, year) %>%
  summarize(nIndiv = sum(speciestotal), nSpp = n()) %>% 
  group_by(stateroute, AbundClass) %>%
  summarize(meanIndiv = mean(nIndiv), meanS = mean(nSpp)) %>%
  left_join(env)

abund_ndvi <- filter(abund_counts, !is.na(AbundClass)) %>%
  ggplot(aes(x = ndvi.mean, y = meanIndiv, color = AbundClass)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("NDVI") + ylab("Number of individuals") + labs(color = "AbundClass")
abund_ndvi

abundHigh_mod_ndvi <- lm(meanIndiv ~ ndvi.mean, data = filter(abund_counts, AbundClass == "High"))
abundMed_mod_ndvi <- lm(meanIndiv ~ ndvi.mean, data = filter(abund_counts, AbundClass == "Med"))
abundLow_mod_ndvi <- lm(meanIndiv ~ ndvi.mean, data = filter(abund_counts, AbundClass == "Low"))

summary(abundHigh_mod_ndvi)
summary(abundMed_mod_ndvi)
summary(abundLow_mod_ndvi)

# N individuals vs. canopy height mean, abundance class

abund_nbcd <- filter(abund_counts, !is.na(AbundClass)) %>%
  ggplot(aes(x = nbcd.mean, y = meanIndiv, color = AbundClass)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean Canopy Height") + ylab("Number of individuals") + labs(color = "AbundClass")
abund_nbcd

abundHigh_mod <- lm(meanIndiv ~ nbcd.mean, data = filter(abund_counts, AbundClass == "High"))
abundMed_mod <- lm(meanIndiv ~ nbcd.mean, data = filter(abund_counts, AbundClass == "Med"))
abundLow_mod <- lm(meanIndiv ~ nbcd.mean, data = filter(abund_counts, AbundClass == "Low"))

summary(abundHigh_mod)
summary(abundMed_mod)
summary(abundLow_mod)

library(cowplot)
plot_grid(hab_ndvi, hab_nbcd, diet_ndvi, diet_nbcd, abund_ndvi, abund_nbcd, nrow = 3)

ggsave("figures/traits_indiv.pdf", width = 11, height = 8.5)

# Species-energy relationships with traits

# N species vs. NDVI, nHabitats1 == 1 | 2 vs. nHabitats1 > 2
theme_set(theme_classic())
hab_ndvi_S <- filter(hab_counts, !is.na(habStatus)) %>%
  ggplot(aes(x = ndvi.mean, y = meanS, color = habStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean NDVI") + ylab("Number of Species") + labs(color = "Habitat")
hab_ndvi_S

habGen_mod_ndvi_S <- lm(meanS ~ ndvi.mean, data = filter(hab_counts, habStatus == "generalist"))
habSp_mod_ndvi_S <- lm(meanS ~ ndvi.mean, data = filter(hab_counts, habStatus == "specialist"))

summary(habGen_mod_ndvi_S)
summary(habSp_mod_ndvi_S)

# N species vs. canopy height mean, nHabitats1 == 1 | 2 vs. nHabitats1 > 2

hab_nbcd_S <- filter(hab_counts, !is.na(habStatus)) %>%
  ggplot(aes(x = nbcd.mean, y = meanS, color = habStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean Canopy Height") + ylab("Number of Species") + labs(color = "Habitat")
hab_nbcd_S

habGen_mod_S <- lm(meanS ~ nbcd.mean, data = filter(hab_counts, habStatus == "generalist"))
habSp_mod_S <- lm(meanS ~ nbcd.mean, data = filter(hab_counts, habStatus == "specialist"))

summary(habGen_mod_S)
summary(habSp_mod_S)

# N species vs. NDVI, omnivore | insct/om

diet_ndvi_S <- filter(diet_counts, !is.na(dietStatus)) %>%
  ggplot(aes(x = ndvi.mean, y = meanS, color = dietStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean NDVI") + ylab("Number of Species") + labs(color = "Diet")
diet_ndvi_S

dietGen_mod_ndvi_S <- lm(meanS ~ ndvi.mean, data = filter(diet_counts, dietStatus == "generalist"))
dietSp_mod_ndvi_S <- lm(meanS ~ ndvi.mean, data = filter(diet_counts, dietStatus == "specialist"))

summary(dietGen_mod_ndvi_S)
summary(dietSp_mod_ndvi_S)

# N species vs. canopy height mean, omnivore | insct/om

diet_nbcd_S <- filter(diet_counts, !is.na(dietStatus)) %>%
  ggplot(aes(x = nbcd.mean, y = meanS, color = dietStatus)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean Canopy Height") + ylab("Number of Species") + labs(color = "Diet")
diet_nbcd_S

dietGen_mod_S <- lm(meanS ~ nbcd.mean, data = filter(diet_counts, dietStatus == "generalist"))
dietSp_mod_S <- lm(meanS ~ nbcd.mean, data = filter(diet_counts, dietStatus == "specialist"))

summary(dietGen_mod_S)
summary(dietSp_mod_S)

# N species vs. NDVI, abundance class

abund_ndvi_S <- filter(abund_counts, !is.na(AbundClass)) %>%
  ggplot(aes(x = ndvi.mean, y = meanS, color = AbundClass)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("NDVI") + ylab("Number of Species") + labs(color = "AbundClass")
abund_ndvi_S

abundHigh_mod_ndvi_S <- lm(meanS ~ ndvi.mean, data = filter(abund_counts, AbundClass == "High"))
abundMed_mod_ndvi_S <- lm(meanS ~ ndvi.mean, data = filter(abund_counts, AbundClass == "Med"))
abundLow_mod_ndvi_S <- lm(meanS ~ ndvi.mean, data = filter(abund_counts, AbundClass == "Low"))

summary(abundHigh_mod_ndvi_S)
summary(abundMed_mod_ndvi_S)
summary(abundLow_mod_ndvi_S)

# N species vs. canopy height mean, abundance class

abund_nbcd_S <- filter(abund_counts, !is.na(AbundClass)) %>%
  ggplot(aes(x = nbcd.mean, y = meanS, color = AbundClass)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean Canopy Height") + ylab("Number of Species") + labs(color = "AbundClass")
abund_nbcd_S

abundHigh_mod_S <- lm(meanS ~ nbcd.mean, data = filter(abund_counts, AbundClass == "High"))
abundMed_mod_S <- lm(meanS ~ nbcd.mean, data = filter(abund_counts, AbundClass == "Med"))
abundLow_mod_S <- lm(meanS ~ nbcd.mean, data = filter(abund_counts, AbundClass == "Low"))

summary(abundHigh_mod_S)
summary(abundMed_mod_S)
summary(abundLow_mod_S)

plot_grid(hab_ndvi_S, hab_nbcd_S, diet_ndvi_S, diet_nbcd_S, abund_ndvi_S, abund_nbcd_S, nrow = 3)

ggsave("figures/traits_spp.pdf", width = 11, height = 8.5)

