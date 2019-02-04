## Plots of abundance vs. NDVI and mean canopy height for different trait groups

library(tidyverse)
library(purrr)
library(forcats)
library(cowplot)

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

# Observations with RT=1, in BCRs of interest, 1995-present, diurnal land birds
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

hist(traits$nHabitats1)

# Abundance at each route by habitat specialization
hab_counts <- counts.subs %>%
  left_join(traits) %>%
  group_by(stateroute, habStatus, year) %>%
  summarize(nIndiv = sum(speciestotal), nSpp = n()) %>% 
  group_by(stateroute, habStatus) %>%
  summarize(meanIndiv = mean(nIndiv), meanS = mean(nSpp)) %>%
  left_join(env)

# Abundance at each route by diet specialization
diet_counts <- counts.subs %>%
  left_join(traits) %>%
  group_by(stateroute, dietStatus, year) %>%
  summarize(nIndiv = sum(speciestotal), nSpp = n()) %>%
  group_by(stateroute, dietStatus) %>%
  summarize(meanIndiv = mean(nIndiv), meanS = mean(nSpp)) %>%
  left_join(env)

# Abundance class range-wide
avg_abund <- counts.subs %>%
  group_by(aou) %>%
  summarize(annual_abund = mean(speciestotal))

avg_quantile <- quantile(avg_abund$annual_abund)

avg_abund$abund_class <- ifelse(avg_abund$annual_abund < avg_quantile[2], "low",
                                ifelse(avg_abund$annual_abund > avg_quantile[4], "high", "medium"))
traits <- traits %>%
  left_join(avg_abund) # range-wide abundance class

# Abundance class at each route
abund_counts <- counts.subs %>%
  group_by(stateroute, aou) %>%
  summarize(annual_abund = mean(speciestotal)) %>%
  group_by(stateroute) %>%
  nest() %>%
  mutate(abundClass = purrr::map(data, ~{
    df <- .
    abund_class <- ifelse(df$annual_abund < avg_quantile[2], "low", ifelse(df$annual_abund > avg_quantile[4], "high", "medium"))
    cbind(df, abund_class)
  })) %>%
  dplyr::select(-data) %>%
  unnest() %>%
  group_by(stateroute, abund_class) %>%
  summarize(nIndiv = sum(annual_abund), nSpp = n()) %>%
  left_join(env)

abund_counts$abund_class <- fct_relevel(abund_counts$abund_class, "high", after = 2)

## Compares range-wide abundance vs. diet and habitat traits
table(traits$habStatus, traits$dietStatus)
hab_test <- table(traits$habStatus, traits$abund_class)
diet_test <- table(traits$dietStatus, traits$abund_class)

chisq.test(hab_test)
chisq.test(diet_test)

### Plots

theme_set(theme_classic())

## productivity vs. canopy height
ndvi_nbcd <- ggplot(env, aes(x = ndvi.mean, y = nbcd.mean)) + geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = F, cex = 1.5) +
  labs(x = "Mean NDVI", y = "Mean Canopy Height")
ndvi_nbcd
ggsave("figures/ndvi_nbcd.pdf")

summary(lm(nbcd.mean ~ ndvi.mean, data = env))

## Traits - env vs. number of individuals
# N individuals vs. NDVI, nHabitats1 == 1 | 2 vs. nHabitats1 > 2
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

abund_ndvi <- filter(abund_counts) %>%
  ggplot(aes(x = ndvi.mean, y = nIndiv, color = abund_class)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("NDVI") + ylab("Number of individuals") + labs(color = "AbundClass")
abund_ndvi

abundHigh_mod_ndvi <- lm(nIndiv ~ ndvi.mean, data = filter(na.omit(abund_counts), abund_class == "high"))
abundMed_mod_ndvi <- lm(nIndiv ~ ndvi.mean, data = filter(na.omit(abund_counts), abund_class == "medium"))
abundLow_mod_ndvi <- lm(nIndiv ~ ndvi.mean, data = filter(na.omit(abund_counts), abund_class == "low"))

summary(abundHigh_mod_ndvi)
summary(abundMed_mod_ndvi)
summary(abundLow_mod_ndvi)

# N individuals vs. canopy height mean, abundance class

abund_nbcd <- filter(na.omit(abund_counts)) %>%
  ggplot(aes(x = nbcd.mean, y = nIndiv, color = abund_class)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean Canopy Height") + ylab("Number of individuals") + labs(color = "AbundClass")
abund_nbcd

abundHigh_mod <- lm(nIndiv ~ nbcd.mean, data = filter(abund_counts, abund_class == "high"))
abundMed_mod <- lm(nIndiv ~ nbcd.mean, data = filter(abund_counts, abund_class == "medium"))
abundLow_mod <- lm(nIndiv ~ nbcd.mean, data = filter(abund_counts, abund_class == "low"))

summary(abundHigh_mod)
summary(abundMed_mod)
summary(abundLow_mod)

# Combine plots
plot_grid(hab_ndvi, hab_nbcd, diet_ndvi, diet_nbcd, abund_ndvi, abund_nbcd, nrow = 3)

ggsave("figures/traits_indiv.pdf", width = 11, height = 8.5)

## Traits - env vs. number of species

# N species vs. NDVI, nHabitats1 == 1 | 2 vs. nHabitats1 > 2
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

abund_ndvi_S <- filter(abund_counts) %>%
  ggplot(aes(x = ndvi.mean, y = nSpp, color = abund_class)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("NDVI") + ylab("Number of Species") + labs(color = "AbundClass")
abund_ndvi_S

abundHigh_mod_ndvi_S <- lm(nSpp ~ ndvi.mean, data = filter(abund_counts, abund_class == "high"))
abundMed_mod_ndvi_S <- lm(nSpp ~ ndvi.mean, data = filter(abund_counts, abund_class == "medium"))
abundLow_mod_ndvi_S <- lm(nSpp ~ ndvi.mean, data = filter(abund_counts, abund_class == "low"))

summary(abundHigh_mod_ndvi_S)
summary(abundMed_mod_ndvi_S)
summary(abundLow_mod_ndvi_S)

# N species vs. canopy height mean, abundance class

abund_nbcd_S <- filter(abund_counts) %>%
  ggplot(aes(x = nbcd.mean, y = nSpp, color = abund_class)) + geom_point(alpha = 0.15) + geom_smooth(method = "lm", se = F) +
  scale_y_log10() + xlab("Mean Canopy Height") + ylab("Number of Species") + labs(color = "AbundClass")
abund_nbcd_S

abundHigh_mod_S <- lm(nSpp ~ nbcd.mean, data = filter(abund_counts, abund_class == "high"))
abundMed_mod_S <- lm(nSpp ~ nbcd.mean, data = filter(abund_counts, abund_class == "medium"))
abundLow_mod_S <- lm(nSpp ~ nbcd.mean, data = filter(abund_counts, abund_class == "low"))

summary(abundHigh_mod_S)
summary(abundMed_mod_S)
summary(abundLow_mod_S)

# Joint plot
plot_grid(hab_ndvi_S, hab_nbcd_S, diet_ndvi_S, diet_nbcd_S, abund_ndvi_S, abund_nbcd_S, nrow = 3)

ggsave("figures/traits_spp.pdf", width = 11, height = 8.5)

