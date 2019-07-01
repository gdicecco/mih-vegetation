## Niche complexity measure for BBS routes

library(tidyverse)

bbs_subset <- read.csv("data/final_bbs_subset.csv", stringsAsFactors = F)

nbcd <- read.csv("data/ndvi_nbcd.csv", stringsAsFactors = F)

newcode <- data.frame(code = seq(1,9), 
                      legend = c("Open water", "Urban", "Barren", "Forest", "Shrubland", 
                                 "Agricultural", "Grasslands", "Wetlands", "Perennial ice, snow"))

frags <- read.csv("data/fragmentation_indices_nlcd_simplified.csv", stringsAsFactors = F) %>%
  filter(year == 2001) %>%
  left_join(newcode, by = c("class" = "code"))


troph_guild <- read.csv("data/Troph_guilds.csv", header = TRUE)
bbs_sub1 <- read.csv("data/final_bbs_subset.csv", header = TRUE)
tax_code <- read.csv("data/Bird_Taxonomy.csv", header = TRUE) %>%
  dplyr::select(AOU_OUT, CRC_SCI_NAME) %>%
  unique() %>% na.omit()

tax_code1 = tax_code[-grep("/", tax_code$CRC_SCI_NAME),] 
tax_code2 = tax_code1[-grep("sp.", tax_code1$CRC_SCI_NAME),]


troph_AOU <- left_join(troph_guild, tax_code2, by = c("Species" = "CRC_SCI_NAME"))
troph_AOU$Species = gsub('Dendroica','Setophaga', troph_AOU$Species)

bbs_sub2 <- filter(bbs_sub1, aou %in% tax_code2$AOU_OUT)
bbs_troph <- left_join(bbs_sub2, troph_AOU, by = c("aou" = "AOU_OUT"))
  
### Measure of niche complexity for each BBS route

forest_ed <- frags %>%
  group_by(stateroute, year) %>%
  mutate(sum.area = sum(total.area)) %>%
  filter(legend == "Forest") %>%
  group_by(stateroute, year) %>%
  summarize(ED = total.edge/sum.area,
            propForest = prop.landscape,
            meanPatchArea = mean.patch.area) %>%
  mutate(edge = ifelse(ED >= 0.1 & propForest >= 0.1, 1, 0))

n_habs <- frags %>%
  group_by(stateroute, year, legend) %>%
  summarize(sum.area = sum(total.area)) %>%
  filter(sum.area > 10000) %>%
  filter(legend != "Open water", legend != "Barren", legend != "Perennial ice, snow", legend != "Wetlands") %>%
  group_by(stateroute) %>%
  summarize(n_habs = n_distinct(legend))

n_layers <- nbcd %>%
  group_by(stateroute) %>%
  summarize(nbcd.mean = mean(nbcd.mean)) %>%
  mutate(can_height = nbcd.mean/10) %>%
  mutate(n_layers = case_when(can_height == 0 ~ 0,
                              can_height > 0 & can_height <= 5 ~ 1,
                              can_height >5 & can_height <= 15 ~ 2,
                              can_height > 15 ~ 3))

niche_complex <- forest_ed %>%
  left_join(n_habs, by = "stateroute") %>%
  left_join(n_layers, by = "stateroute") %>%
  filter(!is.na(can_height), !is.na(n_habs)) %>%
  mutate(n_niche = sum(edge, n_habs, n_layers))

### Niche complexity vs. number foraging guilds, number spp

nSpp <- bbs_troph %>%
  group_by(stateroute) %>%
  summarize(nSpp = n_distinct(aou),
            nGuilds = n_distinct(Trophic.guild)) %>%
  left_join(niche_complex, by = "stateroute") %>%
  left_join(nbcd, by = "stateroute") %>%
  filter(!is.na(year))

theme_set(theme_classic())
ggplot(nSpp, aes(x = n_niche, y = nSpp)) +
  geom_point() + labs(x = "Niche complexity", y = "Species")
ggsave("Figures/richness_niche_complexity.pdf")

niche_mod <- lm(nSpp ~ n_niche, data = nSpp)
ndvi_mod <- lm(nSpp ~ ndvi.mean, data = nSpp)
both_mod <- lm(nSpp ~ n_niche + ndvi.mean, data = nSpp)

summary(niche_mod)$r.squared
summary(ndvi_mod)$r.squared
summary(both_mod)$r.squared

ggplot(nSpp, aes(x = ndvi.mean, y = n_niche)) + geom_point()

ggplot(nSpp, aes(x = n_niche, y = nGuilds)) + geom_point(alpha = 0.1) +
  labs(x = "Niche complexity", y = "Foraging guilds")
ggsave("Figures/guilds_niche_complexity.pdf")

ggplot(nSpp, aes(x = ndvi.mean, y = nGuilds)) + geom_point(alpha = 0.1)

## NDVI and presence of forest edge explain 53% variation in species richness

summary(lm(nSpp ~ ndvi.mean + n_habs + n_layers + edge, data = nSpp)) # add almost 7 species when edge is present

summary(lm(nGuilds ~ ndvi.mean + n_habs + n_layers + edge, data = nSpp)) # nHabs -0.2 foraging guilds per additional habitat
# add one foraging guild for edge presence

## NDVI vs. number of layers predicting species richness and foraging guild richness

layer_mod <- lm(nSpp ~ n_layers, data = nSpp)
layer_ndvi_mod <- lm(nSpp ~ n_layers + ndvi.mean, data = nSpp)

summary(layer_mod)$r.squared
summary(ndvi_mod)$r.squared
summary(layer_ndvi_mod)$r.squared


layer_g_mod <- lm(nGuilds ~ n_layers, data = nSpp)
ndvi_g_mod <- lm(nGuilds ~ ndvi.mean, data = nSpp)
layer_ndvi_g_mod <- lm(nGuilds ~ n_layers + ndvi.mean, data = nSpp)

summary(layer_g_mod)$r.squared
summary(ndvi_g_mod)$r.squared
summary(layer_ndvi_g_mod)$r.squared

