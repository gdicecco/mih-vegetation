# Script to create Figure 1 maps #
library(tidyverse)
library(ggplot2)
library(cowplot)
library(tmap)
library(sf)
library(rgdal)
library(raster)


setwd("\\\\BioArk/hurlbertlab/Databases/BBS/GPS_stoplocations/")
us_routes <- read_sf("bbsrte_2012_alb/bbsrte_2012_alb.shp")
us <- tm_shape(us_states) + tm_borders() + tm_fill(col = "light gray")

# Read in BBS
final.counts <- read.csv("data/final_bbs_subset.csv", header = TRUE) 
bbs_plot_rtes <- filter(us_routes, rteno %in% final.counts$stateroute)

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

# Map of census locations
bbc_sf <- bbc_censuses %>%
  left_join(sites_distinct, by = c("siteID", "sitename")) %>%
  dplyr::select(siteID, sitename, latitude, longitude, year, area, richness) %>%
  right_join(site_census, by = c("siteID", "year")) %>%
  mutate_at("longitude", .funs = ~{.*-1}) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(us_states)) %>%
  group_by(siteID, area) %>%
  summarize(nCensus = length(unique(year)), 
            censusYear = max(year),
            lengthCensus = case_when(min(year) > 1999 & max(year) < 2010 ~ "2000s",
                                     min(year) > 1989 & max(year) < 2000 ~ "1990s",
                                     max(year) < 1990 ~ "1980s",
                                     min(year) < 1990 & max(year) < 2000 ~ "1980s-1990s",
                                     min(year) < 1990 & max(year) > 1999 ~ "1980s-2000s",
                                     min(year) > 1989 & max(year) > 2000 ~ "1990s-2000s"
            ))

us_states

#### Fig 1 map ####
us <- tm_shape(us_states) + tm_borders() + tm_fill(col = "gray")
bbc_map <- us + tm_shape(bbc_sf) + 
  tm_dots(col = "nCensus", alpha = 1, size = 1, palette = "GnBu", title = "Number of Censuses")


point_map <- us + tm_shape(bbs_plot_rtes) + tm_lines(lwd = 2) + tm_shape(bbc_sf) + tm_dots(col = "black", size =0.2)+ 
  tm_add_legend(type = c("line"), labels = c("BBS"),lwd = 2, size = 32, col = "black") +
  tm_add_legend(type = c("symbol"), labels = c(" BBC"), shape = 16,col = "black", size = 32)
tmap_save(point_map, "Figures/Figure1.pdf")



theme_set(theme_classic())

env_bbs_rich <- read.csv("data/env_bbs_rich.csv", header = TRUE)
bbs <- ggplot(env_bbs_rich, aes(x = ndvi.mean, y = spRich)) + 
  geom_point(col = "black", shape=16, size = 2) + 
  geom_smooth(method = "lm", se = FALSE, lwd =1.25) + 
  annotate("text", x = .16, y = 89, label = "BBS", size =11) +
  theme_classic() + xlab("Mean NDVI")+ ylab("Species richness") +
  theme(axis.text.x=element_text(size = 28),axis.text.y=element_text(size=28)) +
  theme(axis.title.x=element_text(size = 32),axis.title.y=element_text(size=32, vjust = 2))
ggsave("Figures/rich_ndvi.pdf", height = 8, width = 12)

summary(lm(spRich ~ ndvi.mean, data = env_bbs_rich))

sppRich <- read.csv("data/env_bbc_rich.csv", header = TRUE)
bbc <- ggplot(sppRich, aes(x = NDVI, y = nSpp)) +
  geom_point(size = 2, col = "black") +
  labs(x = "Mean NDVI", y = "Species richness") +
  geom_smooth(method = "lm", se = FALSE, lwd =1.25) + 
  annotate("text", x = .12, y = 45, label = "BBC", size =11) +
  theme(axis.text.x=element_text(size = 28),axis.text.y=element_text(size=28)) +
  theme(axis.title.x=element_text(size = 32),axis.title.y=element_text(size=32, vjust = 2)) 
ggsave("Figures/spp_rich_ndvi.pdf")

summary(lm(nSpp ~ NDVI, data = sppRich))

plot_grid(bbs + theme(legend.position="none"),
          bbc + theme(legend.position="none"),
          nrow = 2,
          align = 'v',
          labels = c("A","B"),
          label_size = 28,
          hjust = -4.5)
ggsave("Figures/cowplot_Figure1.pdf", width = 8, height = 10)
