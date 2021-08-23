# AVHRR NDVI BBS route processing

library(sf)
library(raster)
library(tidyverse)
library(lubridate)
library(cowplot)

## AVHRR directory
avhrr_dir <- "/Users/gracedicecco/Desktop/data/avhrr_ndvi/"

## BBS sites
bbs_final <- read.csv("data/final_bbs_subset.csv", stringsAsFactors = F)

bbs_sites <- bbs_final %>%
  dplyr::select(stateroute, latitude, longitude) %>%
  distinct()

## AVHRR files
avhrr_files <- data.frame(dirname = list.files(avhrr_dir)) %>%
  mutate(filename = list.files(paste0(avhrr_dir, dirname))[grepl("ndvi.tif", 
                                                                list.files(paste0(avhrr_dir, dirname)))])

## BBS NDVI
avhrr_ndvi <- avhrr_files %>%
  group_by(dirname, filename) %>%
  nest() %>%
  mutate(data = map2(dirname, filename, ~{
    dir <- .x
    f <- .y
    
    ndvi <- raster(paste0(avhrr_dir, dir, "/", f))

    bbs_points <- bbs_sites %>%
      st_as_sf(coords = c("longitude", "latitude")) %>%
      st_set_crs(4326) %>%
      st_transform(crs(ndvi))
    
    bbs_ndvi <- raster::extract(ndvi, bbs_points)
    
    bbs_df <- data.frame(stateroute = bbs_points$stateroute, ndvi = bbs_ndvi)
    
    bbs_df
  }))

avhrr_res <- avhrr_ndvi %>%
  unnest(cols = c("data")) %>%
  group_by(stateroute) %>%
  summarize(mean_ndvi = mean(ndvi)/200,
            year = 2000)
write.csv(avhrr_res, "data/avhrr_ndvi_bbs.csv", row.names = F)

avhrr_res <- read_csv("data/avhrr_ndvi_bbs.csv")

## Compare to spp rich, MODIS & GIMMS NDVI values
bbs_gimms <- read_csv("data/gimms_ndvi_bbs_data.csv") %>%
  filter(month %in% c("may", "jun", "jul"), year %in% c(2000)) %>%
  group_by(site_id) %>%
  summarize(ndvi = mean(ndvi))

bbs_modis_ndvi <- read_csv("data/bbs_modis_ndvi.csv") %>%
  mutate(year = year(calendar_date)) %>%
  filter(year == 2000) %>%
  filter(value > 0) %>%
  mutate(ndvi = value/10000) %>%
  group_by(site) %>%
  summarize(mean_ndvi = mean(ndvi))

bbs_modis_evi <- read_csv("data/bbs_modis_evi.csv") %>%
  mutate(year = year(calendar_date)) %>%
  filter(year == 2000) %>%
  filter(value > 0) %>%
  mutate(evi = value/10000) %>%
  group_by(site) %>%
  summarize(mean_evi = mean(evi))

avhrr_compare <- bbs_gimms %>%
  left_join(bbs_modis_ndvi, by = c("site_id" = "site")) %>%
  left_join(bbs_modis_evi, by = c("site_id" = "site")) %>%
  left_join(avhrr_res, by = c("site_id" = "stateroute"), suffix = c(.x = "_modis", .y = "_avhrr"))
  
cor_table <- cor(avhrr_compare[,2:5], use = "pairwise.complete.obs")
write.table(cor_table, "data/avhrr_modis_gimms_cor_table.txt")

bbs_rich <- bbs_final %>%
  filter(year == 2000) %>%
  dplyr::select(stateroute, spRich) %>%
  distinct() %>%
  left_join(avhrr_res)

summary(lm(spRich ~ mean_ndvi, data = bbs_rich))
 
theme_set(theme_classic(base_size = 15))
ggplot(bbs_rich, aes(x= mean_ndvi, y = spRich)) + geom_point() +
  labs(x = "AVHRR NDVI 2000", y = "Species richness") +
  annotate(geom = "text", x = 0.85, y = 20, label = "R2 = 0.39")
ggsave("Figures/avhrr_bbs_rich.pdf")

## LM for BBS landscape diversity

bbs_env_het <- read.csv("data/bbs_site_env_heterogeneity.csv")

summary(lm(shannonH ~ ndvi.mean, bbs_env_het))

bbs_het_avhrr <- bbs_env_het %>%
  left_join(avhrr_res)

summary(lm(shannonH ~ mean_ndvi, bbs_het_avhrr))

## Foraging guild z-score
null_output_bins <- read.csv("data/bbs_null_output_bins.csv", header = TRUE)

null_output_bins_agg <- null_output_bins %>%
  group_by(stateroute) %>%
  summarize(FGnull_mean = mean(FGNull),
            FGnull_sd = sd(FGNull),
            FGObs = mean(FGObs),
            ndvi.mean = mean(ndvi.mean),
            Sobs = mean(Sobs),
            FGnull_pct = sum(FGObs < FGNull)/1000) %>%
  mutate(FG_z = (FGObs - FGnull_mean)/FGnull_sd)

summary(lm(FG_z ~ ndvi.mean, data = null_output_bins_agg))

null_output_avhrr <- null_output_bins_agg %>%
  left_join(avhrr_res)
summary(lm(FG_z ~ mean_ndvi, data = null_output_avhrr))


