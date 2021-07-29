## MODIS NDVI data at BBS sites

library(MODISTools)
library(tidyverse)

## BBS sites

bbs_final <- read.csv("data/final_bbs_subset.csv", stringsAsFactors = F)

bbs_sites <- bbs_final %>%
  dplyr::select(stateroute, latitude, longitude) %>%
  distinct() %>%
  rename(site_name = "stateroute",
         lat = "latitude",
         lon = "longitude")

bbs_modis_years <- c(2000:2004)

## MODIS NDVI: May 1-July 31
# MOD13Q1

modis_bands <- mt_bands(product = "MOD13Q1")
ndvi_band <- modis_bands$band[grep("NDVI", modis_bands$band)]
evi_band <- modis_bands$band[grep("EVI", modis_bands$band)]


## Extract MODIS data

bbs_modis_ndvi <- vector(mode = "list", length = length(bbs_modis_years))
bbs_modis_evi <- vector(mode = "list", length = length(bbs_modis_years))

for(i in 3:length(bbs_modis_years)) {
  y <- bbs_modis_years[i]
  
  modis_ndvi <- mt_batch_subset(df = bbs_sites[1:950, ],
                                product = "MOD13Q1",
                                start = paste0(y, "-05-01"),
                                end = paste0(y, "-07-31"),
                                band = ndvi_band)
  
  # for(j in 2:nrow(bbs_sites)) {
  #   res <- mt_batch_subset(df = bbs_sites[j, ],
  #                                 product = "MOD13Q1",
  #                                 start = paste0(y, "-05-01"),
  #                                 end = paste0(y, "-07-31"),
  #                                 band = ndvi_band)
  #   
  #   modis_ndvi <- rbind(modis_ndvi, res)
  # }
  # 
  
  bbs_modis_ndvi[[i]] <- modis_ndvi
  
  modis_evi <- mt_batch_subset(df = bbs_sites[1:950, ],
                               product = "MOD13Q1",
                               start = paste0(y, "-05-01"),
                               end = paste0(y, "-07-31"),
                               band = evi_band)
  
  # for(k in 2:nrow(bbs_sites)) {
  #   res2 <- mt_batch_subset(df = bbs_sites[k, ],
  #                          product = "MOD13Q1",
  #                          start = paste0(y, "-05-01"),
  #                          end = paste0(y, "-07-31"),
  #                          band = evi_band)
  #   
  #   modis_evi <- rbind(modis_evi, res2)
  # }
    
  
  bbs_modis_evi[[i]] <- modis_evi
  
  print(paste(Sys.time(), y))
}

modis_ndvi_df <- do.call(rbind.data.frame, bbs_modis_ndvi)

modis_evi_df <- do.call(rbind.data.frame, bbs_modis_evi)

write.csv(modis_ndvi_df, "data/bbs_modis_ndvi.csv", row.names = F)
write.csv(modis_evi_df, "data/bbs_modis_evi.csv", row.names = F)