### MIH code ####
# want to make 4 plots

# NDVI:abun, NDVI:occ
# EVH:abun, EVH:occ
# for specialists/generalists and core-transient split out

library(ggplot2)
library(dplyr)
library(rgdal)
library(raster)

bbs = read.csv('data/bbs_abun.csv', header = TRUE) 
bbs$rich = 1
bbs_rich = bbs %>%
  group_by(stateroute) %>%
  count(rich) 
bbs_rich$sprich = bbs_rich$n
  
gimms_ndvi = read.csv("ENV DATA/gimms_ndvi_bbs_data.csv", header = TRUE)
gimms_agg = gimms_ndvi %>% filter(month == c("may", "jun", "jul")) %>% 
  group_by(site_id)  %>%  summarise(ndvi.mean=mean(ndvi))
gimms_agg$stateroute = gimms_agg$site_id
ndvi = gimms_agg[,c("stateroute", "ndvi.mean")]

ndvi_bbs = left_join(bbs_rich, ndvi, by = "stateroute")
ndvi_bbs = na.omit(ndvi_bbs)


ggplot(ndvi_bbs, aes(x = ndvi.mean, y = sprich)) + geom_point()

# canopy height data #
nbcd =  read.csv("//bioark/HurlbertLab/GIS/NBCD/data/nbcd_processed.csv", header = TRUE)
nbcd_bbs = left_join(bbs_rich, nbcd, by = "stateroute")
nbcd_bbs = na.omit(nbcd_bbs)

ggplot(nbcd_bbs, aes(x = nbcd.mean, y = sprich)) + geom_point() + geom_smooth(method = "lm")


routes_nbcd = routes[routes@data$stateroute %in% nbcd_bbs$stateroute,]
plot(routes_nbcd)

ndvi_ncbd = inner_join(ndvi, nbcd, by = "stateroute")
write.csv(ndvi_ncbd, "ndvi_ncbd.csv", row.names = FALSE)




