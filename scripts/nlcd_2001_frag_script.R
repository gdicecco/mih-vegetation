# Calculate fragmentation measures for routes, all BCRS
# 2001 - not simplified

#### Libraries ####
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(stringr)
library(SDMTools)

# Read in BBS routes shapefile

# 2001
setwd("/proj/hurlbertlab/nlcd_landcover/nlcd_2001_landcover_2011_edition_2014_10_10/")
nlcd <- raster("nlcd_2001_landcover_2011_edition_2014_10_10.img")
routes <- readOGR("/proj/hurlbertlab/gdicecco/nlcd_frag_proj_shapefiles/BBS_routepaths/bbsroutes_1km_buffer.shp")
routes_tr <- spTransform(routes, crs(nlcd))

routenos <- routes_tr@data[ , 1]

setwd("/proj/hurlbertlab/gdicecco/nlcd_2001_frag/")
for(i in 1:nrow(routes_tr@data)) {
  rte <- subset(routes_tr, rteno == routenos[i])
  rtenum <- routenos[i]
  nlcd_crop <- crop(nlcd, rte)
  nlcd_mask <- mask(nlcd_crop, rte)
  class <- ClassStat(nlcd_mask)
  filename <- paste0("classStat_nlcd_30x30_2001_route_", rtenum, ".csv")
  write.csv(class, filename, row.names = F)
}
