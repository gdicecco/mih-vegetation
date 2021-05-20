## Get FRAGSTATS land cover for BBC site areas

library(sp)
library(rgdal)
library(raster)
library(SDMTools)
library(rgeos)

# 2001
setwd("/proj/hurlbertlab/nlcd_landcover/nlcd_2001_landcover_2011_edition_2014_10_10/")
nlcd <- raster("nlcd_2001_landcover_2011_edition_2014_10_10.img")
bbc_tr <- readOGR("/proj/hurlbertlab/gdicecco/bbc_shp/bbc_site_areas_transf.shp")

bbcnos <- bbc_tr@data[ , 1]

setwd("/proj/hurlbertlab/gdicecco/nlcd_bbc/")
for(i in 1:nrow(bbc_tr@data)) {
  rte <- subset(bbc_tr, siteID == bbcnos[i])
  rtenum <- bbcnos[i]
  
  nlcd_ext <- as(extent(nlcd), "SpatialPolygons")
  
  if(gContainsProperly(nlcd_ext, rte)) {
    nlcd_crop <- crop(nlcd, rte)
    nlcd_mask <- mask(nlcd_crop, rte)
    class <- ClassStat(nlcd_mask)
    filename <- paste0("classStat_nlcd_30x30_2001_bbc_", rtenum, ".csv")
    print(filename)
    write.csv(class, filename, row.names = F)
  } else {
    print(rtenum)
  }

}
