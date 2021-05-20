library(rgdal)
library(raster)

prj.string <- "+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"

nlcd_2001 <- raster("/proj/hurlbertlab/nlcd_landcover/nlcd_2001_landcover_2011_edition_2014_10_10/nlcd_2001_landcover_2011_edition_2014_10_10.img") 
nlcd_2001 <-projectRaster(nlcd_2001, crs = prj.string)
writeRaster(nlcd_2001, "nlcd_2001.tif")