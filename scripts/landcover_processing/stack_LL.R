library(rgdal)
library(raster)
library(dplyr)

# setwd("C:/git/mih-vegetation")

prj.string <- "+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"

nlcd_2001 <- raster("nlcd_2001.tif")
lat_long = read.csv("latlongs.csv", header = TRUE)
nbcd <- read.csv("nbcd_processed.csv", header =TRUE) %>%
  left_join(lat_long, by = "stateroute") %>%
  dplyr::select(stateroute, nbcd.mean, latitude, longitude) %>%
  group_by(stateroute) %>%
  mutate(reclass_cat = case_when(nbcd.mean ==0 ~ 0,
                                 nbcd.mean <= 5 ~ 1,
                                 nbcd.mean >5 & nbcd.mean <= 15 ~ 2,
                                 nbcd.mean > 15 ~ 3))


nbcd_proj <- SpatialPointsDataFrame(nbcd[,c("longitude", "latitude")], nbcd[,c("stateroute", "nbcd.mean", "reclass_cat")], proj4string = CRS(prj.string))
rast_dim <- raster(nrows=104424, ncols=161190, xmn=-2493045, xmx=2342655, ymn=177285, ymx=3310005, crs = prj.string)
nbcd_raster <- rasterize(nbcd_proj, rast_dim)
nbcd_raster_proj <- projectRaster(nbcd_raster, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"))
nlcd_nbcd <- raster::stack(nlcd_2001, nbcd_raster_proj)
writeRaster(nlcd_nbcd, "nlcd_nbcd.tif")





# nlcd_nbcd <- raster("Z:/Snell/nlcd_nbcd.tif")




