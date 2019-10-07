library(rgdal)
library(raster)

prj.string <- "+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"

nlcd_2001 <- raster("/proj/hurlbertlab/nlcd_landcover/nlcd_2001_landcover_2011_edition_2014_10_10/nlcd_2001_landcover_2011_edition_2014_10_10.img") 
nlcd_2001 <-projectRaster(nlcd_2001, crs = prj.string)
  
lat_long = read.csv("data/latlongs.csv", header = TRUE)
nbcd <- read.csv("Z:/GIS/NBCD/data/nbcd_processed.csv", header =TRUE) %>%
  left_join(lat_long, by = "stateroute") %>%
  dplyr::select(stateroute, nbcd.mean, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs =prj.string) %>%
  group_by(stateroute) %>%
  summarize(mean_mean = mean(nbcd.mean))

# temporary measure to test data
masked_nlcd <- mask(nlcd_2001, nbcd)
nlcd_nbcd <- raster::stack(nlcd_2001, nbcd) 
# stack the nlcd mask with nbcd
# then try to reclassify into more cats (NBCD val for pixel to reclass the nlcd)
# 0-5, 5-10, 10+