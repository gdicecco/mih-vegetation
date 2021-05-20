library(rgdal)
library(raster)

prj.string <- "+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"

# nlcd_2001 <- raster("/proj/hurlbertlab/nlcd_landcover/nlcd_2001_landcover_2011_edition_2014_10_10/nlcd_2001_landcover_2011_edition_2014_10_10.img") 
# nlcd_2001 <-projectRaster(nlcd_2001, crs = prj.string)
  
lat_long = read.csv("data/latlongs.csv", header = TRUE)
# nbcd <- read.csv("Z:/GIS/NBCD/data/nbcd_processed.csv", header =TRUE) %>%
#   left_join(lat_long, by = "stateroute") %>%
#   dplyr::select(stateroute, nbcd.mean, latitude, longitude) %>%
#   # st_as_sf(coords = c("longitude", "latitude"), crs =prj.string) %>%
#   group_by(stateroute) %>%
#   # summarize(mean_mean = mean(nbcd.mean)) %>%
#   mutate(reclass_cat = case_when(nbcd.mean ==0 ~ 0,
#                                  nbcd.mean <= 5 ~ 1,
#                                  nbcd.mean >5 & nbcd.mean <= 15 ~ 2,
#                                  nbcd.mean > 15 ~ 3))

# temporary measure to test data
# masked_nlcd <- mask(nlcd_2001, nbcd)
# nlcd_nbcd <- raster::stack(nlcd_2001, nbcd) 

