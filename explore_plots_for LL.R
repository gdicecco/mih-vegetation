### MIH code ####
# want to make 4 plots
# NDVI:richness
# landfire:richness
# for specialists/generalists and core-transient split out
# setwd("C:/git/Biotic-Interactions")
library(ggplot2)
library(dplyr)
library(rgdal)
library(raster)

bbs = read.csv('data/bbs_abun.csv', header = TRUE) 
evh =  raster("//bioark/HurlbertLab/GIS/Landfire/us_140evh2.tif")

# Define projection to be used throughout analysis
prj.string <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# evh = projectRaster(evh, crs = prj.string) #UNMASKED!
# derived from BBS_occ script
routes = read.csv("data/latlong_rtes.csv",header =TRUE)
routes$latitude = abs(routes$latitude)
# Makes routes into a spatialPointsDataframe
coordinates(routes)=c('longitude','latitude')
projection(routes) = CRS("+proj=longlat +ellps=WGS84")

# Transforms routes to an equal-area projection - see previously defined prj.string
routes.laea = spTransform(routes, CRS(prj.string))

# A function that draws a circle of radius r around a point: p (x,y)
RADIUS = 40

make.cir = function(p,r){
 points=c()
 for(i in 1:360){
 theta = i*2*pi/360
   y = p[2] + r*cos(theta)
   x = p[1] + r*sin(theta)
   points = rbind(points,c(x,y))
}
points=rbind(points,points[1,])
circle=Polygon(points,hole=F)
circle
}

#Draw circles around all routes
circs = sapply(1:nrow(routes.laea), function(x){
  circ = make.cir(routes.laea@coords[x,],RADIUS)
  circ = Polygons(list(circ),ID=routes.laea@data$stateroute[x])
}
)

circs.5 = SpatialPolygons(circs, proj4string = CRS(prj.string))
circs.sp = getSpPPolygonsIDSlots(circs.5)
row.names(circs.5) <- getSpPPolygonsIDSlots(circs.5)
circs.df = SpatialPolygonsDataFrame(circs.5, data.frame(id = unique(circs.sp), row.names = unique(circs.sp)))

evh.point.ext200 = raster::extract(evh, circs.5)

evhdf <- data.frame(routes, unlist(evh.point.ext), nrow=5652, byrow=T)
# write.csv(evhdf, "evhdf.csv", row.names = FALSE)

evhdf_reclass = read.csv("evhdf.csv", header = TRUE)
evhtidy = evhdf_reclass %>%
  gather(class, val, 7:11)

# evhdf_tidy = read.csv("evhtidy.csv", header = TRUE)

# read in bird range shps
shapefile_path = 'Z:/GIS/NBCD/data/'

# for first time
# all_regions_baw = all_regions_list[grepl("_BAW_height", all_regions_list)] 

all_regions_baw = all_regions_list[grepl("_BAW_height.tif", all_regions_list)] 

nbcd = c()
setwd('Z:/GIS/NBCD/data') # 51BAW errors when run alone
  for(i in all_regions_baw[52:66]){
    # only need to untar and tif first time loop is run
    # untar(paste0(shapefile_path, i))
    # tif = strsplit(i, ".tgz")
    # nbcdheight2 = raster(paste0(shapefile_path, tif, '.tif'))
    nbcdheight2 = raster(i)
    names(nbcdheight2)<-"rast_layer"
    nbcdheight2[nbcdheight2$rast_layer == -32768] <- NA
    nbcd.point = raster::extract(nbcdheight2, routes)
    nbcd.mean = raster::extract(nbcdheight2, circs.5, fun = mean, na.rm=T)
    nbcd.var = raster::extract(nbcdheight2, circs.5, fun = var, na.rm=T)
    nbcd_inter = data.frame(stateroute = names(circs.5), nbcd.point = nbcd.point, nbcd.mean = nbcd.mean, nbcd.var = nbcd.var,file = i)
    nbcd_inter = na.omit(nbcd_inter)
    nbcd = rbind(nbcd, (nbcd_inter))
     # rbind(data.frame(stateroute = names(circs.5), nbcd.point = nbcd.point, nbcd.mean = nbcd.mean, nbcd.var = nbcd.var)
  }


# nbcd = data.frame(nbcd)

