library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(stringr)

setwd("\\\\Bioark.bio.unc.edu/hurlbertlab/Databases/BBS/GPS_stoplocations/")

us_routes <- readOGR("bbsrte_2012_alb/bbsrte_2012_alb.shp")

# subset routes that are between 38000 and 42000 m, remove Alaska (rteno between 3000 and 4000)

us_routes_short <- us_routes[us_routes@data$rte_length < 42000 & us_routes@data$rte_length > 38000, ]
us_subs <- us_routes_short[!(us_routes_short@data$rteno < 4000 & us_routes_short@data$rteno > 3000), ]

bufferRoutes <- gBuffer(us_subs, width = 1000, byid = TRUE)

setwd("\\\\Bioark.bio.unc.edu\\hurlbertlab\\DiCecco\\")
writeOGR(bufferRoutes, ".", "bbsroutes_1km_buffer", driver = "ESRI Shapefile")
