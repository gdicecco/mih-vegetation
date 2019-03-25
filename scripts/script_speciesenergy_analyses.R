### Dataset for species-energy questions
# BBS routes consecutively sampled between 2000-2004, removing transient species and routes that are mostly agricultural

library(tidyverse)
library(ggplot2)
library(cowplot)

### Read in data

# Environmental data

# ndvi data #
#gimms_ndvi = read.csv("data/gimms_ndvi_bbs_data.csv", header = TRUE)
#gimms_agg = gimms_ndvi %>% 
#  filter(year >= 2000, year <= 2004, month == c("may", "jun", "jul")) %>% 
#  group_by(site_id)  %>%  
#  dplyr::summarise(ndvi.mean=mean(ndvi))
#gimms_agg$stateroute = gimms_agg$site_id
#ndvi = gimms_agg[,c("stateroute", "ndvi.mean")]

# canopy height data #
#nbcd =  read.csv("//bioark/HurlbertLab/GIS/NBCD/data/nbcd_processed.csv", header = TRUE)

#ndvi_nbcd <- ndvi %>%
#  left_join(nbcd)
#write.csv(ndvi_nbcd, "data/ndvi_nbcd.csv", row.names = F)

# NLCD data #
#nlcd <- read.csv("\\\\Bioark.bio.unc.edu\\HurlbertLab\\DiCecco\\data\\fragmentation_indices_nlcd_simplified.csv")

#newcode <- data.frame(code = seq(1,9), 
#                      legend = c("Open water", "Urban", "Barren", "Forest", "Shrubland", 
#                                 "Agricultural", "Grasslands", "Wetlands", "Perennial ice, snow"))

#nlcd2001 <- nlcd %>%
#  filter(year == 2001) %>%
#  left_join(newcode, by = c("class" = "code")) %>%
#  group_by(stateroute) %>%
#  filter(prop.landscape == max(prop.landscape)) %>%
#  dplyr::select(year, stateroute, class, total.area, prop.landscape, legend)

#write.csv(nlcd2001, "data/bbs_nlcd2001.csv", row.names = F)

ndvi_nbcd <- read.csv("data/ndvi_ncbd.csv", stringsAsFactors = F)
nlcd2001 <- read.csv("data/bbs_nlcd2001.csv", stringsAsFactors = F)

# BBS data
routes <- read.csv("\\\\Bioark.bio.unc.edu\\hurlbertlab\\Databases\\BBS\\2017\\bbs_routes_20170712.csv")
counts <- read.csv("\\\\Bioark.bio.unc.edu\\hurlbertlab\\Databases\\BBS\\2017\\bbs_counts_20170712.csv")
species <- read.csv("\\\\Bioark.bio.unc.edu\\hurlbertlab\\Databases\\BBS\\2017\\bbs_species_20170712.csv")
weather <- read.csv("\\\\Bioark.bio.unc.edu\\hurlbertlab\\Databases\\BBS\\2017\\bbs_weather_20170712.csv")

routes$stateroute <- routes$statenum*1000 + routes$route
weather$stateroute <-weather$statenum*1000 + weather$route
RT1 <- subset(weather, runtype == 1, select = c("stateroute", "year"))
RT1.routes <- merge(RT1, routes[ , c("statenum", "stateroute", "latitude", "longitude","bcr")], by = "stateroute", all.x = TRUE)
counts$stateroute <- counts$statenum*1000 + counts$route

# Diurnal land birds, no birds of prey
species_list <- species %>%  
  filter(aou > 2880) %>%
  filter(aou < 3650 | aou > 3810) %>%
  filter(aou < 3900 | aou > 3910) %>%
  filter(aou < 4160 | aou > 4210) %>%
  filter(aou != 7010) %>%
  filter(sporder != "Accipitriformes", sporder != "Falconiformes", sporder != "Anseriformes")

## Filter BBS data

# Remove routes that have > 50% agricultural land use
ag_routes <- nlcd2001 %>%
  filter(prop.landscape > 0.5, legend == "Agricultural")

# Filter out transient species at each route
occ_calc <- counts.subs %>% 
  dplyr::select(year, stateroute, aou) %>%
  dplyr::count(aou, stateroute) %>%
  filter(n > 1) 
# occ_calc$occ = occ_calc$n/5 # new occupancy values calculated

# final abun df: 1135 routes, 360 spp
counts.subs <- counts %>%
  inner_join(RT1.routes, by = c("statenum", "stateroute", "year")) %>% # Remove RT = 0 route-years
  filter(year >= 2000, year <= 2004) %>% # Five year period
  group_by(stateroute) %>%
  filter(n_distinct(year) == 5) %>% # Only routes with consecutive sampling during five year period
  filter(aou %in% species_list$aou) %>% # Only diurnal land bird species, no birds of prey
  filter(stateroute %in% ndvi_nbcd$stateroute) %>% # Routes we have NDVI/NBCD for
  filter(!(stateroute %in% ag_routes$stateroute)) %>% # Remove routes that are predominantly agricultural
  filter(aou %in% occ_calc$aou) # add a filter term to exclude everything in the occ_calc vector OR Inner join by st & aou

# bbs richness
bbs_rich <- counts.subs %>% 
  group_by(stateroute) %>%
  dplyr::summarise(spRich = n_distinct(aou))

final.counts <- left_join(counts.subs, bbs_rich, by = "stateroute")

#### Plots ####
# ndvi abun
env_bbs_abun = left_join(bbs_abun, ndvi_nbcd, by = "stateroute")
env_bbs_abun = na.omit(env_bbs_abun)
ggplot(env_bbs_abun, aes(x = ndvi.mean, y = log10(sum))) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("Mean NDVI")+ ylab("log(Abundance)")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) 
ggsave("Figures/abun_ndvi.png", height = 8, width = 12)

# ndvi rich
env_bbs_rich = left_join(bbs_rich, ndvi_nbcd, by = "stateroute")
env_bbs_rich = na.omit(env_bbs_rich)
ggplot(env_bbs_rich, aes(x = ndvi.mean, y = log10(sprich))) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("Mean NDVI")+ ylab("log(Species Richness)")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) +ylim(0,4)
ggsave("Figures/rich_ndvi.png", height = 8, width = 12)


# nbcd abun
ggplot(env_bbs_abun, aes(x = nbcd.mean, y = log10(sum))) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("Mean NBCD")+ ylab("log(Abundance)")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) 
ggsave("C:/Git/mih-vegetation/Figures/abun_nbcd.png", height = 8, width = 12)

# nbcd rich
ggplot(env_bbs_rich, aes(x = nbcd.mean, y = log10(sprich))) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("Mean NBCD")+ ylab("log(Species Richness)")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) 
ggsave("C:/Git/mih-vegetation/Figures/rich_nbcd.png", height = 8, width = 12)
