### MIH code ####
# want to make 4 plots

# NDVI:abun, NDVI:occ
# EVH:abun, EVH:occ
# for specialists/generalists and core-transient split out

library(ggplot2)
library(dplyr)
library(rdataretriever)
##### ecoretriever to download bbs data and derive occupancy values #####
# bbs_eco = rdataretriever::fetch("breed-bird-survey")
# Years = (bbs_eco$breed_bird_survey_counts)
# Years$stateroute = Years$statenum*1000 + Years$route

# filter to land birds
# landbirds <- Years %>%
#  filter(aou > 2880) %>%
#  filter(aou < 3650 | aou > 3810) %>%
#  filter(aou < 3900 | aou > 3910) %>%
#  filter(aou < 4160 | aou > 4210) %>%
#  filter(aou != 7010)

landbirds = read.csv("//bioark/HurlbertLab/Snell/MIH/bbs_data.csv", header = TRUE)

# Get subset of stateroutes that have been surveyed every year from 2001-2015
good_rtes = landbirds %>% 
  dplyr::filter(year > 1994, year < 2011) %>% 
  dplyr::select(year, stateroute) %>%
  unique() %>%    
  dplyr::count(stateroute) %>% 
  filter(n == 15) # have to stay at 15 to keep # of years consistent

# Calculate occupancy for all species at subset of stateroutes above
bbs_sub1 = landbirds %>% 
  filter(year > 1994, year < 2011, stateroute %in% good_rtes$stateroute) %>% 
  dplyr::select(year, stateroute, aou) %>%
  dplyr::count(aou, stateroute) %>%
  filter(n <= 15) 

bbs_sub1$occ = bbs_sub1$n/15 # new occupancy values calculated

landbirds$rich = 1
bbs_rich = landbirds %>%
  filter(year > 1994, year < 2011, stateroute %in% good_rtes$stateroute) %>% 
  group_by(stateroute) %>%
  count(rich) 
bbs_rich$sprich = bbs_rich$n

bbs_abun = landbirds %>% 
  filter(year > 1994, year < 2011, stateroute %in% good_rtes$stateroute) %>% 
  dplyr::select(year, stateroute, aou, speciestotal) %>%
  group_by(stateroute, aou) %>%
  summarise(sum = sum(speciestotal)) 

# ndvi data #
gimms_ndvi = read.csv("data/gimms_ndvi_bbs_data.csv", header = TRUE)
gimms_agg = gimms_ndvi %>% filter(month == c("may", "jun", "jul")) %>% 
  group_by(site_id)  %>%  summarise(ndvi.mean=mean(ndvi))
gimms_agg$stateroute = gimms_agg$site_id
ndvi = gimms_agg[,c("stateroute", "ndvi.mean")]

# canopy height data #
nbcd =  read.csv("//bioark/HurlbertLab/GIS/NBCD/data/nbcd_processed.csv", header = TRUE)
nbcd_bbs = left_join(bbs_sub1, nbcd, by = "stateroute")
nbcd_bbs = na.omit(nbcd_bbs)

# join ndvi and nbcd #
ndvi_nbcd = inner_join(ndvi, nbcd, by = "stateroute")

# left join to get temporal occupancy
env_bbs = left_join(bbs_sub1, ndvi_nbcd, by = "stateroute")
env_bbs = na.omit(env_bbs)

# left join to get abundance
env_bbs_abun = left_join(bbs_abun, ndvi_nbcd, by = "stateroute")
env_bbs_abun = na.omit(env_bbs_abun)

# left join to get richness
env_bbs_rich = left_join(bbs_rich, ndvi_nbcd, by = "stateroute")
env_bbs_rich = na.omit(env_bbs_rich)


# occ
ggplot(env_bbs, aes(x = ndvi.mean, y = occ)) + geom_point() + geom_smooth(method = "lm")+theme_classic()+ theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("Mean NDVI")+ ylab("Occupancy") + xlim(0,1) + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) 
ggsave("Figures/occ_ndvi.png", height = 8, width = 12)

# abun
env_bbs_abun = left_join(bbs_abun, ndvi_nbcd, by = "stateroute")
env_bbs_abun = na.omit(env_bbs_abun)
ggplot(env_bbs_abun, aes(x = ndvi.mean, y = log10(sum))) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("Mean NDVI")+ ylab("log(Abundance)")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) 
ggsave("Figures/abun_ndvi.png", height = 8, width = 12)

# rich
env_bbs_rich = left_join(bbs_rich, ndvi_nbcd, by = "stateroute")
env_bbs_rich = na.omit(env_bbs_rich)
ggplot(env_bbs_rich, aes(x = ndvi.mean, y = log10(sprich))) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("Mean NDVI")+ ylab("log(Species Richness)")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) +ylim(0,4)
ggsave("Figures/rich_ndvi.png", height = 8, width = 12)

# occ
ggplot(env_bbs, aes(x = nbcd.mean, y = occ)) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("Mean NBCD")+ ylab("Occupancy")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) 
ggsave("C:/Git/mih-vegetation/Figures/occ_nbcd.png", height = 8, width = 12)

# abun
ggplot(env_bbs_abun, aes(x = nbcd.mean, y = log10(sum))) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("Mean NBCD")+ ylab("log(Abundance)")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) 
ggsave("C:/Git/mih-vegetation/Figures/abun_nbcd.png", height = 8, width = 12)

# rich
ggplot(env_bbs_rich, aes(x = nbcd.mean, y = log10(sprich))) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("Mean NBCD")+ ylab("log(Species Richness)")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) 
ggsave("C:/Git/mih-vegetation/Figures/rich_nbcd.png", height = 8, width = 12)


##### models for varpar #####
occ_nbcd <- lm(env_bbs$occ ~  env_bbs$nbcd.mean) 
# z scores separated out for env effects - abundance
occ_ndvi = lm(env_bbs$occ ~  env_bbs$ndvi.mean) 
# z scores separated out for env effects - abundance
occ_both = lm(env_bbs$occ ~  env_bbs$nbcd.mean + env_bbs$ndvi.mean) 

NDVI = summary(occ_both)$r.squared - summary(occ_nbcd)$r.squared #ndvi only
NBCD = summary(occ_both)$r.squared - summary(occ_ndvi)$r.squared #nbcd only
SHARED = summary(occ_nbcd)$r.squared - NBCD #shared variance
NONE = 1 - summary(occ_both)$r.squared # neither variance

abun_nbcd <- lm(env_bbs_abun$sum ~  env_bbs_abun$nbcd.mean) 
# z scores separated out for env effects - abundance
abun_ndvi = lm(env_bbs_abun$sum ~  env_bbs_abun$ndvi.mean) 
# z scores separated out for env effects - abundance
abun_both = lm(env_bbs_abun$sum ~  env_bbs_abun$nbcd.mean + env_bbs_abun$ndvi.mean) 

NDVI = summary(abun_both)$r.squared - summary(abun_nbcd)$r.squared #ndvi only
NBCD = summary(abun_both)$r.squared - summary(abun_ndvi)$r.squared #nbcd only
SHARED = summary(abun_nbcd)$r.squared - NBCD #shared variance
NONE = 1 - summary(abun_both)$r.squared # neither variance


rich_nbcd <- lm(env_bbs_rich$sprich ~  env_bbs_rich$nbcd.mean) 
# z scores separated out for env effects - richdance
rich_ndvi = lm(env_bbs_rich$sprich ~  env_bbs_rich$ndvi.mean) 
# z scores separated out for env effects - richdance
rich_both = lm(env_bbs_rich$sprich ~  env_bbs_rich$nbcd.mean + env_bbs_rich$ndvi.mean) 

NDVI = summary(rich_both)$r.squared - summary(rich_nbcd)$r.squared #ndvi only
NBCD = summary(rich_both)$r.squared - summary(rich_ndvi)$r.squared #nbcd only
SHARED = summary(rich_nbcd)$r.squared - NBCD #shared variance
NONE = 1 - summary(rich_both)$r.squared # neither variance


# routes_nbcd = routes[routes@data$stateroute %in% nbcd_bbs$stateroute,]
# plot(routes_nbcd)

