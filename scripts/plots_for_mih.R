### MIH code ####
# want to make 4 plots

# NDVI:abun, NDVI:occ
# EVH:abun, EVH:occ
# for specialists/generalists and core-transient split out

library(ggplot2)
library(dplyr)
library(tidyr)
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
bbs_sub1$trans = "core"
bbs_sub1$trans[bbs_sub1$occ <= 0.34] = "trans"

landbirds$rich = 1
bbs_rich = landbirds %>%
 # filter(year > 1994, year < 2011, stateroute %in% good_rtes$stateroute) %>% 
  group_by(stateroute) %>%
  dplyr::count(rich) 
bbs_rich$spRich = bbs_rich$n

bbs_abun = landbirds %>% 
  filter(year > 1994, year < 2011, stateroute %in% good_rtes$stateroute) %>% 
  dplyr::select(year, stateroute, aou, speciestotal) %>%
  group_by(stateroute) %>%
  dplyr::summarise(sum = sum(speciestotal)) 

# ndvi data #
gimms_ndvi = read.csv("data/gimms_ndvi_bbs_data.csv", header = TRUE)
gimms_agg = gimms_ndvi %>% filter(month == c("may", "jun", "jul")) %>% 
  group_by(site_id)  %>%  dplyr::summarise(ndvi.mean=mean(ndvi))
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

env_bbs_notrans = filter(env_bbs, trans == "core")

# left join to get abundance
env_bbs_abun = left_join(bbs_abun, ndvi_nbcd, by = "stateroute")
env_bbs_abun = na.omit(env_bbs_abun)

# left join to get richness
env_bbs_rich = left_join(bbs_rich, ndvi_nbcd, by = "stateroute")
env_bbs_rich = na.omit(env_bbs_rich)

ndvi_range = c()
sp_list = unique(env_bbs_notrans$aou)
for(i in sp_list){
  sp = filter(env_bbs_notrans, aou == i)
  ndvi = range(sp$ndvi.mean)
  ndvi_range = rbind(ndvi_range, c(i, ndvi))
}
ndvi_range = data.frame(ndvi_range)
names(ndvi_range) = c("AOU", "NDVI.min", "NDVI.max")
write.csv(ndvi_range, "data/ndvi_range.csv", row.names = FALSE)  
  
#### plotting ####
ndvi_range$range = ndvi_range$NDVI.max - ndvi_range$NDVI.min
# zero = 1 occurrence
ndvi_range$AOU = as.factor(ndvi_range$AOU)
ndvi_plot = filter(ndvi_range, range > 0)
# NDVI plot
ggplot(ndvi_plot, aes(x = reorder(AOU, - range), y = range)) + geom_errorbar(width = 0, size = 1, aes(ymin= ndvi_plot$NDVI.min, ymax=ndvi_plot$NDVI.max)) +theme_classic()+ theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("AOU")+ ylab("NDVI Range")+ theme(axis.text.x=element_text(size = 20, angle = 90),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) 
ggsave("Figures/ndvi_range.pdf", height = 32, width = 42)
 
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


#### From CT Figure 5b ####
library(plyr)

bbs_abun_occ = landbirds %>% 
  filter(year > 1994, year < 2011, stateroute %in% good_rtes$stateroute) %>% 
  dplyr::select(year, stateroute, aou, speciestotal)
subsettedData1 = subset(bbs_abun_occ, speciestotal > 0)
spTime = ddply(subsettedData1, .(stateroute, aou), summarize, 
               spTime = length(unique(year)))
siteTime = ddply(subsettedData1, .(stateroute), summarize, 
                 siteTime = length(unique(year)))
spSiteTime = merge(spTime, siteTime)
propOcc = data.frame(site = spSiteTime$stateroute, 
                     species = spSiteTime$aou,
                     propOcc = spSiteTime$spTime/spSiteTime$siteTime)

rich_notrans = propOcc %>% filter(propOcc > 1/3) %>% dplyr::count(site)

bbsocc_rich = left_join(propOcc, bbs_rich, by = c("site" = "stateroute"))
bbsocc_rich = bbsocc_rich[ , !names(bbsocc_rich) %in% c("n")] 
bbsocc_rich = left_join(bbsocc_rich, rich_notrans)
bbsocc_rich$spRichnotrans = bbsocc_rich$n
bbsocc_rich = bbsocc_rich[ , !names(bbsocc_rich) %in% c("n")] 

bbs_env = left_join(bbsocc_rich, ndvi_nbcd, c("site" = "stateroute"))

# cor test not really working - need for loop?
cor.test(bbs_env$spRich, bbs_env$ndvi.mean)
bar1 = cor.test(bbs_env$spRich, bbs_env$ndvi.mean)$estimate
CI1lower =  cor.test(bbs_env$spRich, bbs_env$ndvi.mean)$conf.int[1]
CI1upper = cor.test(bbs_env$spRich, bbs_env$ndvi.mean)$conf.int[2]
bar3 = cor.test(bbs_env$spRich, bbs_env$nbcd.mean)$estimate
CI3lower = cor.test(bbs_env$spRich, bbs_env$nbcd.mean)$conf.int[1]
CI3upper =  cor.test(bbs_env$spRich, bbs_env$nbcd.mean)$conf.int[2]

bar2 = cor.test(bbs_env$spRichnotrans, bbs_env$ndvi.mean)$estimate
CI2lower = cor.test(bbs_env$spRichnotrans, bbs_env$ndvi.mean)$conf.int[1]
CI2upper =   cor.test(bbs_env$spRichnotrans, bbs_env$ndvi.meani)$conf.int[2]
bar4 = cor.test(bbs_env$spRichnotrans, bbs_env$nbcd.mean)$estimate
CI4lower =  cor.test(bbs_env$spRichnotrans, bbs_env$nbcd.mean)$conf.int[1]
CI4upper =  cor.test(bbs_env$spRichnotrans, bbs_env$nbcd.mean)$conf.int[2]

bar5 = cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$ndvi.mean)$estimate
CI5lower = cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$ndvi.mean)$conf.int[1]
CI5upper =  cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$ndvi.meani)$conf.int[2]
bar6 = cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$nbcd.mean)$estimate
CI6lower = cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$nbcd.mean)$conf.int[1]
CI6upper =  cor.test(bbs_env$spRich-bbs_env$spRichnotrans, bbs_env$nbcd.mean)$conf.int[2]

corr_res <- data.frame(All = c(bar1, bar3), Ntrans = c(bar2, bar4), Trans = c(bar5, bar6)) 
corr_res$env = c("NDVI", "NBCD")
corr_res_long = gather(corr_res, "class","value", c(All:Trans))
corr_res_long$CIlower = c(CI1lower,CI3lower,CI2lower,CI4lower, CI5lower, CI6lower)
corr_res_long$CIupper = c(CI1upper,CI3upper,CI2upper,CI4upper, CI5upper, CI6upper)
corr_res_long$env = factor(corr_res_long$env, levels = c("NDVI", "NBCD"), ordered = TRUE)

corr_NDVI = filter(corr_res_long, env == "NDVI")
corr_nbcd = filter(corr_res_long, env == "NBCD")
colscale = c("dark orange2","yellow","#c51b8a")
limits = aes(ymax = corr_res_long$CIupper, ymin=corr_res_long$CIlower)
# no variation - add in CIS?
l = ggplot(data=corr_res_long, aes(factor(env), value, fill = class, alpha = 0.7))+ geom_bar(width = 0.8, position = position_dodge(width = 0.9), stat="identity")+ scale_fill_manual(values = c("All" = "dark orange2","Trans" = "#c51b8a","Ntrans" = "yellow"), labels = c("All species","Excluding transients", "Transients only"))+ geom_bar(data=corr_res_long, aes(factor(env), value, fill = class), width = 0.8, position = position_dodge(width = 0.9), stat="identity")+ geom_errorbar(aes(ymin = corr_res_long$CIlower, ymax = corr_res_long$CIupper), width =.1, position = position_dodge(.9))+ theme_classic() + theme(axis.text.x=element_text(size=46, color = "black", vjust = 5), axis.ticks.x=element_blank(),axis.text.y=element_text(size=30, color = "black"),axis.title.x=element_text(size=46, color = "black"),axis.title.y=element_text(size=46,angle=90,vjust = 2))+ xlab(NULL) + ylab(expression(paste(italic("r")))) + scale_y_continuous(breaks=c(-0.5,-0.3,-0.1,.1,.3,.5))+ guides(fill=guide_legend(title=NULL)) + theme(legend.text = element_text(size = 38), legend.title = element_blank(), legend.key.height=unit(3,"line")) + geom_hline(yintercept=0, lty = "dashed", lwd = 1.25) + theme(plot.margin=unit(c(1,1,2,1),"cm"))
ggsave("Figures/richenv.png", height = 8, width = 12)



#### sum abun instead of rich ####
bbsocc_abun = left_join(propOcc, bbs_abun, by = c("site" = "stateroute"))
bbsocc_abun = bbsocc_abun[ , !names(bbsocc_abun) %in% c("n")]
abun_notrans = bbsocc_abun %>% filter(propOcc > 1/3) %>% dplyr::count(sum)
bbsocc_abun = left_join(bbsocc_abun, abun_notrans)
bbsocc_abun$abunnotrans = bbsocc_abun$n
bbsocc_abun = bbsocc_abun[ , !names(bbsocc_abun) %in% c("n")] 

bbs_env = left_join(bbsocc_abun, ndvi_nbcd, c("site" = "stateroute"))

# cor test not really working - need for loop?
cor.test(bbs_env$sum, bbs_env$ndvi.mean)
bar1 = cor.test(bbs_env$sum, bbs_env$ndvi.mean)$estimate
CI1lower =  cor.test(bbs_env$sum, bbs_env$ndvi.mean)$conf.int[1]
CI1upper = cor.test(bbs_env$sum, bbs_env$ndvi.mean)$conf.int[2]
bar3 = cor.test(bbs_env$sum, bbs_env$nbcd.mean)$estimate
CI3lower = cor.test(bbs_env$sum, bbs_env$nbcd.mean)$conf.int[1]
CI3upper =  cor.test(bbs_env$sum, bbs_env$nbcd.mean)$conf.int[2]

bar2 = cor.test(bbs_env$abunnotrans, bbs_env$ndvi.mean)$estimate
CI2lower = cor.test(bbs_env$abunnotrans, bbs_env$ndvi.mean)$conf.int[1]
CI2upper =   cor.test(bbs_env$abunnotrans, bbs_env$ndvi.meani)$conf.int[2]
bar4 = cor.test(bbs_env$abunnotrans, bbs_env$nbcd.mean)$estimate
CI4lower =  cor.test(bbs_env$abunnotrans, bbs_env$nbcd.mean)$conf.int[1]
CI4upper =  cor.test(bbs_env$abunnotrans, bbs_env$nbcd.mean)$conf.int[2]

bar5 = cor.test(bbs_env$sum-bbs_env$abunnotrans, bbs_env$ndvi.mean)$estimate
CI5lower = cor.test(bbs_env$sum-bbs_env$abunnotrans, bbs_env$ndvi.mean)$conf.int[1]
CI5upper =  cor.test(bbs_env$sum-bbs_env$abunnotrans, bbs_env$ndvi.meani)$conf.int[2]
bar6 = cor.test(bbs_env$sum-bbs_env$abunnotrans, bbs_env$nbcd.mean)$estimate
CI6lower = cor.test(bbs_env$sum-bbs_env$abunnotrans, bbs_env$nbcd.mean)$conf.int[1]
CI6upper =  cor.test(bbs_env$sum-bbs_env$abunnotrans, bbs_env$nbcd.mean)$conf.int[2]

corr_res <- data.frame(All = c(bar1, bar3), Ntrans = c(bar2, bar4), Trans = c(bar5, bar6)) 
corr_res$env = c("NDVI", "NBCD")
corr_res_long = gather(corr_res, "class","value", c(All:Trans))
corr_res_long$CIlower = c(CI1lower,CI3lower,CI2lower,CI4lower, CI5lower, CI6lower)
corr_res_long$CIupper = c(CI1upper,CI3upper,CI2upper,CI4upper, CI5upper, CI6upper)
corr_res_long$env = factor(corr_res_long$env, levels = c("NDVI", "NBCD"), ordered = TRUE)

corr_NDVI = filter(corr_res_long, env == "NDVI")
corr_nbcd = filter(corr_res_long, env == "NBCD")
colscale = c("dark orange2","yellow","#c51b8a")
limits = aes(ymax = corr_res_long$CIupper, ymin=corr_res_long$CIlower)
# no variation - add in CIS?
l = ggplot(data=corr_res_long, aes(factor(env), value, fill = class, alpha = 0.7))+ geom_bar(width = 0.8, position = position_dodge(width = 0.9), stat="identity")+ scale_fill_manual(values = c("All" = "dark orange2","Trans" = "#c51b8a","Ntrans" = "yellow"), labels = c("All species","Excluding transients", "Transients only"))+ geom_bar(data=corr_res_long, aes(factor(env), value, fill = class), width = 0.8, position = position_dodge(width = 0.9), stat="identity")+ geom_errorbar(aes(ymin = corr_res_long$CIlower, ymax = corr_res_long$CIupper), width =.1, position = position_dodge(.9))+ theme_classic() + theme(axis.text.x=element_text(size=46, color = "black", vjust = 5), axis.ticks.x=element_blank(),axis.text.y=element_text(size=30, color = "black"),axis.title.x=element_text(size=46, color = "black"),axis.title.y=element_text(size=46,angle=90,vjust = 2))+ xlab(NULL) + ylab(expression(paste(italic("r")))) + scale_y_continuous(breaks=c(-0.5,-0.3,-0.1,.1,.3,.5))+ guides(fill=guide_legend(title=NULL)) + theme(legend.text = element_text(size = 38), legend.title = element_blank(), legend.key.height=unit(3,"line")) + geom_hline(yintercept=0, lty = "dashed", lwd = 1.25) + theme(plot.margin=unit(c(1,1,2,1),"cm"))
ggsave("Figures/abunenv.png", height = 8, width = 12)