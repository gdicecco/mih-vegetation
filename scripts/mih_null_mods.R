library(ggplot2)
library(dplyr)
library(tidyr)
library(rdataretriever)

troph_guild <- read.csv("Z:/Databases/Trophic Guilds/Troph_guilds.csv", header = TRUE)
bbs_sub1 <- read.csv("data/bbs_sub1.csv", header = TRUE)
gimms_ndvi = read.csv("data/gimms_ndvi_bbs_data.csv", header = TRUE)
tax_code <- read.csv("data/Bird_Taxonomy.csv", header = TRUE) %>%
  dplyr::select(AOU_OUT, CRC_SCI_NAME) %>%
  unique() %>% na.omit()

tax_code1 = tax_code[-grep("/", tax_code$CRC_SCI_NAME),] 
tax_code2 = tax_code1[-grep("sp.", tax_code1$CRC_SCI_NAME),]


troph_AOU <- left_join(troph_guild, tax_code2, by = c("Species" = "CRC_SCI_NAME"))
troph_AOU$Species = gsub('Dendroica','Setophaga', troph_AOU$Species)

bbs_sub2 <- filter(bbs_sub1, aou %in% tax_code2$AOU_OUT)
bbs_troph <- left_join(bbs_sub2, troph_AOU, by = c("aou" = "AOU_OUT"))

test <- filter(bbs_troph, is.na(Species) == TRUE)
tcode <- left_join(test, tax_code2, by = c("aou"="AOU_OUT")) %>%
  select(aou, Species)
sp_nocodes <- unique(tcode)
# tax_code$CRC_SCI_NAME[tax_code$CRC_SCI_NAME == "Dendragapus obscurus/fuliginosus"] <- "Dendragapus obscurus"


gimms_agg = gimms_ndvi %>% filter(month == c("may", "jun", "jul")) %>% 
  group_by(site_id)  %>%  dplyr::summarise(ndvi.mean=mean(ndvi))
gimms_agg$stateroute = gimms_agg$site_id
ndvi = gimms_agg[,c("stateroute", "ndvi.mean")]

# left join to get temporal occupancy
env_bbs = left_join(bbs_troph, ndvi, by = "stateroute") %>%
  left_join(., troph_guild) %>% 
  na.omit(.) 

null_pool1 <- data.frame(env_bbs$aou, env_bbs$Trophic.guild) %>%
  distinct()

#### null model ####
null_output = c()
for(route in env_bbs$stateroute){
  subdata = filter(env_bbs, stateroute == route)
  if(length(unique(subdata$stateroute)) > 1){
    print(subdata$ndvi.mean)
  }
    for(r in 1:100){
      ndvi = unique(subdata$ndvi.mean)
      FGobs = length(unique((subdata$Trophic.guild)))
      Sobs = length(unique((subdata$aou)))
      Fnull = sample_n(null_pool1, Sobs, replace = FALSE) 
      FGNull = length(unique((Fnull$env_bbs.Trophic.guild)))
      null_output = rbind(null_output, c(r, ndvi, Sobs, FGobs, FGNull))      
      }
    } # end r loop

null_output = data.frame(null_output)
colnames(null_output) = c("iteration", "ndvi.mean","Sobs", "FGObs", "FGNull")
# write.csv(null_output, "Data/null_output.csv", row.names = FALSE)

# aggregate by ndvi mean
null_output_agg <- null_output %>% group_by(ndvi.mean) %>%
  summarise(FGObs = mean(FGObs), mean_FGNull = mean(FGNull),Sobs = mean(Sobs))
mod <- lm(FGObs ~ mean_FGNull, data = null_output_agg)

ggplot(null_output_agg, aes(x = FGObs, y = mean_FGNull)) + theme_classic() + geom_point(aes(col = ndvi.mean), size = 2) + geom_abline(intercept = 0, slope = 1, col = "black", lwd = 1.5) + xlab("Number of Guilds Observed")+ ylab("Number of Guilds Null") + theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30))


null_long <- gather(null_output_agg, "Troph", "Num", FGObs:mean_FGNull)
ggplot(null_long, aes(x = ndvi.mean, y = Num)) + theme_classic() + geom_point(aes(col = Troph), size = 2) + geom_abline() + xlab("Mean NDVI")+ ylab("Number of Guilds") + theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30))
ggsave("FG_ndvi.pdf")
# sample from pool of spp that occur at a given ndvi band - pool in groups of 0.05 (20 groups)
# what are all the species observed in that bin
#### null model ####
init.time = Sys.Date()
env_bbs$bin <- binsize*floor(env_bbs$ndvi.mean/binsize) + binsize/2
binsize <- 0.05

null_output_bins = c() 
for(route in env_bbs$stateroute){
  subdata = filter(env_bbs, stateroute == route)
  if(length(unique(subdata$stateroute)) > 1){
    print(subdata$ndvi.mean)
    }
  null_pool2 = filter(env_bbs, subdata$bin == bin)
  for(r in 1:100){
    ndvi = unique(subdata$ndvi.mean)
    print(paste(ndvi, r, Sys.Date()))
    FGobs = length(unique((subdata$Trophic.guild)))
    Sobs = length(unique((subdata$aou)))
    Fnull = sample_n(null_pool2, Sobs, replace = FALSE) 
    FGNull = length(unique((Fnull$env_bbs.Trophic.guild)))
    null_output_bins = rbind(null_output_bins, c(r, ndvi, FGobs, Sobs, FGNull))      
  }
} # end r loop

null_output_bins = data.frame(null_output_bins)
colnames(null_output_bins) = c("iteration", "ndvi.mean", "FGObs", "Sobs","FGNull")
# write.csv(null_output_bins, "Data/null_output_bins.csv", row.names = FALSE)

# aggregate by ndvi mean
null_output_bins_agg <- null_output_bins %>% group_by(ndvi.mean) %>%
  summarise(Sobs = mean(Sobs), FGObs = mean(FGObs), mean_FGNull = mean(FGNull))
mod <- lm(FGObs ~ mean_FGNull, data = null_output_bins_agg)


ggplot(null_output_bins_agg, aes(x = FGObs, y = mean_FGNull)) + theme_classic() + geom_point(aes(col = ndvi.mean), size = 2) + geom_abline(intercept = 0, slope = 1, col = "black", lwd = 1.5) + xlab("Number of Guilds Observed")+ ylab("Number of Guilds Null") + theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30))


null_long_bins <- gather(null_output_bins_agg, "Troph", "Num", FGObs:mean_FGNull)
ggplot(null_long_bins, aes(x = ndvi.mean, y = Num)) + theme_classic() + geom_point(aes(col = Troph), size = 2) + geom_abline() + xlab("Mean NDVI")+ ylab("Number of Guilds") + theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30))
ggsave("FG_ndvi_binned.pdf")



