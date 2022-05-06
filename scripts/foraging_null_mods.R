library(ggplot2)
library(dplyr)
library(tidyr)

troph_guild <- read.csv("data/Troph_guilds.csv", header = TRUE)
bbs_sub1 <- read.csv("data/final_bbs_subset.csv", header = TRUE)
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
for(rt in unique(env_bbs$stateroute)) {
  init.time <- Sys.time()
  subdata = filter(env_bbs, stateroute == rt)

    for(r in 1:999){
      stateroute <- unique(subdata$stateroute)
      ndvi = unique(subdata$ndvi.mean)
      FGobs = length(unique((subdata$Trophic.guild)))
      Sobs = length(unique((subdata$aou)))
      Fnull = sample_n(null_pool1, Sobs, replace = FALSE) 
      FGNull = length(unique((Fnull$env_bbs.Trophic.guild)))
      null_output = rbind(null_output, c(stateroute, r, ndvi, Sobs, FGobs, FGNull))      
    }
  
  end.time <- Sys.time()
  print(paste(rt, "-", end.time - init.time, "seconds elapsed"))
  
    } # end r loop

null_output = data.frame(null_output)
colnames(null_output) = c("stateroute", "iteration", "ndvi.mean","Sobs", "FGObs", "FGNull")
# write.csv(null_output, "data/bbs_null_output.csv", row.names = FALSE)

# aggregate by ndvi mean
null_output_agg <- null_output %>% group_by(ndvi.mean) %>%
  summarise(FGObs = mean(FGObs), mean_FGNull = mean(FGNull),Sobs = mean(Sobs))
mod <- lm(FGObs ~ mean_FGNull, data = null_output_agg)

null_output_agg$z_score <- (null_output_agg$FGObs - null_output_agg$mean_FGNull)/sd(null_output_agg$mean_FGNull)

hist(null_output_agg$z_score)

ggplot(null_output_agg, aes(x = ndvi.mean, y = z_score)) + 
  theme_classic() + geom_point(aes(col = FGObs), size = 2) + 
  geom_abline(intercept = 0, slope = 0, col = "black", lwd = 1.5, lty = "dashed") + 
  geom_smooth(method = "lm", se = F, color = "red") +xlab("Mean NDVI")+ 
  ylab("Number of Guilds z-score") + 
  theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30))
# ggsave("Figures/FG_ndvi_z.pdf")

null_long <- gather(null_output_agg, "Troph", "Num", FGObs:mean_FGNull)
ggplot(null_long, aes(x = ndvi.mean, y = Num)) + theme_classic() + geom_point(aes(col = Troph, alpha = 0.5), size = 2) + geom_abline() + xlab("Mean NDVI")+ ylab("Number of Guilds") + theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30))
# ggsave("FG_ndvi.pdf")

# sample from pool of spp that occur at a given ndvi band - pool in groups of 0.05 (20 groups)
# what are all the species observed in that bin
#### null model ####

binsize <- 0.05
env_bbs$bin <- binsize*floor(env_bbs$ndvi.mean/binsize) + binsize/2

null_output_bins = c() 
for(rt in unique(env_bbs$stateroute)) {
  
  init.time <- Sys.time()
  
  subdata = filter(env_bbs, stateroute == rt)

  null_pool2 = filter(env_bbs, bin == unique(subdata$bin)) %>%
    filter(!(is.na(Trophic.guild))) %>%
    distinct(aou, Trophic.guild)
  
  for(r in 1:999){
    stateroute <- unique(subdata$stateroute)
    ndvi = unique(subdata$ndvi.mean)
    FGobs = length(unique((subdata$Trophic.guild)))
    Sobs = length(unique((subdata$aou)))
    Fnull = sample_n(null_pool2, Sobs, replace = FALSE) 
    FGNull = length(unique((Fnull$Trophic.guild)))
    null_output_bins = rbind(null_output_bins, c(stateroute, r, ndvi, FGobs, Sobs, FGNull))      
  }
  end.time <- Sys.time()
  print(paste(rt, "-", end.time - init.time, "seconds elapsed"))
} # end r loop

null_output_bins = data.frame(null_output_bins)
colnames(null_output_bins) = c("stateroute", "iteration", "ndvi.mean", "FGObs", "Sobs","FGNull")
# write.csv(null_output_bins, "data/bbs_null_output_bins.csv", row.names = FALSE)


# aggregate by ndvi mean
null_output_bins_agg <- null_output_bins %>% group_by(ndvi.mean) %>%
  summarise(Sobs = mean(Sobs), FGObs = mean(FGObs), mean_FGNull = mean(FGNull))
mod <- lm(FGObs ~ mean_FGNull, data = null_output_bins_agg)

null_output_bins_agg$z_score <- (null_output_bins_agg$FGObs - null_output_bins_agg$mean_FGNull)/sd(null_output_bins_agg$mean_FGNull)

hist(null_output_bins_agg$z_score)

##### start her for fig plots ####
null_output <- read.csv("data/bbs_null_output.csv", header = TRUE)
null_output_bins <- read.csv("data/bbs_null_output_bins.csv", header = TRUE)

null_output_agg <- null_output %>%
  group_by(stateroute) %>%
  summarize(FGnull_mean = mean(FGNull),
            FGnull_sd = sd(FGNull),
            FGObs = mean(FGObs),
            ndvi.mean = mean(ndvi.mean),
            Sobs = mean(Sobs),
            FGnull_pct = sum(FGObs < FGNull)/1000) %>%
  mutate(FG_z = (FGObs - FGnull_mean)/FGnull_sd)

null_output_bins_agg <- null_output_bins %>%
  group_by(stateroute) %>%
  summarize(FGnull_mean = mean(FGNull),
            FGnull_sd = sd(FGNull),
            FGObs = mean(FGObs),
            ndvi.mean = mean(ndvi.mean),
            Sobs = mean(Sobs),
            FGnull_pct = sum(FGObs < FGNull)/1000) %>%
  mutate(FG_z = (FGObs - FGnull_mean)/FGnull_sd)

summary(lm(FG_z ~ ndvi.mean, data = null_output_bins_agg))

ggplot(null_output_bins_agg, aes(x = FGObs, y = FGnull_mean)) + 
  theme_classic() + geom_point(aes(col = ndvi.mean), size = 2) + 
  geom_abline(intercept = 0, slope = 1, col = "black", lwd = 1.5) + 
  xlab("Number of Guilds Observed")+ ylab("Number of Guilds Null") + 
  theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30))


null_long_bins <- gather(null_output_bins_agg, "Troph", "Num", FGObs:FGnull_mean) %>%
ggplot(., aes(x = ndvi.mean, y = Num)) + theme_classic() + geom_point(aes(col = Troph), size = 2) + geom_abline() + xlab("Mean NDVI")+ ylab("Number of Guilds") + theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30))
# ggsave("FG_ndvi_binned.pdf")

ggplot(null_output_agg, aes(x = ndvi.mean, y = FGnull_pct, col = FGObs)) +
  geom_point() + theme_classic() +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_smooth(method = "lm", se = F, col = "black") +
  labs(x = "NDVI", y = "Proportion sims obs < null", col = "Obs. Foraging Guilds") +
  ggtitle("BBS, no bins")
# ggsave("Figures/BBS_null_mod_percentile.pdf", units = "in", width = 8, height = 6)

ggplot(null_output_bins_agg, aes(x = ndvi.mean, y = FGnull_pct, col = FGObs)) +
  geom_point() + theme_classic() + 
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_smooth(method = "lm", se = F, col = "black") +
  labs(x = "NDVI", y = "Proportion sims obs < null", col = "Obs. Foraging Guilds") +
  ggtitle("BBS, NDVI bins")
# ggsave("Figures/BBS_null_mod_bins_percentile.pdf", units = "in", width = 8, height = 6)



null_bbs_z <- ggplot(null_output_bins_agg, aes(x = ndvi.mean, y = FG_z)) + theme_classic() + 
  geom_point(aes(col = FGObs), size = 2) + 
  geom_abline(intercept = 0, slope = 0, col = "black", lwd = 2, lty = "dashed") + 
  labs(col = "Number of foraging guilds") +
  geom_smooth(method = "lm", se = F, color = "blue", lwd = 1.25) +xlab("Mean NDVI")+ ylab("Foraging guild z-score") + theme(axis.text.x=element_text(size = 30),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) +
  theme(axis.text.x=element_text(size = 28),axis.text.y=element_text(size=28)) +
  theme(axis.title.x=element_text(size = 32),axis.title.y=element_text(size=32, vjust = 2)) +
  theme(legend.text=element_text(size = 20, vjust = -1), legend.key.height=unit(2, "lines"),
        legend.key.width = unit(4, "line"), legend.position = "top", legend.title = element_text(size = 20)) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  scale_color_continuous(breaks = c(4,6,8,10,12,14), type = "viridis") 

# ggsave("Figures/FG_ndvi_binz.pdf")



null_output_bins <- read.csv("data/bbc_null_output_bins.csv")
null_output_bins_z <- null_output_bins %>%
  group_by(siteID) %>%
  summarize(FGnull_mean = mean(FGNull),
            FGnull_sd = sd(FGNull),
            FGObs = mean(FGObs),
            ndvi.mean = mean(ndvi.mean),
            Sobs = mean(Sobs),
            FGnull_pct = sum(FGObs < FGNull)/1000) %>%
  mutate(FG_z = (FGObs - FGnull_mean)/FGnull_sd)

summary(lm(FG_z ~ ndvi.mean, data = null_output_bins_z))

null_bbc_z <- ggplot(null_output_bins_z, aes(x = ndvi.mean, y = FG_z, col = FGObs)) +
  geom_point(size = 2) +
  theme_classic() + 
  geom_abline(intercept = 0, slope = 0, col = "black", lwd = 2, lty = "dashed") +
  # geom_smooth(method = "lm", se = F, col = "blue", lwd = 1.25) +
  labs(x = "Mean NDVI", y = "Foraging guild z-score", col = "Number of foraging guilds") +
  theme(axis.text.x=element_text(size = 28),axis.text.y=element_text(size=28)) +
  theme(axis.title.x=element_text(size = 32),axis.title.y=element_text(size=32, vjust = 2)) +
  theme(legend.text=element_text(size = 20, vjust = -1), legend.key.height=unit(2, "lines"),
       legend.key.width = unit(4, "line"), legend.position = "top", legend.title = element_text(size = 20)) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  scale_color_continuous(breaks = c(2,4,6,8,10,12,14), type = "viridis") 
# ggsave("Figures/BBC_null_mod_bins_z.pdf", units = "in", width = 8, height = 6)


foura <- plot_grid(null_bbs_z + theme(legend.position="top"),
          labels = c("A"),
          label_size = 28,
          hjust = -1.75)

fourb <- plot_grid(null_bbc_z + theme(legend.position="top"),
           labels = c("B"),
           label_size = 28,
           hjust = -1.75)

grid_effects <- plot_grid(foura,
                          fourb,
                          align = 'hv',
                          labels = c("BBS", "BBC"),
                          label_size = 28,
                          vjust = 7,
                          hjust = -1.75) 

ggsave("Figures/null_mod_plots.pdf", units = "in", width = 14, height = 8)
