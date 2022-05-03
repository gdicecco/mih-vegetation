### Dataset for species-energy questions
# BBS routes consecutively sampled between 2000-2004, removing transient species and routes that are mostly agricultural

library(tidyverse)
library(ggplot2)
library(cowplot)

### Read in data
theme_set(theme_classic(base_size = 30))
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
  filter(sporder != "Accipitriformes", 
         sporder != "Falconiformes", 
         sporder != "Anseriformes",
         sporder != "Cathartiformes")
#write.csv(species_list, "data/species_list.csv", row.names = F)

## Filter BBS data

# Remove routes that have > 50% agricultural land use
ag_routes <- nlcd2001 %>%
  filter(prop.landscape > 0.5, legend == "Agricultural")

counts.subs <- counts %>%
  inner_join(RT1.routes, by = c("statenum", "stateroute", "year")) %>% # Remove RT = 0 route-years
  filter(year >= 2000, year <= 2004) %>% # Five year period
  group_by(stateroute) %>%
  filter(n_distinct(year) == 5) %>% # Only routes with consecutive sampling during five year period
  filter(aou %in% species_list$aou) %>% # Only diurnal land bird species, no birds of prey
  filter(stateroute %in% ndvi_nbcd$stateroute) %>% # Routes we have NDVI/NBCD for
  filter(!(stateroute %in% ag_routes$stateroute))  # Remove routes that are predominantly agricultural

# Filter out transient species at each route
occ_calc <- counts.subs %>% 
  dplyr::select(year, stateroute, aou) %>%
  dplyr::count(aou, stateroute) %>%
  filter(n > 1) 
# occ_calc$occ = occ_calc$n/5 # new occupancy values calculated

# bbs richness
bbs_rich <- counts.subs %>% 
  group_by(stateroute) %>%
  dplyr::summarise(spRich = n_distinct(aou))

final.count.occ <- counts.subs %>% 
filter(aou %in% occ_calc$aou) # add a filter term to exclude everything in the occ_calc vector OR Inner join by st & aou

# final abun df: 1135 routes, 360 spp ***want sum across years?
final.counts <- left_join(final.count.occ, bbs_rich, by = "stateroute") 
#write.csv(final.counts, "data/final_bbs_subset.csv", row.names = F)

final.counts <- read.csv("data/final_bbs_subset.csv", stringsAsFactors = F)
bbs_rich <- final.counts %>% 
  group_by(stateroute) %>%
  dplyr::summarise(spRich = n_distinct(aou))


final.abun <- final.counts %>% 
  group_by(stateroute) %>%
  dplyr::summarise(sum = sum(speciestotal)) 

#### Plots ####

# ndvi abun
env_bbs_abun = left_join(final.abun, ndvi_nbcd, by = "stateroute")
ni_a = ggplot(env_bbs_abun, aes(x = ndvi.mean, y = log10(sum))) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28)) + xlab("Mean NDVI")+ ylab("log(Abundance)")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 25),axis.ticks=element_blank(), axis.text.y=element_text(size=25)) 
# ggsave("Figures/abun_ndvi.png", height = 8, width = 12)

# ndvi rich
env_bbs_rich = left_join(bbs_rich, ndvi_nbcd, by = "stateroute")
# write.csv(env_bbs_rich, "data/env_bbs_rich.csv", row.names = FALSE)
ni_r = ggplot(env_bbs_rich, aes(x = ndvi.mean, y = spRich)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + theme_classic() + theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28)) + xlab("Mean NDVI")+ ylab("Species Richness")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 25),axis.ticks=element_blank(), axis.text.y=element_text(size=25)) 
# ggsave("Figures/rich_ndvi.pdf", height = 8, width = 12)

# nbcd abun
nd_a = ggplot(env_bbs_abun, aes(x = nbcd.mean, y = log10(sum))) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28)) + xlab("Mean NBCD")+ ylab("log(Abundance)")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 25),axis.ticks=element_blank(), axis.text.y=element_text(size=25)) 
# ggsave("Figures/abun_nbcd.png", height = 8, width = 12)

# nbcd rich
nd_r = ggplot(env_bbs_rich, aes(x = nbcd.mean, y = log10(spRich))) + geom_point() + geom_smooth(method = "lm") + theme_classic() + theme(axis.title.x=element_text(size=28),axis.title.y=element_text(size=28)) + xlab("Mean NBCD")+ ylab("log(Richness)")  + geom_point(col = "black", shape=16, size = 2)+ theme(axis.text.x=element_text(size = 25),axis.ticks=element_blank(), axis.text.y=element_text(size=25)) 
# ggsave("Figures/rich_nbcd.png", height = 8, width = 12)

z <- plot_grid(ni_a + theme(legend.position="top"),
               ni_r + theme(legend.position="none"),
               nd_a + theme(legend.position="none"),
               nd_r + theme(legend.position="none"),
               nrow = 2,
               align = 'hv',
               labels = c("A","B", "C", "D"),
               label_size = 24, 
               hjust = -3)
# ggsave("Figures/cowplot_NDVI_NBCD.pdf", height = 8, width = 12)

### Figure 5 ###
ndvi.plot = left_join(final.counts, ndvi_nbcd, by = "stateroute")
ndvi_range = c()
sp_list = unique(ndvi.plot$aou)
for(i in sp_list){
  sp = filter(ndvi.plot, aou == i)
  ndvi = range(sp$ndvi.mean)
  ndvi_iqr = IQR(sp$ndvi.mean)
  ndvi_mean <- sum(sp$ndvi.mean*sp$speciestotal)/sum(sp$speciestotal)
  ndvi_range = rbind(ndvi_range, c(i, ndvi, ndvi_iqr, ndvi_mean))
}
ndvi_range = data.frame(ndvi_range)
names(ndvi_range) = c("AOU", "NDVI.min", "NDVI.max", "NDVI.iqr", "NDVI.mean")
#write.csv(ndvi_range, "data/ndvi_range.csv", row.names = FALSE)  

# ndvi - check outliers
ndvi_out <- ndvi.plot %>%
  group_by(aou, stateroute) %>%
  filter(n_distinct(year) > 1) %>%
  group_by(aou) %>%
  summarize(min = min(ndvi.mean),
            max = max(ndvi.mean),
            iqr = IQR(ndvi.mean),
            q1 = quantile(ndvi.mean, c(0.05)),
            q3 = quantile(ndvi.mean, c(0.95)),
            upper = q3 + 1.5*iqr,
            lower = q1 - 1.5*iqr,
            outliers = n_distinct(stateroute[ndvi.mean > upper | ndvi.mean < lower]))

min <- ggplot(ndvi_out, aes(x = q1, y = min)) + geom_point() + 
  labs(x = "5th percentile", y = "min NDVI") +
  geom_abline(slope = 1, intercept = 0)
max <- ggplot(ndvi_out, aes(x = q3, y = max)) + geom_point() + 
  labs(x = "95th percentile", y = "max NDVI") +
  geom_abline(slope = 1, intercept = 0)
plot_grid(min, max, nrow = 1)
ggsave("Figures/ndvi_range_percentiles.pdf", height = 6, width = 11)
 
#### NDVI range ranked ####
ndvi_range$range = ndvi_range$NDVI.max - ndvi_range$NDVI.min
# zero = 1 occurrence
ndvi_range$AOU = as.factor(ndvi_range$AOU)
ndvi_plot = filter(ndvi_range, range > 0)
# NDVI plot
ggplot(ndvi_plot, aes(x = reorder(AOU, - range), y = range)) + geom_errorbar(width = 0, size = 1, aes(ymin= ndvi_plot$NDVI.min, ymax=ndvi_plot$NDVI.max)) +theme_classic()+ theme(axis.title.x=element_text(size=36),axis.title.y=element_text(size=36)) + xlab("AOU")+ ylab("NDVI Range")+ theme(axis.text.x=element_text(size = 20, angle = 90),axis.ticks=element_blank(), axis.text.y=element_text(size=30)) 
# ggsave("Figures/ndvi_range_rank.pdf", height = 32, width = 42)


## NDVI breadth vs. foraging guilds

ndvi_range <- read.csv("data/ndvi_range.csv", stringsAsFactors = F)
troph_guild <- read.csv("/Volumes/HurlbertLab/Databases/Trophic Guilds/Troph_guilds.csv", header = TRUE)

gimms_ndvi = read.csv("data/gimms_ndvi_bbs_data.csv", header = TRUE)
tax_code <- read.csv("data/Bird_Taxonomy.csv", header = TRUE) %>%
  dplyr::select(AOU_OUT, CRC_SCI_NAME) %>%
  unique() %>% na.omit()

tax_code1 = tax_code[-grep("/", tax_code$CRC_SCI_NAME),] 
tax_code2 = tax_code1[-grep("sp.", tax_code1$CRC_SCI_NAME),]

troph_AOU <- left_join(troph_guild, tax_code2, by = c("Species" = "CRC_SCI_NAME"))
troph_AOU$Species = gsub('Dendroica','Setophaga', troph_AOU$Species)

bbs_sub2 <- filter(final.counts, aou %in% tax_code2$AOU_OUT)
bbs_troph <- left_join(final.counts, troph_AOU, by = c("aou" = "AOU_OUT"))

test <- filter(bbs_troph, is.na(Species) == TRUE)
tcode <- left_join(test, tax_code2, by = c("aou"="AOU_OUT")) %>%
  dplyr::select(aou, Species)
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

# species traits 
bbs_niches <- env_bbs %>%
  left_join(ndvi_range, by = c("aou" = "AOU")) %>%
  rename("route_ndvi" = "ndvi.mean",
         "spp_ndvi_mean" = "NDVI.mean",
         "spp_ndvi_min" = "NDVI.min",
         "spp_ndvi_max" = "NDVI.max")

### NDVI range vs. mean NDVI
col_scale <- c("darkgoldenrod2",
                        "palevioletred", "lightpink",
                        "purple2", "mediumorchid1",
                        "seagreen4", 
                        "navy","dodgerblue4", "dodgerblue2", 
                        "deepskyblue3", "cornflowerblue", 
                        "cyan3", "lightskyblue2",
                        "violetred3", 
                         "springgreen3", "palegreen1") 

ndvi_range_pts <- bbs_niches %>%
  distinct(aou, Trophic.guild, spp_ndvi_mean, spp_ndvi_max, spp_ndvi_min) %>%
  ggplot(aes(x = spp_ndvi_mean, y = spp_ndvi_max - spp_ndvi_min)) + 
  geom_point(aes(size = 2)) + 
  scale_color_manual(values = col_scale) + 
    labs(x = "Mean NDVI", y = "NDVI range") +
   theme(legend.position = "none")
# ggsave("Figures/species_ndvi_range_vs_meanNDVI.pdf", units = "in", height = 6, width = 8)

### avg NDVI range vs. NDVI bin

binsize <- 0.05

bbs_niches$ndvi_bin <- binsize*floor(bbs_niches$route_ndvi/binsize) + binsize/2

# write.csv(bbs_niches, "data/bbs_counts_niches.csv", row.names = F)

# need to add error bars
range_bins <- bbs_niches %>%
  group_by(ndvi_bin) %>%
  distinct(aou, spp_ndvi_max, spp_ndvi_min) %>%
  summarize(avg_range = mean(spp_ndvi_max - spp_ndvi_min, na.rm = T),
            lower = sd(spp_ndvi_max - spp_ndvi_min), 
            upper = sd(spp_ndvi_max - spp_ndvi_min)) %>%
  ggplot(aes(x = ndvi_bin, y = avg_range)) + geom_point(size = 6, shape = 15) +
  labs(x = "NDVI bin", y = "NDVI range") +
  geom_errorbar(aes(ymin = avg_range -(1.96*lower), ymax = avg_range +(1.96*upper))) +
  theme(legend.position = "none") +
  geom_smooth(method = "lm", se = F, cex = 1.5)
# ggsave("Figures/avg_NDVI_range_vs_NDVIbin.pdf", units = "in", height = 6, width = 8)

### NDVI range for spp in each foraging guild
trophic_abbv <- bbs_niches %>%
  ungroup() %>%
  distinct(Trophic.guild) %>%
  mutate(manual_abb = c("G:gu","I:uf","I:be",    
                         "I:aa","N",
                         "I:ab","O:gf",
                         "I:gg","I:lf", 
                         "I:bg","F:uc",
                         "H:gf","C:gh",  
                         "G:lc","F:lc","O:af")) %>%
  arrange(Trophic.guild) %>%
  mutate(legend_label = case_when(Trophic.guild == "Insectivore: air hawker under canopy" ~
                                    paste0("Insectivore: air hawker below canopy", " (", manual_abb, ")"),
                                  TRUE ~ paste0(Trophic.guild, " (", manual_abb, ")")))
trophic_abbv$color <- col_scale

abbv_labels <- c(trophic_abbv$manual_abb)
names(abbv_labels) <- trophic_abbv$Trophic.guild

trophic_boxplot <- bbs_niches %>%
  ungroup() %>%
  distinct(aou, Trophic.guild, spp_ndvi_min, spp_ndvi_max, spp_ndvi_mean) %>%
  na.omit(.) %>%
  group_by(Trophic.guild) %>%
  mutate(ndvi_range = spp_ndvi_max - spp_ndvi_min,
         median_range = median(ndvi_range)) %>%
  left_join(trophic_abbv, by = "Trophic.guild") %>%
  ggplot(aes(x = fct_reorder(Trophic.guild, median_range), y = ndvi_range, fill = Trophic.guild)) + 
  geom_boxplot() + 
  geom_jitter(size = 4, height = 0, width = 0.1, show.legend = F) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank()) +
  ylim(0, 1) +
  labs(y = "NDVI range", fill = "Foraging niche") +
  xlab("") +
  scale_fill_manual(values = col_scale) +
  scale_x_discrete(labels = abbv_labels)
# ggsave("Figures/ndvi_range_by_trophicGuild.pdf")

### Spp by foraging guild in each NDVI bin

foraging_rich <- bbs_niches %>%
  group_by(ndvi_bin, Trophic.guild, stateroute) %>%
  distinct(aou, Trophic.guild, stateroute) %>%
  # mean per trophic guild at each route
  summarize(nSpp = n_distinct(aou)) %>%
  group_by(ndvi_bin, Trophic.guild) %>%
  summarize(mean_nSpp = mean(nSpp)) %>%
  group_by(ndvi_bin) %>%
  mutate(total_spp = sum(mean_nSpp)) %>%
  left_join(trophic_abbv, by = "Trophic.guild")

totals <- foraging_rich %>%
  group_by(ndvi_bin) %>%
  mutate(n_guild = n_distinct(Trophic.guild)) %>%
  ungroup() %>%
  dplyr::select(ndvi_bin, total_spp, n_guild) %>%
  distinct()

forage_plot <- ggplot() +
  geom_col(data = foraging_rich, aes(x = ndvi_bin, y = mean_nSpp, fill = Trophic.guild),
           position = "stack", col = "black") + 
  scale_fill_manual(values = trophic_abbv$color, labels = trophic_abbv$legend_label) +
  labs(x = "NDVI bin", y = "Number of species", fill = "Foraging guild") +
  geom_text(data = totals,
            aes(x = ndvi_bin, y = total_spp + 8, label = n_guild), size = 8, vjust = 3) +
  theme(legend.text = element_text(size = 38), legend.title = element_text(size = 38))
# ggsave("Figures/trophic_guilds_by_NDVIbin.pdf")

#### cowplot ####
legend <- get_legend(forage_plot) 
theme_set(theme_classic(base_size = 40))
grid_effects <- plot_grid(ndvi_range_pts + theme(legend.position="none"),
          range_bins + theme(legend.position="none"),
          trophic_boxplot + theme(legend.position="none"),
          forage_plot + theme(legend.position="none"),
          align = 'hv',
          labels = c("A", "B", "C", "D"),
          label_size = 45,
          hjust = .02,
          nrow = 2) 
final_fig<- plot_grid(grid_effects, legend, rel_widths = c(1.9, 1.1))
ggsave("Figures/cowplot_BBS.pdf", units = "in", width = 35, height = 20)

