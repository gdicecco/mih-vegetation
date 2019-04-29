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


#### null model ####
null_output = c()
init.time = Sys.Date()
for(rte in unique(env_bbs$ndvi.mean)){
  subdata = subset(env_bbs, ndvi.mean == rte)
    for(r in 1:10){
      print(paste(rte, r, Sys.Date()))
      FGobs = length(unique((subdata$Trophic.guild)))
      Sobs = length(unique((subdata$aou)))
      FGnull = sample_n(env_bbs, Sobs, replace = FALSE) %>%
        select(aou, Trophic.guild)
      ndf = data.frame(r, rte, FGobs, Sobs, FGnull) # we might just want to include a count here instead of rows?
                                                    # assuming we want num of FG obs/num FG actual?
      null_output = rbind(null_output, ndf)
      }
    } # end r loop



null_output = data.frame(null_output)
colnames(null_output) = c("number", "datasetID", "site", "SAD_excl","Non_trans")