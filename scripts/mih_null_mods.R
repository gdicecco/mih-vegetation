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
bbs_sub2 <- filter(bbs_sub1, aou %in% tax_code2$AOU_OUT)
bbs_troph <- left_join(bbs_sub2, troph_AOU, by = c("aou" = "AOU_OUT"))


test <- filter(bbs_troph, is.na(Species) == TRUE)
tcode <- left_join(test, tax_code[,c("AOU_OUT", "PRIMARY_COM_NAME")], by = c("aou"="AOU_OUT")) 
sp_nocodes <- unique(tcode$PRIMARY_COM_NAME)
# tax_code$CRC_SCI_NAME[tax_code$CRC_SCI_NAME == "Dendragapus obscurus/fuliginosus"] <- "Dendragapus obscurus"

####
sp_list$match = as.character(sp_list$SciName)
# renaming to get latest scientific names for mismatch spp
sp_list$match[sp_list$match =="Oreothlypis peregrina"] = "Vermivora peregrina"

sp_list$match[sp_list$match =="Vermivora pinus"] = "Vermivora cyanoptera"

sp_list$match[sp_list$match =="Stellula calliope"] = "Selasphorus calliope"

sp_list$match = gsub('Setophaga ', 'Dendroica ', sp_list$match)

sp_list$match[sp_list$match =="Dendroica ruticilla"] = "Setophaga ruticilla"

sp_list$match[sp_list$match =="Picoides nuttallii"] = "Dryobates nuttallii"

sp_list$match[sp_list$match =="Cardellina canadensis"] = "Wilsonia canadensis"

sp_list$match[sp_list$match =="Geothlypis philadelphia"] = "Oporornis philadelphia"

sp_list$match[sp_list$match =="Oreothlypis ruficapilla"] = "Vermivora ruficapilla"

sp_list$match[sp_list$match =="Oreothlypis celata"] = "Vermivora celata"

sp_list$match[sp_list$match =="Cardellina pusilla"] = "Wilsonia pusilla"

sp_list$match[sp_list$match =="Oreothlypis virginiae"] = "Vermivora virginiae"

sp_list$match[sp_list$match =="Poecile hudsonica"] = "Parus hudsonicus"

sp_list$match[sp_list$match =="Pica hudsonia"] = "Pica pica"

sp_list$match = gsub('Poecile ', 'Parus ', sp_list$match)

sp_list$match[sp_list$match =="Dendroica citrina"] = "Wilsonia citrina"

sp_list$match[sp_list$match =="Geothlypis formosus"] = "Oporornis formosus"

sp_list$match[sp_list$match =="Oreothlypis luciae"] = "Vermivora luciae"

sp_list$match[sp_list$match =="Geothlypis tolmiei"] = "Oporornis tolmiei"

sp_list$match[sp_list$match =="Calcarius mccownii"] = "Rhynchophanes mccownii"

sp_list$match[sp_list$match =="Picoides villosus"] = "Leuconotopicus villosus"

sp_list$match[sp_list$match =="Picoides pubescens"] = "Dryobates pubescens"

sp_list$match[sp_list$match =="Picoides dorsalis"] = "Picoides tridactylus"

sp_list$match[sp_list$match =="Picoides scalaris"] = "Dryobates scalaris"

sp_list$match[sp_list$match =="Picoides albolarvatus"] = "Leuconotopicus albolarvatus"

sp_list$match[sp_list$match =="Picoides borealis"] = "Leuconotopicus borealis"

sp_list$match[sp_list$match =="Aimophila cassinii"] = "Peucaea cassinii"

sp_list$match[sp_list$match =="Aimophila aestivalis"] = "Peucaea aestivalis"

sp_list$match[sp_list$match =="Aimophila botterii"] = "Peucaea botterii"

sp_list$match[sp_list$match =="Aimophila carpalis"] = "Peucaea carpalis"

sp_list$match[sp_list$match =="Oreothlypis crissalis"] = "Vermivora crissalis"

sp_list$match[sp_list$match =="Ixoreus naevius"] = "Zoothera naevia"





gimms_agg = gimms_ndvi %>% filter(month == c("may", "jun", "jul")) %>% 
  group_by(site_id)  %>%  dplyr::summarise(ndvi.mean=mean(ndvi))
gimms_agg$stateroute = gimms_agg$site_id
ndvi = gimms_agg[,c("stateroute", "ndvi.mean")]



# left join to get temporal occupancy
env_bbs = left_join(bbs_sub1, ndvi, by = "stateroute") %>%
  left_join(., troph_guild)
  na.omit(env_bbs) 


#### null model ####
null_output = c()
init.time = Sys.Date()
for(rte in bbs_sub1$stateroute){
  subdata = subset(bbs_sub1, stateroute == rte)
  sites = unique(subdata$site)
  for(site in sites){
    sitedata = subdata[subdata$site == site,]
    notrans = na.omit(sitedata[sitedata$propOcc > 1/3,])
    trans = na.omit(sitedata[sitedata$propOcc <= 1/3,])
    num_notrans = length(notrans$propOcc)
    num_trans = length(trans$propOcc)
    
    for(r in 1:10){
      print(paste(rte, site, r, Sys.Date()))
      
      if(num_notrans >= num_trans) {
        null_sample = sample_n(notrans, num_notrans - num_trans, replace = FALSE) %>%
          rbind(trans)
      } else {
        null_sample = sample_n(trans, num_notrans, replace = FALSE)  
      }
      
      logseries_weights_excl = null_sample %>%
        group_by(datasetID, site) %>% 
        dplyr::summarize(weights = get_logseries_weight(abunds), treatment = 'Null')
      
      null_output = rbind(null_output, c(r, rte, site, logseries_weights_excl[,3], num_notrans))
      nwd = nrow(sad_data)
      
    } # end r loop
  } # end site loop
} # end dataset loop


null_output = data.frame(null_output)
colnames(null_output) = c("number", "datasetID", "site", "SAD_excl","Non_trans")