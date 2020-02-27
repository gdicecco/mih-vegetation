## Combine BBC fragstats files into one dataset

library(tidyverse)
library(purrr)

dir <- "C:/Users/gdicecco/Desktop/data/nlcd_bbc/"

files <- list.files(dir) %>%
  as.data.frame(stringsAsFactors = F) %>%
  rename(filename = ".")

bbc_fragstats <- files %>%
  mutate(siteID = word(filename, 6,6, sep = "_")) %>%
  mutate_at(c("siteID"), ~word(., 1,1, sep ="\\.")) %>%
  group_by(filename, siteID) %>%
  nest() %>%
  mutate(data = map(filename, ~read_csv(paste0(dir, .)))) %>%
  unnest(cols = c(data))

write.csv(bbc_fragstats, "data/bbc_fragstats.csv", row.names = F)
