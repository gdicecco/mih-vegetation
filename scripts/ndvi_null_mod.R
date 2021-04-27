## NDVI range null model
## Bootstrap P-value for mean NDVI range vs. NDVI bin slope estimate

library(tidyverse)

theme_set(theme_classic(base_size = 15))

# Read in data

# spp NDVI range

ndvi_range <- read_csv("data/ndvi_range.csv")

# BBS routes by NDVI bin

bbs_niches <- read_csv("data/bbs_counts_niches.csv")

## Empirical slope: NDVI range vs. NDVI bin

range_bins <- bbs_niches %>%
  group_by(ndvi_bin) %>%
  distinct(aou, spp_ndvi_max, spp_ndvi_min) %>%
  summarize(avg_range = mean(spp_ndvi_max - spp_ndvi_min, na.rm = T),
            lower = sd(spp_ndvi_max - spp_ndvi_min), 
            upper = sd(spp_ndvi_max - spp_ndvi_min),
            n_spp = n_distinct(aou))

emp_slope <- coef(lm(avg_range ~ ndvi_bin, data = range_bins))[2]

ndvi_bins <- range_bins$ndvi_bin

# Find min and max overall in dataset
# Round so that species which occur across all these values don't have NA mean constraints later on
min <- round(min(ndvi_range$NDVI.min), 2) # NDVI min overall
max <- round(max(ndvi_range$NDVI.max), 2) # NDVI max overall

# Species mean limits

ndvi_lims <- ndvi_range %>%
  mutate(NDVI.range = NDVI.max - NDVI.min,
         mean_min = min + (NDVI.mean - NDVI.min),
         mean_max = max - (NDVI.max - NDVI.mean))

ggplot(ndvi_lims, aes(x = NDVI.range)) + 
  geom_point(aes(y = mean_min, col = "min")) + 
  geom_point(aes(y = mean_max, col = "max")) +
  labs(x = "NDVI range", y = "NDVI", col= "Mean constraint")
ggsave("figures/ndvi_null_mod_mean_constraints.pdf", height = 4, width = 6, units = "in")

## Null slopes: 999 replications

# Function: NDVI sim
# Input: NDVI bins
# Simulates for BBS species, shift of mean NDVI and calculation of average NDVI range by NDVI bin
# Output: Slope of NDVI range ~ NDVI bin for simulated species

ndvi_sim <- function(Bins) {
  
  # Sample an NDVI mean from between species constraints 
  null_ndvi <- ndvi_lims %>%
    mutate(null_mean = map2_dbl(mean_min, mean_max, 
                                ~runif(1, min = .x, max = .y)),
           null_shift = NDVI.mean - null_mean,
           null_min = NDVI.min - null_shift,
           null_max = NDVI.max - null_shift)
  
  # Populate assemblages by NDVI bin
  # Average NDVI range by NDVI bin
  
  null_bins <- data.frame(bin = Bins) %>%
    group_by(bin) %>%
    nest() %>%
    mutate(spp_list = map(bin, ~{
      b <- .
      
      null_ndvi %>%
        filter(null_min <= b, b <= null_max)
      
    })) %>%
    mutate(ndvi_range = map_dbl(spp_list, ~{
      df <- .
      
      ranges <- df %>%
        mutate(null_range = null_max - null_min)
      
      mean(ranges$null_range)
    })) %>%
    mutate(n_spp = map_dbl(spp_list, ~nrow(.)))
  
  # Null slope estimate avg NDVI range vs. NDVI bin
  
  slope <- coef(lm(ndvi_range ~ bin, data = null_bins))[2]
  
  return(slope)
}

# Null distribution of slopes

res <- data.frame(sim = seq(1:999), range_slope = c(NA))

for(i in 1:999) {
  s <- ndvi_sim(ndvi_bins)
  
  res[i, 2] <- s$range_slope

}

# % of null distribution more extreme than empirical slope
sum(emp_slope > res$range_slope)/nrow(res)

ggplot(res, aes(x = range_slope)) + geom_histogram() + 
  geom_vline(xintercept = emp_slope, col = "blue") +
  labs(x = "Slope: avg NDVI range ~ NDVI bin", y = "Count")
ggsave("figures/null_ndvi_slopes.pdf")
