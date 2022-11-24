## Step4_Response Detection Algorithm  ##

## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022 # 

#============================================#
# STEP 4: Response Detection Algorithm
#============================================#

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
## Disturbhf - Walter et al. 2022 L&O  
if (!require(disturbhf)) install.packages('remotes')
remotes::install_github('jonathan-walter/disturbhf')
library(disturbhf)

# Chlorophyll-a #===================
# data # 
hort_sonde

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy_low = hort_sonde %>%
  select(pond_id, doy, chla) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = chla)
testy_low

refy_low = hort_sonde %>%
  select(pond_id, doy, chla) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=chla)
refy_low

## Intermediate Coupling ## 
testy_int = hort_sonde %>%
  select(pond_id, doy, chla) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = chla)
testy_int

refy_int = hort_sonde %>%
  select(pond_id, doy, chla) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = chla)
refy_int

## High Coupling ## 
testy_high = hort_sonde %>%
  select(pond_id, doy, chla) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = chla)
testy_high

refy_high = hort_sonde %>%
  select(pond_id, doy, chla) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=chla)
refy_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low = mwdistdiffz(testy = testy_low, refy = refy_low, 
                      wwidth = 7, 
                      ddiff_method = 'integral')
rda_int = mwdistdiffz(testy = testy_int, refy = refy_int, 
                      wwidth = 7, 
                      ddiff_method = 'integral')
rda_high = mwdistdiffz(testy = testy_high, refy = refy_high, 
                       wwidth = 7, 
                       ddiff_method = 'integral')
