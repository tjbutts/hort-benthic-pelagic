## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts January 2022

#============================================#
# STEP 0: Clean Data 
#============================================#
rm(list=ls())
graphics.off()

# Required Libraries for analysis and visualization
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(magrittr)) install.packages('magrittr')
library(magrittr)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate) 

# Set working directory as needed to location of cleaned data # 


############# Data sets ############## 
## Fish Data ## 
# Body Size 
fish_lw = read_csv('fish_length_weight.csv') # In project folder 

# Gastric Lavage data 
setwd("C:/Users/Owner/Box/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Fish Data/Diet Data")
gaslav_spp = read_csv('gaslav_dat.csv') %>% rename(sampleid = organism)
gaslav_spp
gaslav_paklog = read_csv('gastric_lavage_log.csv')
gaslav_paklog

join_gaslav = left_join(gaslav_spp, gaslav_paklog, by='sampleid')
join_gaslav 

gaslav_dat = join_gaslav %>% 
  rename(diet_id = spp.x, 
         abundance = count, 
         fish_id = spp.y,
         doy = DOY) %>% 
  select(!(sifted)) %>% # remove sifted column as all samples have been sifted 
  select(pond, doy, fish_id, length, weight, whirlpak, diet_id, abundance)
gaslav_dat

# Set working directory to project folder #
#write_csv(gaslav_dat, 'gaslav_clean.csv') # Run to create new .csv file 

## Field Data ## 
# Get raw data # 
setwd("C:/Users/Owner/Box/Hort Farm Experiment/2020 Benthic Pelagic Experiment")
gapfilled_dat = read_csv('daily-sonde-profiles_mean-values_10-30cm_gapfilled.csv')
treatment_column = read_csv('ghg-model-dataset.csv') %>% select(treatment, doy, pond_id)
gapfilled_dat
setwd("C:/Users/Owner/Box/Iowa Data/Field Data/Hort Farm Field Data/2020 Hort Farm Field Data")
field_dat = read_csv('hort_fielddata_20.csv') 
field_dat_select = field_dat %>% 
  select(period, pond, doy, tp, tp_detectlim, tn, tn_detectlim, srp, srp_detectlim, nox, nox_detectlim, nhx, nhx_detectlim) %>%
  rename(pond_id = pond) %>% 
  arrange(doy)
field_dat_select

# Join data to get surface water data - base is the gapfilled data # 
field_join = left_join(gapfilled_dat, field_dat_select, by = c('pond_id', 'doy'))
field_join 
field_join2 = left_join(field_join, treatment_column, by = c('pond_id', 'doy'))
field_join2

# Clean data # 
field_dat_clean = field_join2 %>% 
  select(pond_id, treatment, period, doy, temp, do_sat, do, ph, chla_rfu, chla, cond, sp_cond, 
         phyco_rfu, phyco, salinity, cyanofluor_chl, tp, tp_detectlim, srp, srp_detectlim, tn, tn_detectlim, nox, nox_detectlim,
         nhx, nhx_detectlim) %>%
  rename(tp_flag = tp_detectlim, 
         srp_flag = srp_detectlim, 
         tn_flag = tn_detectlim, 
         nox_flag = nox_detectlim,
         nhx_flag = nhx_detectlim) %>% 
  mutate(tp_flag = gsub('A', 'a', tp_flag), 
         tp_flag = gsub('Z', 'b', tp_flag), 
         srp_flag = gsub('A', 'a', srp_flag), 
         srp_flag = gsub('Z', 'b', srp_flag),
         tn_flag = gsub('A', 'a', tn_flag), 
         tn_flag = gsub('Z', 'b', tn_flag),
         nox_flag = gsub('A', 'a', nox_flag), 
         nox_flag = gsub('Z', 'b', nox_flag),
         nhx_flag = gsub('A', 'a', nhx_flag), 
         nhx_flag = gsub('Z', 'b', nhx_flag))
field_dat_clean

is.nan.data.frame <- function(x) # function to turn NaN into 0
do.call(cbind, lapply(x, is.nan)) # call to function 
field_dat_clean[is.nan(field_dat_clean)] <- 0 # turn NaN into 0 
field_dat_clean

# Make dataset # 
setwd("C:/Users/Owner/Box/Active/Active Hort/Tyler Hort Resilience")
# write_csv(field_dat_clean, 'hort20_surface_dat.csv')

## Metabolism data ## 
setwd("C:/Users/Owner/Box/Hort Farm Experiment/2020 Benthic Pelagic Experiment")
metabolism_dat = read_csv('ghg-model-dataset.csv') %>% select(pond_id, treatment, period, 
                                                              doy, GPP, R, NEP)
metabolism_dat
dosat_minidot = read_csv('2020_dologger_dat.csv')
dosat_minidot
daily_do = dosat_minidot %>% 
  mutate(datetime = mdy_hm(datetime), 
         date = mdy(date)) %>%
  mutate(doy = yday(date)) %>%
  group_by(pond, doy) %>%
  summarise(
    dosat_dailyavg = mean(dosat)) %>%
  rename(pond_id = pond) %>%
  select(pond_id, doy, dosat_dailyavg) %>%
  filter(doy > 141 & doy < 241)
daily_do

metab_join = left_join(metabolism_dat, daily_do, by = c('pond_id', 'doy'))
metab_join

setwd("C:/Users/Owner/Box/Active/Active Hort/Tyler Hort Resilience")
write_csv(metab_join, 'hort20_metab_dat.csv')
