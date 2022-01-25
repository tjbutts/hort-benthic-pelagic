## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts January 2022

# Grace has entered the chat
# This is a github connection test
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
if (!require(readr)) install.packages('readr')
library(readr) 

# Set working directory as needed to location of cleaned data # 
## Throughout - setwd() will be used to pull raw data, clean it, and then write_csv() is used to place it the project folder 
# Current write_csv() commands are commented out to avoid overwriting data now that it is included in the project folder

############# Field Data ############## 

## Field Data ## 
# Get raw data # 
setwd("C:/Users/Tyler/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment")
gapfilled_dat = read_csv('daily-sonde-profiles_mean-values_10-30cm_gapfilled.csv')
treatment_column = read_csv('ghg-model-dataset.csv') %>% select(treatment, doy, pond_id)
gapfilled_dat
setwd("C:/Users/Tyler/Box Sync/Iowa Data/Field Data/Hort Farm Field Data/2020 Hort Farm Field Data")
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
  mutate(tp_flag = gsub('A', NA, tp_flag), 
         tp_flag = gsub('Z', 'b', tp_flag), 
         srp_flag = gsub('A', NA, srp_flag), 
         srp_flag = gsub('Z', 'b', srp_flag),
         tn_flag = gsub('A', NA, tn_flag), 
         tn_flag = gsub('Z', 'b', tn_flag),
         nox_flag = gsub('A', NA, nox_flag), 
         nox_flag = gsub('Z', 'b', nox_flag),
         nhx_flag = gsub('A', NA, nhx_flag), 
         nhx_flag = gsub('Z', 'b', nhx_flag))
field_dat_clean

is.nan.data.frame <- function(x) # function to turn NaN into 0
do.call(cbind, lapply(x, is.nan)) # call to function 
field_dat_clean[is.nan(field_dat_clean)] <- 0.1 # turn NaN into 0 
field_dat_clean

# Make dataset # 
setwd("C:/Users/Owner/Box/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Tyler Hort Resilience/hort-benthic-pelagic")
#write_csv(field_dat_clean, 'hort20_surface_dat.csv')

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

setwd("C:/Users/Tyler/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Tyler Hort Resilience/hort-benthic-pelagic")
#write_csv(metab_join, 'hort20_metab_dat.csv')

## Food Web Data ##===============================

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

## Periphyton ## 
setwd("C:/Users/Tyler/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Periphyton")
peri_dat = read_csv('periphyton_dat_full.csv')
peri_dat_raw = peri_dat %>%
  rename(launch = doy_launch, 
         collect = doy_collect, 
         bottle_dilution = dilution_l,
         filtered_sample = filtered,
         pond_id = pond, 
         chl_rfu = chl) %>% 
  mutate(plate_area = 57.76*3) %>% # area of the 3 plates of the periphyton samplers 
  select(pond_id, collect, launch, bottle_dilution, filtered_sample, chl_rfu, biomass_ug_l, plate_area)

periphy_clean = peri_dat_raw %>%
  mutate(biomass_area = (biomass_ug_l*0.3)/plate_area) %>% # get biomass of periphyton per area 
  select(pond_id, launch, collect, biomass_area)

setwd("C:/Users/Tyler/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Tyler Hort Resilience/hort-benthic-pelagic")
#write_csv(periphy_clean, 'periphy_clean.csv')  

## Macrophytes ## 
setwd("C:/Users/Tyler/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Macrophytes")
macrophy_clean = read_csv('Hort2020_Biomass_Compiled.csv') %>% 
  rename(doy = DOY, 
         pond_id = Pond, 
         biomass = Biomass,
         pot_fol = POT_FOL,
         pot_nod = POT_NOD) %>%
  select(pond_id, doy, pot_fol, pot_nod, biomass) 
macrophy_clean

setwd("C:/Users/Tyler/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Tyler Hort Resilience/hort-benthic-pelagic")
#write_csv(macrophy_clean, 'macrophy_clean.csv')

## Zooplankton ## 
setwd("C:/Users/Tyler/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Zooplankton")
zp_raw = read_csv('2020_Hort_ZoopBiomass_clean.csv')
zp_clean = zp_raw %>% select(!(X1)) %>% select(pond_id, treatment, period, doy, group, taxon, biomass)

setwd("C:/Users/Tyler/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Tyler Hort Resilience/hort-benthic-pelagic")
#write_csv(zp_clean, 'hort_zp_clean_11622.csv')

## Macroinvertebrates ## 
# Combine .csv files into one file
# Read in individual .csv into one .csv file 
df <- list.files(path="C:/Users/Owner/Box/Iowa Data/Biology Data/Macroinvertebrates/2020 Hort Farm Macroinvertebrate Sheets", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
df 
# Add in order taxonomic classification # 
miv_taxa = read_csv('unique_miv_taxa.csv')
miv_taxa %<>% rename(taxa = unique_taxa)

miv_join = left_join(df, miv_taxa, by = 'taxa')
miv_dat = miv_join %>%
  select(sampleid, spelling_correct, order_class, count, gear) %>%
  rename(taxa = spelling_correct)
miv_dat

