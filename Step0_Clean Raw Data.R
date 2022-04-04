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
setwd("J:/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment")
profile_dat = read_csv('daily-sonde-profiles_mean-values_10-30cm.csv')
treatment_column = read_csv('ghg-model-dataset.csv') %>% select(treatment, doy, pond_id)
profile_dat
setwd("J:/Box Sync/Iowa Data/Field Data/Hort Farm Field Data/2020 Hort Farm Field Data")
field_dat = read_csv('hort_fielddata_20.csv') 
field_dat_select = field_dat %>% 
  select(period, pond, doy, tp, tp_detectlim, tn, tn_detectlim, srp, srp_detectlim, nox, nox_detectlim, nhx, nhx_detectlim) %>%
  rename(pond_id = pond) %>% 
  arrange(doy)
field_dat_select

# Join data to get surface water data - base is the sonde profile data # 
field_join = left_join(profile_dat, field_dat_select, by = c('pond_id', 'doy'))
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
write_csv(field_dat_clean, 'hort20_surface_dat.csv')

#write_csv(metab_join, 'hort20_metab_dat.csv')

## Food Web Data ##===============================

## Fish Data ## 
# Body Size 
fish_lw = read_csv('fish_length_weight.csv') # In project folder 

# Gastric Lavage data 
setwd("C:/Users/Tyler/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Fish Data/Diet Data")
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

setwd("C:/Users/Tyler/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Tyler Hort Resilience/hort-benthic-pelagic")
gaslav_org = read_csv('unique_gaslav_taxa.csv')

gaslav_dat = left_join(gaslav_dat, gaslav_org, by = 'diet_id') %>% 
  mutate(treatment = case_when(.$pond %in% c('A', 'B', 'C') ~ 'pulsed', 
                               .$pond %in% c('D', 'E', 'F') ~ 'reference')) %>%
  select(pond, treatment, doy, fish_id, length, weight, diet_id, broad_taxa, abundance)
gaslav_dat

# Set working directory to project folder #
setwd("C:/Users/Tyler/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Tyler Hort Resilience/hort-benthic-pelagic")
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
  mutate(plate_area_m = 0.1*3) %>% # area of the 3 plates (m^2) of the periphyton samplers (Envco Hester Dendy Sampler) 
  mutate(plate_area_cm = 3000) %>% # area of the 3 plates in cm
  select(pond_id, collect, launch, bottle_dilution, filtered_sample, chl_rfu, biomass_ug_l, plate_area_m, plate_area_cm)
peri_dat_raw

periphy_clean = peri_dat_raw %>%
  mutate(biomass_area_m2 = (biomass_ug_l*bottle_dilution)/plate_area_m) %>% # get biomass of periphyton per area (ug/m^2)
  mutate(biomass_area_cm2 = (biomass_ug_l*bottle_dilution)/plate_area_cm) %>% # get biomass of periphyton per area (ug/cm^2)
  select(pond_id, launch, collect, biomass_area_m2, biomass_area_cm2)
periphy_clean

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
  mutate(areal_biomass = biomass/0.4) %>% #divided by the area of the rake pull (0.4 m^2)
  select(pond_id, doy, pot_fol, pot_nod, biomass, areal_biomass)
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
df <- list.files(path="J:/Box Sync/Iowa Data/Biology Data/Macroinvertebrates/2020 Hort Farm Macroinvertebrate Sheets", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
df 

unique(df$taxa) # Add to 'unique_miv_taxa' dataset key 

# Add in order taxonomic classification #
setwd("J:/Box Sync/Hort Farm Experiment/2020 Benthic Pelagic Experiment/Tyler Hort Resilience/hort-benthic-pelagic")
miv_taxa = read_csv('unique_miv_taxa.csv')
miv_taxa %<>% rename(taxa = unique_taxa)
miv_taxa

miv_join = left_join(df, miv_taxa, by = 'taxa')
miv_dat = miv_join %>%
  select(sampleid, spelling_correct, order_class, common_name, count, gear) %>%
  rename(taxa = spelling_correct) %>% 
  drop_na()
miv_dat

# Calculate Density based on gear # 
# Hess Sampler = 0.0006 cubic meters 
# Ponar Sampler = 0.0052 cubic meters 
miv_dat2 = miv_dat %>% mutate(sample_area = case_when(
    endsWith(gear, "D") ~ 0.0232, # Sample area of the Ekman Dredge (was actually a ponar dredge)
    endsWith(gear, "S") ~ 0.0710)) # Sample volume of the Modified Hess Sampler (diameter 0.3 m, surface area = 0.071 m^2) 
miv_dat2

hort_mivdensity = miv_dat2 %>%
  mutate(density = count/sample_area) %>% # Number of individuals per sample area (m^2) 
  mutate(pond_id = substr(sampleid, 4,4)) %>% 
  mutate(doy = as.numeric(substr(sampleid, 5,7))) %>%
  mutate(treatment = case_when(.$pond_id %in% c('A', 'B', 'C') ~ 'pulsed', 
                               .$pond_id %in% c('D', 'E', 'F') ~ 'reference')) %>%
  mutate(period = cut(doy, breaks=c(-Inf, 176, 212, Inf), labels=c("prepulse","postpulse1","postpulse2"))) %>%
  select(sampleid, pond_id, doy, treatment, period, taxa, order_class, common_name, gear, density)
hort_mivdensity

#write_csv(hort_mivdensity, 'hort_mivdensity.csv')
