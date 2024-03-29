## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts January 2022

#============================================#
# STEP 1: LOAD IN DATASETS 
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
# Visualization
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2) 
if (!require(scales)) install.packages('scales')
library(scales) 

# Set working directory to the R Project folder 

# Data sets # 

## Meteorology ## 
hort_met = read_csv('weather-station_data.csv')
hort_met

## Water Quality and response variables ## 
hort_field = read_csv('hort20_surface_dat.csv')
hort_field

## Metabolism ## 
hort_dosat = read_csv('hort20_dosat_dat.csv')
hort_dosat

hort_metabolism = read_csv('daily-metabolism_data.csv') 
hort_gpp = hort_metabolism %>% 
  select(pond_id, doy, GPP) %>%
  filter(GPP >= 0)
hort_r = hort_metabolism %>%
  select(pond_id, doy, R) %>%
  filter(R <= 0)
hort_metab_join = left_join(hort_gpp, hort_r, by = c('doy', 'pond_id'))
hort_metabolism_clean = hort_metab_join %>% 
  mutate(NEP = GPP+R) %>% 
  drop_na()
hort_metabolism_clean

## Food Web Data ## 
hort_fish_bodysize = read_csv('fish_length_weight.csv') # fish size 
hort_fish_gaslav = read_csv('gaslav_clean.csv') # fish diet (gastric lavage)
hort_periphy = read_csv('periphy_clean.csv') # periphyton
hort_macrophy = read_csv('macrophy_clean.csv') # macrophytes 
hort_zoop = read_csv('hort_zp_clean_11622.csv') # zooplankton 
hort_mivdensity = read_csv('hort_mivdensity.csv') # macroinvertebrates 
