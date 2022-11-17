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
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)
# Set working directory to the R Project folder 

# Data sets # 

## Meteorology ## 
hort_met = read_csv('weather-station_data.csv')
hort_met

## Water Quality and response variables ## 
hort_field = read_csv('hort20_surface_dat.csv')
hort_field

# Sonde profile data - average between 10 - 30 cm depth 
   # One DOY is missing in early summer, data filled by linear interpolation 
hort_sonde = read_csv('daily-sonde-profiles_mean-values_10-30cm_gapfilled.csv')
hort_sonde

## do ## 
hort_dosat = read_csv('hort20_dosat_dat.csv')
hort_dosat

# Load in metabolism data from Robert 
metab = read_csv('daily-metabolism_data_robertcorrected.csv') %>% 
  filter(flag == 0)
metab # Need to interrogate distribution of missing values ## 
  # Striplot by DOY for each pond 

## Food Web Data ## 
hort_fish_bodysize = read_csv('fish_length_weight.csv') # fish size 
hort_fish_gaslav = read_csv('gaslav_clean.csv') # fish diet (gastric lavage)
hort_periphy = read_csv('periphy_clean.csv') # periphyton
hort_macrophy = read_csv('macrophy_clean.csv') # macrophytes 
hort_zoop = read_csv('hort_zp_clean_72622.csv') # zooplankton 
hort_mivdensity = read_csv('hort_mivdensity.csv') # macroinvertebrates 
