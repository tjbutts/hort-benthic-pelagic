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
if (!require(here)) install.packages('here')
library(here)
# Set working directory to the R Project folder 

# Data sets # 

# Sonde profile data - average between 10 - 30 cm depth + surface nutrients 
hort_field = read_csv('surface_nutrients_chla.csv')
hort_field

hort_ysi = read_csv('profiles_daily_deepsite.csv')
hort_ysi

# Load in metabolism data from Robert 
metab = read_csv('daily-metabolism_data_robertcorrected.csv') %>% 
  filter(flag == 0)
metab  

## Food Web Data ## 
hort_fish_bodysize = read_csv('fish_length_weight.csv') # fish size 
hort_fish_gaslav = read_csv('gaslav_clean.csv') # fish diet (gastric lavage)
hort_periphy = read_csv('periphy_clean.csv') # periphyton
hort_zoop = read_csv('hort_zp_clean_72622.csv') # zooplankton 
hort_mivdensity = read_csv('hort_mivdensity.csv') # macroinvertebrates 

## Summary Statistics ## 
hort_rdasum = read_csv('rdalgo_summarystats.csv') # Response Detection Algorithm Summary Stats 
