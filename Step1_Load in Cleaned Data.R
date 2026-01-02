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
# If EDIutilsAddons requires installation, follow the line of code below # 
remotes::install_github("bmcafee/EDIutilsAddons") # Install EDIutilsAddons package for easy EDI data download
library(EDIutilsAddons)

# Data is stored in EDI Data Repository. If using this data cite the Scripts on Zenoda as well as the data: 
# Butts, T.J., E.A. Albright, Q.K. Shingai, R.A. Johnson, and G.M. Wilkinson. 2026. 
#   Summer water chemistry, high frequency sensors, zooplankton and benthic macroinvertebrate 
#   community composition, periphyton, fish, and macrophyte biomass, along with lake metabolism 
#   and greenhouse gas dynamics in six experimental ponds in central Iowa, USA (2020) ver 1. 
#   Environmental Data Initiative. https://doi.org/10.6073/pasta/c5b157a1b0f294404627494dfc1587e7 (Accessed 2026-01-02).

# Load in EDI Data Sets # 
hort_field = get_data("edi.2238.1", filenum = 13) # Surface nutrients and algal concentration 
hort_field

hort_ysi = get_data("edi.2238.1", filenum = 2) # Sonde Profiles 
hort_ysi

# Load in metabolism data from Robert 
metab = get_data("edi.2238.1", filenum = 8) %>% # Ecosystem metabolism 
  filter(is.na(flag))
metab

## Food Web Data ## 
hort_fish_bodysize = get_data('edi.2238.1', filenum = 18) # fish size 
hort_fish_gaslav = get_data('edi.2238.1', filenum = 19) # fish diet (gastric lavage) 
hort_periphy = get_data('edi.2238.1', filenum = 14) # periphyton 
hort_zoop = get_data('edi.2238.1', filenum = 17) # zooplankton 
hort_mivdensity = get_data('edi.2238.1', filenum = 16) # macroinvertebrates 

# The following can be pulled from the data generated in Step 4, however, for convenience it is archived in 
# the published Zenodo script release for ease. Download from the Zenodo DOI and set working directory to load in this .csv 
hort_rdasum = read_csv("rdalgo_summarystats.csv")
