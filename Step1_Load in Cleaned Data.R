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

# Set working directory to the R Project folder 

# Data sets # 
