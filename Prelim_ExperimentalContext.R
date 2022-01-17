## Hort Farm Ecosystem Resilience Project ###
# Code originally written by GM Wilkinson January 2022

#Experimental Context =================================
# Exploratory time series of: 
  # Nutrient concentrations
  # Nutrient ratios
  # DOC (when we get the data)

# Read in the relevant data and packages
library(tidyverse)
hort_field = read_csv('hort20_surface_dat.csv')
hort_field

# Because we will be plotting by pond, make some separate data frames to make life easier
fieldA = hort_field %>% filter(pond_id == "A")
fieldB = hort_field %>% filter(pond_id == "B")
fieldC = hort_field %>% filter(pond_id == "C")
fieldD = hort_field %>% filter(pond_id == "D")
fieldE = hort_field %>% filter(pond_id == "E")
fieldF = hort_field %>% filter(pond_id == "F")

#Plot the nutrient data
plot(fieldA$doy, fieldA$tp)