## Convert Chloropohyll to areal estimate ## 
# Pond Width = 12.2 m 
# Pond Length = 45.7 m 
# Mean Depth = 1.2 m 

# VOLUME = 669048 (from pulse addition calculations) 
# SURFACE AREA = 12.2*45.7 = 557.54 m^2

# Periphyton # 
## Already an areal estimate ## 
hort_periphy

## Look at in a time series ## 
#set up plot - use collect date as the DOY # 

# Chlorophyll-a # 
## Take the ug/L estimate and convert to ug/m^2 ## 
## Convert ug/L to ug/m^3 then multiply by sample depth to get m^2 ## 
## Data are gapfilled data between 10-30 cm sample depth, use 0.20 m for multiplier ## 

# Zooplankton Areal # 
## Take the ug/L estimate and convert to ug/m^2 ## 
## Convert ug/L to ug/m^3 then multiply by tow depth to get m^2 

# Generate A boxplot per pond #=============================
