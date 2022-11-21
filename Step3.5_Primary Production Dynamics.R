## Step3_Primary Production Dynamics ##

## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022

#============================================#
# STEP 3: Chlorophyll-a time series 
#           & Response Detection Algorithm
#============================================#

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

# ========= FUNCTIONS ========= # 
loess()
optim()

# ========= PLOTTING COLORS ===== # 
low_col_B = rgb(74, 166, 81, max = 255, alpha = 180) #Pond B, Pond F
low_col_F = rgb(74, 166, 81, max = 255, alpha = 100) #Pond B, Pond F
low_col = rgb(74, 166, 81, max = 255, alpha = 255) #Pond B, Pond F
ref_col = rgb(155, 155, 155, max=255, alpha = 100) # Reference
black_col = rgb(0,0,0, max=255, alpha = 100) # Black

int_col_A = rgb(44, 127, 184, max = 255, alpha = 180) #Pond A, pond D
int_col_D = rgb(44, 127, 184, max = 255, alpha = 100) #Pond A, pond D
int_col = rgb(44, 127, 184, max = 255, alpha = 255) #Pond A, pond D

high_col_C = rgb(8, 29, 88, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(8, 29, 88, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(8, 29, 88, max = 255, alpha = 255) #Pond C, Pond E

# Data # 
hort_sonde # Daily profile data 

# Separate to just chlorophyll-a measurements 
chl = hort_sonde %>%
  select(pond_id, doy, chla)
chl

# Chlorophyll-a by pond (easier to apply LOESS fit if separated) #
algA = chl %>% #pulse, int
  filter(pond_id == "A") 

algB = chl %>% #pulse, low
  filter(pond_id == "B") 

algC = chl %>% #pulse, high
  filter(pond_id == "C") 

algD = chl %>% #ref, int
  filter(pond_id == "D") 

algE = chl %>% #ref, high
  filter(pond_id == "E") 

algF = chl %>% #ref, low
  filter(pond_id == "F") 

# LOESS regression of chlorophyll for pattern - not analysis ##==========================
set.seed(55)

# loess low pulse 
alg_B_loess = loess(chla ~ doy, data = algB, span = 0.10) # 10% span
alg_B_loess

# get smoothed output
alg_B_smooth = predict(alg_B_loess, se = TRUE)

# loess low reference
alg_F_loess = loess(chla ~ doy, data = algF, span = 0.10) # 10% span
alg_F_loess

# get smoothed output
alg_F_smooth = predict(alg_F_loess, se = TRUE)

# plot
plot(algF$chla, x=algF$doy, type = 'p', pch = 20, cex=1.5, xlab = 'Day of Year, 2020',
     ylab = 'Chlorophyll-a', xlim=c(140, 245), ylim=c(0, 35), col = ref_col)
polygon(c(142:240, 240:142), c(alg_F_smooth$fit - alg_F_smooth$se.fit, 
                               rev(alg_F_smooth$fit + alg_F_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(alg_F_smooth$fit, x=algF$doy, col=ref_col, lwd = 3)

par(new=T) # add new smooth to same plot 

plot(algB$chla, x=algB$doy, type = 'p', pch = 20, cex=1.5, xlab = 'Day of Year, 2020',
     ylab = 'Chlorophyll-a', xlim=c(140, 245), ylim=c(0, 35), col = low_col_F)
polygon(c(142:240, 240:142), c(alg_B_smooth$fit - alg_B_smooth$se.fit, 
                               rev(alg_B_smooth$fit + alg_B_smooth$se.fit)), 
        col = low_col_F, border = NA)
lines(alg_B_smooth$fit, x=algB$doy, col=low_col_B, lwd = 3)


#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
