## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022

#============================================#
# STEP 3: Chlorophyll-a time series 
#           & Response Detection Algorithm
#============================================#

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
## Disturbhf - Walter & Buelo 
if (!require(disturbhf)) install.packages('remotes')
remotes::install_github('jonathan-walter/disturbhf')
library(disturbhf)

# Data # 
hort_sonde # Daily profile data 

# Separate to just chlorophyll-a measurements 
chl = hort_sonde %>%
  select(pond_id, doy, chla)
chl

# Chlorophyll-a by pond (easier to apply GAMs if separated) # 
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

# GAM-ing the chlorophyll for pattern - not analysis ##==========================

set.seed(55)

#GAM - Intermediate pulse 
alg_A_gam <- gam(chla~ s(doy, k = 50),data = algA, method = 'REML') # k chosen to minimize pattern in residuals 
summary(alg_A_gam) # smoothing term significant 
gam.check(alg_A_gam)

#GAM - Intermediate reference
alg_D_gam <- gam(chla~ s(doy, k = 50),data = algD, method = 'REML')
summary(alg_D_gam) # smoothing term significant 
gam.check(alg_D_gam)

#GAM - High pulse
alg_C_gam <- gam(chla~ s(doy, k = 50),data = algC, method = 'REML')
summary(alg_C_gam) # smoothing term significant 
gam.check(alg_C_gam)

#GAM - High reference 
alg_E_gam <- gam(chla~ s(doy, k = 50),data = algE, method = 'REML')
summary(alg_E_gam) # smoothing term significant 
gam.check(alg_E_gam)

#GAM - Low pulse
alg_B_gam <- gam(chla~ s(doy, k = 50),data = algB, method = 'REML')
summary(alg_B_gam) # smoothing term significant 
gam.check(alg_B_gam)

#GAM - Low reference 
alg_F_gam <- gam(chla~ s(doy, k = 50),data = algF, method = 'REML')
summary(alg_F_gam) # smoothing term significant 
gam.check(alg_F_gam)

# Plotting GAMs #=========================
#Colors for data visualization
#Ref: lty = 1, pulse: lty = 1
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

# Set up the Plot
# Max width = 7 inches; Max height = 9 inches 
windows(height = 3, width = 6)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

# Low Coupling # 
plot(alg_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim=c(140, 245), ylim=c(0, 35),
     cex = .75, pch = 17, lwd = 0.5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(alg_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, ylim=c(0, 35), xlim=c(140, 245),
     cex = 0.75, pch = 16, lwd = 0.5, lty = 1, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Chlorophyll-a (ug/L)", cex = 1.25)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# Intermediate Coupling # 
plot(alg_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim = c(140, 245), ylim=c(0, 35), 
     cex = 0.75, pch = 17, lwd = 0.5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(alg_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(0, 35), xlim = c(140,245),
     cex = 0.75, pch = 16, lwd = 0.5, lty = 1, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# High Coupling # 
plot(alg_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", ylim=c(0, 35), xlim=c(140,245),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(alg_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(0, 35), xlim=c(140, 245), 
     cex = 0.75, pch = 16, lwd = 0.5, lty = 1, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# Run Response Detection Analysis #======================

# Create disturbed and reference time series # 
## Low Coupling ## 
testy_low = hort_sonde %>%
  select(pond_id, doy, chla) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = chla)
testy_low

refy_low = hort_sonde %>%
  select(pond_id, doy, chla) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=chla)
refy_low

## Intermediate Coupling ## 
testy_int = hort_sonde %>%
  select(pond_id, doy, chla) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = chla)
testy_int

refy_int = hort_sonde %>%
  select(pond_id, doy, chla) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = chla)
refy_int

## High Coupling ## 
testy_high = hort_sonde %>%
  select(pond_id, doy, chla) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = chla)
testy_high

refy_high = hort_sonde %>%
  select(pond_id, doy, chla) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=chla)
refy_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low = mwdistdiffz(testy = testy_low, refy = refy_low, 
                     wwidth = 7, 
                     ddiff_method = 'integral')
rda_int = mwdistdiffz(testy = testy_int, refy = refy_int, 
                     wwidth = 7, 
                     ddiff_method = 'integral')
rda_high = mwdistdiffz(testy = testy_high, refy = refy_high, 
                       wwidth = 7, 
                       ddiff_method = 'integral')

# Plot Z-score plots # 
windows(height = 3, width = 6)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     cex.axis=1.2, cex=0.75, ylab = '', xlab = '',
     lwd=4, col=low_col, data=rda_low)
mtext(side = 2, line = 3, "Z-scores", cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     cex.axis=1.2, cex=0.75, ylab = '', xlab = '',
     lwd=4, col=int_col, data=rda_int)
mtext(side = 1, line = 3, "Day of Year, 2020", cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5) 

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     cex.axis=1.2, cex=0.75, ylab = '', xlab = '',
     lwd=4, col=high_col, data=rda_high)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5) 

# Combine Chlorophyll-a and RDA analysis into one plot # 
# Set up the Plot
# Max width = 7 inches; Max height = 9 inches 
windows(height = 6, width = 6)
par(mfrow =c(2,3),omi = c(1.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

## Chlorophyll GAMs ## 
# Low Coupling # 
plot(alg_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim=c(140, 245), ylim=c(0, 35),
     cex = .75, pch = 17, lwd = 0.5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(alg_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, ylim=c(0, 35), xlim=c(140, 245),
     cex = 0.75, pch = 16, lwd = 0.5, lty = 1, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Chlorophyll-a (ug/L)", cex = 1.25)
mtext(side = 3, line = 1, 'Low Coupling', cex = 1.25)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# Intermediate Coupling # 
plot(alg_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim = c(140, 245), ylim=c(0, 35), 
     cex = 0.75, pch = 17, lwd = 0.5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(alg_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(0, 35), xlim = c(140,245),
     cex = 0.75, pch = 16, lwd = 0.5, lty = 1, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 1, 'Intermediate', cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# High Coupling # 
plot(alg_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", ylim=c(0, 35), xlim=c(140,245),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(alg_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(0, 35), xlim=c(140, 245), 
     cex = 0.75, pch = 16, lwd = 0.5, lty = 1, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 1, 'High Coupling', cex = 1.25)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

## Response Detection Algorithm ## 
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     cex.axis=1.2, cex=0.75, ylab = '', xlab = '',
     lwd=4, col=low_col, data=rda_low)
mtext(side = 2, line = 3, "Z-scores", cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     cex.axis=1.2, cex=0.75, ylab = '', xlab = '',
     lwd=4, col=int_col, data=rda_int)
mtext(side = 1, line = 3, "Day of Year, 2020", cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5) 

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     cex.axis=1.2, cex=0.75, ylab = '', xlab = '',
     lwd=4, col=high_col, data=rda_high)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5)

