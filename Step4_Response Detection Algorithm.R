## Step4_Response Detection Algorithm  ##

## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022 # 

#============================================#
# STEP 4: Response Detection Algorithm
#============================================#

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
## Disturbhf - Walter et al. 2022 L&O  
if (!require(disturbhf)) install.packages('remotes')
remotes::install_github('jonathan-walter/disturbhf')
library(disturbhf)

# Chlorophyll-a #===================
# data # 
hort_sonde

# Run Response Detection Analysis # 
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

# Gross Primary Production #===================
# data # 
metab 

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy2_low = metab %>%
  select(pond_id, doy, GPP) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = GPP)
testy2_low

refy2_low = metab %>%
  select(pond_id, doy, GPP) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=GPP)
refy2_low

## Intermediate Coupling ## 
testy2_int = metab %>%
  select(pond_id, doy, GPP) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = GPP)
testy2_int

refy2_int = metab %>%
  select(pond_id, doy, GPP) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = GPP)
refy2_int

## High Coupling ## 
testy2_high = metab %>%
  select(pond_id, doy, GPP) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = GPP)
testy2_high

refy2_high = metab %>%
  select(pond_id, doy, GPP) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=GPP)
refy2_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low2 = mwdistdiffz(testy = testy2_low, refy = refy2_low, 
                      wwidth = 7, 
                      ddiff_method = 'integral')
rda_int2 = mwdistdiffz(testy = testy2_int, refy = refy2_int, 
                      wwidth = 7, 
                      ddiff_method = 'integral')
rda_high2 = mwdistdiffz(testy = testy2_high, refy = refy2_high, 
                       wwidth = 7, 
                       ddiff_method = 'integral')

# Plot chl-a and GPP Response Detection Algorithm #=================

# ========= PLOTTING COLORS ===== # 
low_col_B = rgb(74, 166, 81, max = 255, alpha = 180) #Pond B, Pond F
low_col_F = rgb(74, 166, 81, max = 255, alpha = 100) #Pond B, Pond F
low_col = rgb(74, 166, 81, max = 255, alpha = 255) #Pond B, Pond F
ref_col = rgb(155, 155, 155, max=255, alpha = 100) # Reference
black_col = rgb(0,0,0, max=255, alpha = 100) # Black
transparent = rgb(255,255,255, max=255, alpha = 0)

int_col_A = rgb(44, 127, 184, max = 255, alpha = 180) #Pond A, pond D
int_col_D = rgb(44, 127, 184, max = 255, alpha = 100) #Pond A, pond D
int_col = rgb(44, 127, 184, max = 255, alpha = 255) #Pond A, pond D

high_col_C = rgb(8, 29, 88, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(8, 29, 88, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(8, 29, 88, max = 255, alpha = 255) #Pond C, Pond E

## ============ Plot Margins ================= ##
# Window for checking plot 
#windows(height = 4, width = 6) 

# Will create plot in whatever file path you set  
pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure4.pdf", 
    height = 4, 
    width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3), mar = c(0,0.5,0.5,0), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0)) 

## Chlorophyll-a RDA plot ##==========================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', col.axis = transparent, yaxt = 'n', 
     lwd=3, col=low_col, data=rda_low)
axis(side = 2, at=c(-2, 0, 2, 4, 6))
mtext(side = 2, line = 3.2, 
      expression('Chlorophyll-'~italic(a)), cex = 9/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 9/12)
text(141, 6, 'A', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', col.axis = transparent,
     lwd=3, col=int_col, data=rda_int)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5) 
text(141, 6, 'B', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', col.axis = transparent, 
     lwd=3, col=high_col, data=rda_high)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5)
text(141, 6, 'C', font = 2)

# Add in Labels 
text(155, 2.5, 'Response')
text(230, 0.9, 'Recovery')

## GPP RDA plot ##============================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n',
     lwd=3, col=low_col, data=rda_low2)
mtext(side = 2, line = 3.2, 'Gross Primary Production', cex = 9/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 9/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
axis(side = 2, at=c(-2, 0, 2, 4, 6))

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5) 
text(141, 6, 'D', font = 2)

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
      ylab = '', xlab = '', yaxt = 'n', 
     lwd=3, col=int_col, data=rda_int2)
axis(side = 2, at = c(-2, 0, 2, 4, 6), labels = F)
mtext(side = 1, line = 2, 'Day of Year, 2020', cex = 9/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5)
text(141, 6, 'E', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
      ylab = '', xlab = '', yaxt = 'n', 
     lwd=3, col=high_col, data=rda_high2)
axis(side = 2, at=c(-2, 0,2,4,6), labels = F)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
abline(h=0.5)
text(141, 6, 'F', font = 2)

# Create plot in specified file path # 
dev.off()
