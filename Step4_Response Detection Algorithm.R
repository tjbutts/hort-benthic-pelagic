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
hort_field

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy_low = hort_field %>%
  select(pond_id, doy, chla_10_30) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = chla_10_30)
testy_low

refy_low = hort_field %>%
  select(pond_id, doy, chla_10_30) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=chla_10_30)
refy_low

## Intermediate Coupling ## 
testy_int = hort_field %>%
  select(pond_id, doy, chla_10_30) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = chla_10_30)
testy_int

refy_int = hort_field %>%
  select(pond_id, doy, chla_10_30) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = chla_10_30)
refy_int

## High Coupling ## 
testy_high = hort_field %>%
  select(pond_id, doy, chla_10_30) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = chla_10_30)
testy_high

refy_high = hort_field %>%
  select(pond_id, doy, chla_10_30) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=chla_10_30)
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
# data - need to include NAs for rollign window # 
metab_gross = read_csv('daily-metabolism_data_robertcorrected.csv') 
metab_gross$doy # Look for missing day of years # 
# No missing days 

# Replace values with flag =1 with NAs # 
metab_gross$GPP[metab_gross$flag == 1] <- NA
metab_gross$R[metab_gross$flag == 1] <- NA
metab_gross$NEP[metab_gross$flag == 1] <- NA
metab_gross # False data now removed and replaced with NAs 

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

# Respiration #===================
# data - need to include NAs for rollign window # 
metab_gross = read_csv('daily-metabolism_data_robertcorrected.csv') 
metab_gross$doy # Look for missing day of years # 
# No missing days 

# Replace values with flag =1 with NAs # 
metab_gross$GPP[metab_gross$flag == 1] <- NA
metab_gross$R[metab_gross$flag == 1] <- NA
metab_gross$NEP[metab_gross$flag == 1] <- NA
metab_gross # False data now removed and replaced with NAs 

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy3_low = metab %>%
  select(pond_id, doy, R) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = R)
testy3_low

refy3_low = metab %>%
  select(pond_id, doy, R) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=R)
refy3_low

## Intermediate Coupling ## 
testy3_int = metab %>%
  select(pond_id, doy, R) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = R)
testy3_int

refy3_int = metab %>%
  select(pond_id, doy, R) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = R)
refy3_int

## High Coupling ## 
testy3_high = metab %>%
  select(pond_id, doy, R) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = R)
testy3_high

refy3_high = metab %>%
  select(pond_id, doy, R) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=R)
refy3_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low3 = mwdistdiffz(testy = testy3_low, refy = refy3_low, 
                       wwidth = 7, 
                       ddiff_method = 'integral')
rda_int3 = mwdistdiffz(testy = testy3_int, refy = refy3_int, 
                       wwidth = 7, 
                       ddiff_method = 'integral')
rda_high3 = mwdistdiffz(testy = testy3_high, refy = refy3_high, 
                        wwidth = 7, 
                        ddiff_method = 'integral')

# Net Ecosystem Production #===================
# data - need to include NAs for rollign window # 
metab_gross = read_csv('daily-metabolism_data_robertcorrected.csv') 
metab_gross$doy # Look for missing day of years # 
# No missing days 

# Replace values with flag =1 with NAs # 
metab_gross$GPP[metab_gross$flag == 1] <- NA
metab_gross$R[metab_gross$flag == 1] <- NA
metab_gross$NEP[metab_gross$flag == 1] <- NA
metab_gross # False data now removed and replaced with NAs 

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy4_low = metab %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = NEP)
testy4_low

refy4_low = metab %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=NEP)
refy4_low

## Intermediate Coupling ## 
testy4_int = metab %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = NEP)
testy4_int

refy4_int = metab %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = NEP)
refy4_int

## High Coupling ## 
testy4_high = metab %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = NEP)
testy4_high

refy4_high = metab %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=NEP)
refy4_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low4 = mwdistdiffz(testy = testy4_low, refy = refy4_low, 
                       wwidth = 7, 
                       ddiff_method = 'integral')
rda_int4 = mwdistdiffz(testy = testy4_int, refy = refy4_int, 
                       wwidth = 7, 
                       ddiff_method = 'integral')
rda_high4 = mwdistdiffz(testy = testy4_high, refy = refy4_high, 
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

high_col_C = rgb(75, 31, 110, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(75, 31, 110, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(75, 31, 110, max = 255, alpha = 255) #Pond C, Pond E

col=rgb(255,48,48, max=255, alpha=75, names= 'firebrick1') # Extended heat period 

## ============ Plot Margins ================= ##
# Window for checking plot 
windows(height = 6, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure4.pdf", 
 #   height = 4, 
 #    width = 6)

# Set dimensions for figure array # 
par(mfrow =c(4,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0)) 

## Chlorophyll-a RDA plot ##==========================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', col.axis = transparent, yaxt = 'n', 
     lwd=3, col=low_col, data=rda_low)
axis(side = 2, at=c(-2, 0, 2, 4, 6))
mtext(side = 2, line = 3.2, 
      expression('Chlorophyll-'~italic(a)), cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)
text(141, 5.8, 'A', font = 2)
mtext(side = 3, line = 0.1, 'Low Complexity', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', col.axis = transparent,
     lwd=3, col=int_col, data=rda_int)
mtext(side = 3, line = 0.1, 'Intermediate', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'B', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', col.axis = transparent, 
     lwd=3, col=high_col, data=rda_high)
mtext(side = 3, line = 0.1, 'High Complexity', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,0), lty = 2, lwd = 2)
lines(c(223,223), c(1.7,20000), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'C', font = 2)

# Add in Labels 
text(155, 2.5, 'Response')
text(230, 0.9, 'Recovery')

## GPP RDA plot ##============================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n',
     lwd=3, col=low_col, data=rda_low2, col.axis = transparent)
mtext(side = 2, line = 3.2, 'GPP', cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
axis(side = 2, at=c(-2, 0, 2, 4, 6))

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'D', font = 2)

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
      ylab = '', xlab = '', yaxt = 'n', 
     lwd=3, col=int_col, data=rda_int2, col.axis = transparent)
axis(side = 2, at = c(-2, 0, 2, 4, 6), labels = F)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'E', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
      ylab = '', xlab = '', yaxt = 'n', 
     lwd=3, col=high_col, data=rda_high2, col.axis = transparent)
axis(side = 2, at=c(-2, 0,2,4,6), labels = F)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'F', font = 2)

## Respiration RDA plot ##==========================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n', col.axis = transparent, 
     lwd=3, col=low_col, data=rda_low3)
axis(side = 2, at=c(-2, 0, 2, 4, 6))
mtext(side = 2, line = 3.2, 
      expression('R'), cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)
text(141, 5.8, 'G', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), col.axis = transparent,
     ylab = '', xlab = '', 
     lwd=3, col=int_col, data=rda_int3)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'H', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), col.axis = transparent, 
     ylab = '', xlab = '', 
     lwd=3, col=high_col, data=rda_high3)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'I', font = 2)

## Net Ecosystem Production RDA plot ##==========================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n', 
     lwd=3, col=low_col, data=rda_low4)
axis(side = 2, at=c(-2, 0, 2, 4, 6))
mtext(side = 2, line = 3.2, 
      expression('NEP'), cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)
text(141, 5.8, 'J', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', 
     lwd=3, col=int_col, data=rda_int4)
mtext(side = 1, line = 3, 'Last Day of Year in\n7-day rolling window', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'K', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', 
     lwd=3, col=high_col, data=rda_high4)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'L', font = 2)
# Create plot in specified file path # 
#dev.off()

# Pull Day of Years for Response and Recovery # 

# Summary Figure for Response, Recovery Option 2 #===================================
# Data pulled from: 
  # rda = chlorophyll-a
  # rda2 = GPP 
  # rda3 = Respiration
  # rda4 = NEP 
hort_rdasum # NA here is no response detected or no recovery detected after response 
hort_rdasum.filt = hort_rdasum %>% filter(treatment != "high")
hort_rdasum.filt # no responses detected in high so don't plot 

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

high_col_C = rgb(75, 31, 110, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(75, 31, 110, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(75, 31, 110, max = 255, alpha = 255) #Pond C, Pond E

col=rgb(255,48,48, max=255, alpha=75, names= 'firebrick1') # Extended heat period 

# Window for checking plot 
windows(height = 4, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure4.pdf", 
#   height = 4, 
#    width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0)) 

## chlorophyll-a Response # ==================
chl_rda = hort_rdasum %>% 
  filter(variable == "chla")
chl_rda$treatment <- factor(chl_rda$treatment, levels = c("low", "int"))

# shape data into matrix 
matrix_chl <- with(chl_rda, tapply(response_d, list(treatment, pulse), sum, na.rm = TRUE))

colnames(matrix_chl) <- c("Post First Pulse", "Post Second Pulse")

  # Create the barplot
barplot(matrix_chl, beside = TRUE, col = c(low_col, int_col), 
        ylim = c(0, 25), xaxt = 'n', 
        xlab = "", ylab = "Days")
box()
text(6, 24, 'A', font = 2)
 

mtext(side = 3, expression('Chlorophyll-'~italic(a)))  
axis(side = 1, at = c(2, 5), tick = T, labels = F)
mtext(side = 2, line = 3, "Time to Response")
mtext(side = 2, line = 1.9, "(Days)")

## GPP Response #========================
gpp_rda = hort_rdasum %>% 
  filter(variable == "gpp")
gpp_rda$treatment <- factor(gpp_rda$treatment, levels = c("low", "int"))

# shape data into matrix 
matrix_gpp <- with(gpp_rda, tapply(response_d, list(treatment, pulse), sum, na.rm = TRUE))

colnames(matrix_gpp) <- c("Post First Pulse", "Post Second Pulse")

# Create the barplot
barplot(matrix_gpp, beside = TRUE, col = c(low_col, int_col), 
        ylim = c(0, 25),  xaxt = 'n', 
        xlab = "", ylab = "Days")
box()
text(6, 24, 'B', font = 2)

# get bar positions info 
text(x = 1.4, y = 0, labels = "n.d.", pos = 3, col = "black", cex = 1.5)
text(x = 4.4, y = 0, labels = "n.d.", pos = 3, col = "black", cex = 1.5)

mtext(side = 3, "GPP")
axis(side = 1, at = c(2, 5), tick = T, labels = F)

## R Response #==============================
r_rda = hort_rdasum %>% 
  filter(variable == "r")
r_rda$treatment <- factor(r_rda$treatment, levels = c("low", "int"))

# shape data into matrix 
matrix_r <- with(r_rda, tapply(response_d, list(treatment, pulse), sum, na.rm = TRUE))

colnames(matrix_r) <- c("Post First Pulse", "Post Second Pulse")

# Create the barplot
barplot(matrix_r, beside = TRUE, col = c(low_col, int_col), 
        ylim = c(0, 25),  xaxt = 'n', 
        xlab = "", ylab = "Days")
box()
text(6, 24, 'C', font = 2)
# get bar positions info 
axis(side = 1, at = c(2, 5), tick = T, labels = F)
text(x = 1.5, y = 0, labels = "n.d.", pos = 3, col = "black", cex = 1.5)
text(x = 2.5, y = 0, labels = "n.d.", pos = 3, col = "black", cex = 1.5)
text(x = 4.5, y = 0, labels = "n.d.", pos = 3, col = "black", cex = 1.5)
mtext(side = 3, "R")

# Chlorophyll-a Recovery #======================

# shape data into matrix 
matrix_chl <- with(chl_rda, tapply(recovery_d, list(treatment, pulse), sum, na.rm = TRUE))

colnames(matrix_chl) <- c("Post First Pulse", "Post Second Pulse")

# Create the barplot
barplot(matrix_chl, beside = TRUE, col = c(low_col, int_col), 
        ylim = c(0, 25),  xaxt = 'n', 
        xlab = "", ylab = "Days")
box()

# get bar positions info 
text(x = 5.5, y = 0, labels = "n.d", pos = 3, col = "black", cex = 1.5)
mtext(side = 2, line = 3, "Time to Recovery")
mtext(side = 2, line = 1.9, "(Days)") 
axis(side = 1, at = c(2, 5), tick = T, labels = c("Post 1st Pulse", "Post 2nd Pulse"))
text(6, 24, 'D', font = 2)

## Gross Primary Production #========================== 

# shape data into matrix 
matrix_gpp <- with(gpp_rda, tapply(recovery_d, list(treatment, pulse), sum, na.rm = TRUE))

colnames(matrix_gpp) <- c("Post 1st Pulse", "Post 2nd Pulse")

# Create the barplot
barplot(matrix_gpp, beside = TRUE, col = c(low_col, int_col), 
        ylim = c(0, 25), xaxt = 'n',
        xlab = "", ylab = "Days")
box()
text(6, 24, 'E', font = 2)

# get bar positions info 
text(x = 1.4, y = 0, labels = "NA", pos = 3, col = "black", cex = 1.5)
text(x = 4.4, y = 0, labels = "NA", pos = 3, col = "black", cex = 1.5)
axis(side = 1, at = c(2, 5), tick = T, labels = c("Post 1st Pulse", "Post 2nd Pulse"))

## Respiration #===============================


# shape data into matrix 
matrix_r <- with(r_rda, tapply(recovery_d, list(treatment, pulse), sum, na.rm = TRUE))

colnames(matrix_r) <- c("Post First Pulse", "Post Second Pulse")

# Create the barplot
barplot(matrix_r, beside = TRUE, col = c(low_col, int_col), 
        ylim = c(0, 25), xaxt = 'n',
        xlab = "", ylab = "Days")
box()
text(6, 24, 'F', font = 2)

# get bar positions info 
text(x = 1.5, y = 0, labels = "NA", pos = 3, col = "black", cex = 1.5)
text(x = 2.5, y = 0, labels = "NA", pos = 3, col = "black", cex = 1.5)
text(x = 4.5, y = 0, labels = "NA", pos = 3, col = "black", cex = 1.5)
axis(side = 1, at = c(2, 5), tick = T, labels = c("Post 1st Pulse", "Post 2nd Pulse"))

legend("topleft", legend = c('low', 'intermediate'), 
       col = c(low_col, int_col), pch = 15, cex = 1.5, pt.cex = 2, bty = 'n')

# Summary Figure for Response, Recovery Option 1 #===================================
# Data pulled from: 
# rda = chlorophyll-a
# rda2 = GPP 
# rda3 = Respiration
# rda4 = NEP 
hort_rdasum # NA here is no response detected or no recovery detected after response 
hort_rdasum.filt = hort_rdasum %>% filter(treatment != "high")
hort_rdasum.filt # no responses detected in high so don't plot 

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

high_col_C = rgb(75, 31, 110, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(75, 31, 110, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(75, 31, 110, max = 255, alpha = 255) #Pond C, Pond E

col=rgb(255,48,48, max=255, alpha=75, names= 'firebrick1') # Extended heat period 

windows(height = 8, width = 7) 
# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure4.pdf", 
#   height = 4, 
#    width = 6)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(1.5,1.5,1,0.1), oma = c(4,3,1,1))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Put together 3x3 plot for summary figure # 
hort_rdasum
chl.low <- hort_rdasum %>% 
  filter(treatment == "low") %>% 
  filter(variable == "chla") %>% 
  mutate(pulse = factor(pulse, levels = c("post_first", "post_second")))
chl.low

# Transpose data for stacking
bar_data <- t(as.matrix(chl.low[, c("response_d", "recovery_d")]))
rownames(bar_data) <- c("Response", "Recovery")
bar_data <- bar_data[, c(2,1)]

# Create stacked horizontal bar plot
barplot(
  bar_data,
  horiz = TRUE,
  beside = FALSE,
  col = c(low_col, "gray50"),
  border = NA,
  names.arg = c("Second Pulse", "First Pulse"),
  cex.names = 1.2,
  xlab = "Days", 
  xlim = c(0, 45)
)
box()
mtext(side = 2, expression('Chlorophyll-'~italic(a)), line = 1.5)
text(12.2, 1.55, "Persistence", font = 2, cex = 1.5)
text(19, 0.35, "Resilience", font = 2, col = "white", cex = 1.5)
mtext(side = 3, "Low Complexity", line = 0.1)
text(43, 2.4, "A", font = 2, cex = 1.2)
#legend("topright", legend = rownames(bar_data), fill = c(low_col, "gray50"))

chl.int <- hort_rdasum %>% 
  filter(treatment == "int") %>% 
  filter(variable == "chla") %>% 
  mutate(pulse = factor(pulse, levels = c("post_first", "post_second")))
chl.int

# Transpose data for stacking
bar_data <- t(as.matrix(chl.int[, c("response_d", "recovery_d")]))
rownames(bar_data) <- c("Response", "Recovery")
bar_data <- bar_data[, c(2,1)]

# Create stacked horizontal bar plot
barplot(
  bar_data,
  horiz = TRUE,
  beside = FALSE,
  col = c(int_col, "gray50"),
  border = NA,
  names.arg = c("Second Pulse", "First Pulse"),
  cex.names = 1.2,
  xlab = "Days", 
  xlim = c(0, 45)
)
box()
text(24, 0.6, "NA", font = 2, col = "gray50", cex = 2)
mtext(side = 3, "Intermediate", line = 0.1)

#mtext(side = 2, expression('Chlorophyll-'~italic(a)), line = 1.5)
text(43, 2.4, "B", font = 2, cex = 1.2)

chl.hi <- hort_rdasum %>% 
  filter(treatment == "high") %>% 
  filter(variable == "chla") %>% 
  mutate(pulse = factor(pulse, levels = c("post_first", "post_second")))
chl.hi

# Transpose data for stacking
bar_data <- t(as.matrix(chl.hi[, c("response_d", "recovery_d")]))
rownames(bar_data) <- c("Response", "Recovery")
bar_data <- bar_data[, c(2,1)]

# Create stacked horizontal bar plot
barplot(
  bar_data,
  horiz = TRUE,
  beside = FALSE,
  col = c(high_col, "gray50"),
  border = NA,
  names.arg = c("Second Pulse", "First Pulse"),
  cex.names = 1.2,
  xlab = "Days", 
  xlim = c(0, 45)
)
box()
text(6, 0.8, "n.d.", font = 2, col = high_col, cex = 2)
text(6, 2, "n.d.", font = 2, col = high_col, cex = 2)
mtext(side = 3, "High Complexity")

#mtext(side = 2, expression('Chlorophyll-'~italic(a)), line = 1.5)
text(43, 2.4, "C", font = 2, cex = 1.2)

## GPP ## 
gpp.low <- hort_rdasum %>% 
  filter(treatment == "low") %>% 
  filter(variable == "gpp") %>% 
  mutate(pulse = factor(pulse, levels = c("post_first", "post_second")))
gpp.low

# Transpose data for stacking
bar_data <- t(as.matrix(gpp.low[, c("response_d", "recovery_d")]))
rownames(bar_data) <- c("Response", "Recovery")
bar_data <- bar_data[, c(2,1)]

# Create stacked horizontal bar plot
barplot(
  bar_data,
  horiz = TRUE,
  beside = FALSE,
  col = c(low_col, "gray50"),
  border = NA,
  names.arg = c("Second Pulse", "First Pulse"),
  xlab = "Days", 
  cex.names = 1.2,
  xlim = c(0, 45)
)
box()
mtext(side = 2, "GPP", line = 1.5)
#mtext(side = 1, "Days", line = 1.8)
#text(23.5, 0.6, "NA", font = 2, col = "gray50", cex = 2)
#mtext(side = 3, "Intermediate", line = 0.1)
text(43, 2.4, "D", font = 2, cex = 1.2)
text(6, 0.8, "n.d.", font = 2, col = low_col, cex = 2)
text(6, 2, "n.d.", font = 2, col = low_col, cex = 2)

gpp.int <- hort_rdasum %>% 
  filter(treatment == "int") %>% 
  filter(variable == "gpp") %>% 
  mutate(pulse = factor(pulse, levels = c("post_first", "post_second")))
gpp.int

# Transpose data for stacking
bar_data <- t(as.matrix(gpp.int[, c("response_d", "recovery_d")]))
rownames(bar_data) <- c("Response", "Recovery")
bar_data <- bar_data[, c(2,1)]

# Create stacked horizontal bar plot
barplot(
  bar_data,
  horiz = TRUE,
  beside = FALSE,
  col = c(int_col, "gray50"),
  border = NA,
  names.arg = c("Second Pulse", "First Pulse"),
  xlab = "Days", 
  cex.names = 1.2,
  xlim = c(0, 45)
)
box()

#text(23.5, 0.6, "NA", font = 2, col = "gray50", cex = 2)
#mtext(side = 3, "Intermediate", line = 0.1)
text(43, 2.4, "E", font = 2, cex = 1.2)

gpp.hi <- hort_rdasum %>% 
  filter(treatment == "high") %>% 
  filter(variable == "gpp") %>% 
  mutate(pulse = factor(pulse, levels = c("post_first", "post_second")))
gpp.hi

# Transpose data for stacking
bar_data <- t(as.matrix(gpp.hi[, c("response_d", "recovery_d")]))
rownames(bar_data) <- c("Response", "Recovery")
bar_data <- bar_data[, c(2,1)]

# Create stacked horizontal bar plot
barplot(
  bar_data,
  horiz = TRUE,
  beside = FALSE,
  col = c(high_col, "gray50"),
  border = NA,
  names.arg = c("Second Pulse", "First Pulse"),
  cex.names = 1.2,
  xlab = "Days", 
  xlim = c(0, 45)
)
box()
text(6, 0.8, "n.d.", font = 2, col = high_col, cex = 2)
text(6, 2, "n.d.", font = 2, col = high_col, cex = 2)

#mtext(side = 2, expression('Chlorophyll-'~italic(a)), line = 1.5)
text(43, 2.4, "F", font = 2, cex = 1.2)

## R ## 
r.low <- hort_rdasum %>% 
  filter(treatment == "low") %>% 
  filter(variable == "r") %>% 
  mutate(pulse = factor(pulse, levels = c("post_first", "post_second")))
r.low

# Transpose data for stacking
bar_data <- t(as.matrix(r.low[, c("response_d", "recovery_d")]))
rownames(bar_data) <- c("Response", "Recovery")
bar_data <- bar_data[, c(2,1)]

# Create stacked horizontal bar plot
barplot(
  bar_data,
  horiz = TRUE,
  beside = FALSE,
  col = c(low_col, "gray50"),
  border = NA,
  names.arg = c("Second Pulse", "First Pulse"),
  cex.names = 1.2,
  xlab = "Days", 
  xlim = c(0, 45)
)
box()
mtext(side = 2, "R", line = 1.5)
mtext(side = 1, "Days", line = 1.8)
text(6, 0.8, "n.d.", font = 2, col = low_col, cex = 2)
text(6, 2, "n.d.", font = 2, col = low_col, cex = 2)
text(43, 2.4, "G", font = 2, cex = 1.2)
#legend("topright", legend = rownames(bar_data), fill = c(low_col, "gray50"))

r.int <- hort_rdasum %>% 
  filter(treatment == "int") %>% 
  filter(variable == "r") %>% 
  mutate(pulse = factor(pulse, levels = c("post_first", "post_second")))
r.int

# Transpose data for stacking
bar_data <- t(as.matrix(r.int[, c("response_d", "recovery_d")]))
rownames(bar_data) <- c("Response", "Recovery")
bar_data <- bar_data[, c(2,1)]

# Create stacked horizontal bar plot
barplot(
  bar_data,
  horiz = TRUE,
  beside = FALSE,
  col = c(int_col, "gray50"),
  border = NA,
  names.arg = c("Second Pulse", "First Pulse"),
  cex.names = 1.2,
  xlab = "Days", 
  xlim = c(0, 45)
)
box()

mtext(side = 1, "Days", line = 1.8)
text(5, 2, "n.d.", cex = 2, col = int_col, font =2)
#text(23.5, 0.6, "NA", font = 2, col = "gray50", cex = 2)
#mtext(side = 3, "Intermediate", line = 0.1)
text(43, 2.4, "H", font = 2, cex = 1.2)

r.high <- hort_rdasum %>% 
  filter(treatment == "high") %>% 
  filter(variable == "r") %>% 
  mutate(pulse = factor(pulse, levels = c("post_first", "post_second")))
r.high

# Transpose data for stacking
bar_data <- t(as.matrix(r.high[, c("response_d", "recovery_d")]))
rownames(bar_data) <- c("Response", "Recovery")
bar_data <- bar_data[, c(2,1)]

# Create stacked horizontal bar plot
barplot(
  bar_data,
  horiz = TRUE,
  beside = FALSE,
  col = c(high_col, "gray50"),
  border = NA,
  names.arg = c("Second Pulse", "First Pulse"),
  cex.names = 1.2,
  xlab = "Days", 
  xlim = c(0, 45)
)
box()

mtext(side = 1, "Days", line = 1.8)
text(5, 2, "n.d.", cex = 2, col = high_col, font =2)
text(5, 0.8, "n.d.", cex = 2, col = high_col, font =2)
#text(23.5, 0.6, "NA", font = 2, col = "gray50", cex = 2)
#mtext(side = 3, "highermediate", line = 0.1)
text(43, 2.4, "I", font = 2, cex = 1.2)
