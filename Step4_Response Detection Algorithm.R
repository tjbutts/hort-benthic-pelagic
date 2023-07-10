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

high_col_C = rgb(8, 29, 88, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(8, 29, 88, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(8, 29, 88, max = 255, alpha = 255) #Pond C, Pond E

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
mtext(side = 3, line = 0.1, 'Low Coupling', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

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

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'B', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', col.axis = transparent, 
     lwd=3, col=high_col, data=rda_high)
mtext(side = 3, line = 0.1, 'High Coupling', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)

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

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'L', font = 2)
# Create plot in specified file path # 
#dev.off()

