## Hort Farm Analysis Exploration ## 

## Disturhf - Walter & Buelo - ##===============
install.packages('remotes')
remotes::install_github('jonathan-walter/disturbhf')
library(disturbhf)

# Functions: alarmfilter, decimal_doy, disturbalarm, mwdistdiffz, plot_mwddz 
# mwdistdiffz_ks = computes differences between the continuous distribution functions (cdf) for observations within a moving window and a reference distribution, and the z-scores of differences relative to samples of the reference distribution
### -> Used for identifying recovery times from disturbance in time series data 
# disturbalarm = interprets output from mwdistdiffz to detect disturbances and recovery time
## Determine the thresholds - defaults 0.5 and 2 (z-score values)
# alarmfilter = filters alarm signals to reduce errors from transient events; used to filter outpuf from disturbalarm
# decimal_doy = computes the decimal day of year using hours, minutes, and seconds 
# plot_mwddz = plot of the difference between test and reference distribution through time 

hort_field
col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')

# first run with Sonde profile data (chla - average between 10 -30 cm) #
# Low Coupling - Sonde #
testy_low = hort_field %>%
  select(pond_id, doy, chla) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = chla)
testy_low

refy_low = hort_field %>%
  select(pond_id, doy, chla) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=chla)
refy_low

mwd1_lo = mwdistdiffz(testy = testy_low, refy = refy_low,wwidth = 7, ddiff_method = 'dist')
mwd2_lo = mwdistdiffz(testy = testy_low, refy = refy_low,wwidth = 7,ddiff_method = 'integral')
plot_mwddz(mwd1_lo, diff_ts = 'zz')
plot_mwddz(mwd2_lo, diff_ts = 'zz')
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2) 
abline(h=0.5)
rect(185,-2,190,15, col=col, border=NA)

# Int Coupling - Sonde #
testy_int = hort_field %>%
  select(pond_id, doy, chla) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = chla)
testy_int

refy_int = hort_field %>%
  select(pond_id, doy, chla) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = chla)
refy_int

mwd1_int = mwdistdiffz(testy = testy_int, refy = refy_int,wwidth = 9, ddiff_method = 'dist')
mwd2_int = mwdistdiffz(testy = testy_int, refy = refy_int,wwidth = 7, ddiff_method = 'integral')
plot_mwddz(mwd1_int, diff_ts = 'zz')
plot_mwddz(mwd2_int, diff_ts = 'zz')

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2) 
abline(h=0.5)
rect(185,-2,190,15, col=col, border=NA)


# High Coupling - Sonde #
testy_high = hort_field %>%
  select(pond_id, doy, chla) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = chla)
testy_high

refy_high = hort_field %>%
  select(pond_id, doy, chla) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=chla)
refy_high

mwd1_hi = mwdistdiffz(testy = testy_high, refy = refy_high,wwidth = 14 ,ddiff_method = 'dist')
mwd2_hi = mwdistdiffz(testy = testy_high, refy = refy_high,wwidth = 14 ,ddiff_method = 'integral')
plot_mwddz(mwd1_hi, diff_ts = 'zz')
plot_mwddz(mwd2_hi, diff_ts = 'zz')

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2) 
abline(h=0.5)
col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')
rect(180,-2,195,15, col=col, border = NA)

# Plot in 3x1 array #  
#Plotting Colors
#Colors for data visualization
#Ref: lty = 3, pulse: lty = 1
low_col_B = rgb(74, 166, 81, max = 255, alpha = 180) #Pond B, Pond F
low_col_F = rgb(74, 166, 81, max = 255, alpha = 100) #Pond B, Pond F
low_col = rgb(74, 166, 81, max = 255, alpha = 255) #Pond B, Pond F

int_col_A = rgb(44, 127, 184, max = 255, alpha = 180) #Pond A, pond D
int_col_D = rgb(44, 127, 184, max = 255, alpha = 100) #Pond A, pond D
int_col = rgb(44, 127, 184, max = 255, alpha = 255) #Pond A, pond D

high_col_C = rgb(8, 29, 88, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(8, 29, 88, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(8, 29, 88, max = 255, alpha = 255) #Pond C, Pond E

col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1') # Color for Heatwave 

windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

mwd2_lo = mwdistdiffz(testy = testy_low, refy = refy_low,wwidth = 7, ddiff_method = 'integral')
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     cex.axis=1.2, cex=0.75, ylab = '', xlab = '',
     lwd=4, col=low_col, data=mwd2_lo)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2, lwd=2) 
abline(h=0.5) 
col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')
rect(185,-4,190,15, col=col, border = NA)
mtext(side = 2, line = 3, "Z-scores", cex = 1.25)

mwd2_int = mwdistdiffz(testy = testy_int, refy = refy_int, wwidth = 7, ddiff_method = 'integral')
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     cex.axis=1.2, cex=0.75, ylab = '', xlab = '',
     lwd=4, col=int_col, data=mwd2_int)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2, lwd=2) 
abline(h=0.5)
rect(185,-4,190,15, col=col, border=NA)
mtext(side = 1, line = 3, "Day of Year, 2020", cex = 1.25)

mwd2_hi = mwdistdiffz(testy = testy_high, refy = refy_high, wwidth = 7, ddiff_method = 'integral')
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     cex.axis=1.2, cex=0.75, ylab = '', xlab = '',
     lwd=4, col=high_col, data=mwd2_hi)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2, lwd=2) 
abline(h=0.5)
rect(185,-4,190,15, col=col, border=NA)

# first run with NEP data #
hort_metabolism 

# Low Coupling - NEP # 
testy_low = hort_metabolism %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = NEP)
testy_low

refy_low = hort_metabolism %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=NEP)
refy_low

mwd1_lo = mwdistdiffz(testy = testy_low, refy = refy_low,wwidth = 10, ddiff_method = 'dist')
mwd2_lo = mwdistdiffz(testy = testy_low, refy = refy_low,wwidth = 10,ddiff_method = 'integral')
plot_mwddz(mwd1_lo, diff_ts = 'zz')

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2) 
abline(h=0.5)

# Int Coupling - NEP # 
testy_int = hort_metabolism %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = NEP)
testy_int

refy_int = hort_metabolism %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = NEP)
refy_int

mwd1_int = mwdistdiffz(testy = testy_int, refy = refy_int,wwidth = 10, ddiff_method = 'dist')
mwd2_int = mwdistdiffz(testy = testy_int, refy = refy_int,wwidth = 10, ddiff_method = 'integral')
plot_mwddz(mwd1_int, diff_ts = 'zz')
plot_mwddz(mwd2_int, diff_ts = 'zz')

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2) 
abline(h=0.5)

# High Coupling - NEP #
testy_high = hort_metabolism %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = NEP)
testy_high

refy_high = hort_metabolism %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=NEP)
refy_high

mwd1_hi = mwdistdiffz(testy = testy_high, refy = refy_high,wwidth = 10 ,ddiff_method = 'dist')
mwd2_hi = mwdistdiffz(testy = testy_high, refy = refy_high,wwidth = 10 ,ddiff_method = 'integral')
plot_mwddz(mwd1_hi, diff_ts = 'zz')

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2) 
abline(h=0.5)

plot_mwddz(mwd2_hi)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2)
abline(h=0.5)

# Plot in 3x1 array # 
windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

mwd1_lo = mwdistdiffz(testy = testy_low, refy = refy_low,wwidth = 10, ddiff_method = 'dist')
plot_mwddz(mwd1_lo, diff_ts = 'zz')
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=1.5) 
abline(h=0) 

mwd1_int = mwdistdiffz(testy = testy_int, refy = refy_int, wwidth = 10, ddiff_method = 'dist')
plot_mwddz(mwd1_int, diff_ts = 'zz')
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=1.5) 
abline(h=0)

mwd1_hi = mwdistdiffz(testy = testy_high, refy = refy_high, wwidth = 10, ddiff_method = 'dist')
plot_mwddz(mwd1_hi, diff_ts = 'zz')
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=1.5) 
abline(h=0)

# Plot in 3x1 array # 
windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

mwd2_lo = mwdistdiffz(testy = testy_low, refy = refy_low,wwidth = 10, ddiff_method = 'integral')
plot_mwddz(mwd2_lo, diff_ts = 'zz')
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=1.5) 
abline(h=0) 
abline(h=-1.5)

mwd2_int = mwdistdiffz(testy = testy_int, refy = refy_int, wwidth = 10, ddiff_method = 'integral')
plot_mwddz(mwd2_int, diff_ts = 'zz')
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=1.5) 
abline(h=0)
abline(h=-1.5)

mwd2_hi = mwdistdiffz(testy = testy_high, refy = refy_high, wwidth = 10, ddiff_method = 'integral')
plot_mwddz(mwd2_hi, diff_ts = 'zz')
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=1.5) 
abline(h=0)
abline(h=-1.5) 

# Gephart - Shock Detection #==========================
# Steps: 
## 1. Fit a lowess regression to time series data of the response variable 
## 2. Plot residuals against the time-lagged residuals 
## 3. Use Cook's D to ID extreme points in the regression of residuals versus time-lagged residuals 
hort_field
col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')

## Low coupling ## 
low_pulse = hort_field %>%
  filter(pond_id == 'B') %>%
  select(pond_id, doy, chla) %>% 
  as.data.frame()

low_loess = loess(chla~doy, data=low_pulse)
predict(low_loess)
