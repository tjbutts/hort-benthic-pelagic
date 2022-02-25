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

# first run with Sond profile data (chla - average between 10 -30 cm) #======================
# Low Coupling - Sonde #======================= 
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

mwd1_lo = mwdistdiffz(testy = testy_low, refy = refy_low,wwidth = 10, ddiff_method = 'dist')
mwd2_lo = mwdistdiffz(testy = testy_low, refy = refy_low,wwidth = 10,ddiff_method = 'integral')
plot_mwddz(mwd1_lo, diff_ts = 'zz')

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2) 
abline(h=0.5)


# Int Coupling - Sonde #========================= 
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

plot_mwddz(mwd2_int)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=2)
abline(h=0.5)

# High Coupling - Sonde #==========================
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

mwd2_int = mwdistdiffz(testy = testy_int, refy = refy_int, wwidth = 10, ddiff_method = 'integral')
plot_mwddz(mwd2_int, diff_ts = 'zz')
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=1.5) 
abline(h=0)

mwd2_hi = mwdistdiffz(testy = testy_high, refy = refy_high, wwidth = 10, ddiff_method = 'integral')
plot_mwddz(mwd2_hi, diff_ts = 'zz')
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)
abline(h=1.5) 
abline(h=0)

