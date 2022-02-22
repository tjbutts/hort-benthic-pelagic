## Hort Farm Analysis Exploration ## 

## Disturhf - Walter & Buelo - ##===============
install.packages('remotes')
remotes::install_github('jonathan-walter/disturbhf')
library(disturbhf)

# Functions: alarmfilter, decimal_doy, disturbalarm, mwdistdiffz, plot_mwddz 
# mwdistdiffz_ks = computes differences between the continuous distribution functions (cdf) for observations within a moving window and a reference distribution, and the z-scores of differences relative to samples of the reference distribution
### -> Used for identifying recovery times from disturbance in time series data 
# disturbalarm = interprets output from mwdistdiffz to detect disturbances and recovery time
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

mwd1 = mwdistdiffz(testy = testy_low, refy = refy_low,wwidth = 3 ,ddiff_method = 'dist')
mwd2 = mwdistdiffz(testy = testy_low, refy = refy_low,wwidth = 3 ,ddiff_method = 'integral')
plot_mwddz(mwd1)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)

plot_mwddz(mwd2)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)

# Disturb Alarm # 
dis1 = disturbalarm(mwd1)
dis2 = disturbalarm(mwd2)

# Alarm Filter #

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

mwd1 = mwdistdiffz(testy = testy_high, refy = refy_high,wwidth = 3 ,ddiff_method = 'dist')
mwd2 = mwdistdiffz(testy = testy_high, refy = refy_high,wwidth = 3 ,ddiff_method = 'integral')
plot_mwddz(mwd1)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)

plot_mwddz(mwd2)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)

# Disturb Alarm # 
disturbalarm(mwd1)
disturbalarm(mwd2)

# Alarm Filter # 