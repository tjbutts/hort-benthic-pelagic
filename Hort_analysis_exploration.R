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