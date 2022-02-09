## Hort Farm Ecosystem Resilience Project ###
# Code originally written by GM Wilkinson January 2022

#Experimental Context =================================
# Exploratory time series of: 
# Nutrient concentrations
# Nutrient ratios
# DOC (when we get the data)

# Read in the relevant data and packages
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

hort_field = read_csv('hort20_surface_dat.csv')

# Because we will be plotting by pond, make some separate data frames to make life easier
fieldA = hort_field %>% #pulse, int
  filter(pond_id == "A") %>%
  filter(!(is.na(tp))) %>%
  mutate(tp_diff = tp - lag(tp),
         srp_diff = srp - lag(srp),
         tn_diff = tn - lag(tn),
         nox_diff = nox - lag(nox)) %>%
  mutate(tp_scale = scale(tp_diff, center = TRUE, scale = TRUE),
         srp_scale = scale(srp_diff, center = TRUE, scale = TRUE),
         tn_scale = scale(tn_diff, center = TRUE, scale = TRUE),
         nox_scale = scale(nox_diff, center = TRUE, scale = TRUE))

fieldB = hort_field %>% #pulse, low
  filter(pond_id == "B") %>%
  filter(!(is.na(tp))) %>%
  mutate(tp_diff = tp - lag(tp),
         srp_diff = srp - lag(srp),
         tn_diff = tn - lag(tn),
         nox_diff = nox - lag(nox)) %>%
  mutate(tp_scale = scale(tp_diff, center = TRUE, scale = TRUE),
         srp_scale = scale(srp_diff, center = TRUE, scale = TRUE),
         tn_scale = scale(tn_diff, center = TRUE, scale = TRUE),
         nox_scale = scale(nox_diff, center = TRUE, scale = TRUE))

fieldC = hort_field %>% #pulse, high
  filter(pond_id == "C") %>%
  filter(!(is.na(tp))) %>%
  mutate(tp_diff = tp - lag(tp),
         srp_diff = srp - lag(srp),
         tn_diff = tn - lag(tn),
         nox_diff = nox - lag(nox)) %>%
  mutate(tp_scale = scale(tp_diff, center = TRUE, scale = TRUE),
         srp_scale = scale(srp_diff, center = TRUE, scale = TRUE),
         tn_scale = scale(tn_diff, center = TRUE, scale = TRUE),
         nox_scale = scale(nox_diff, center = TRUE, scale = TRUE))

fieldD = hort_field %>% #ref, int
  filter(pond_id == "D") %>%
  filter(!(is.na(tp))) %>%
  mutate(tp_diff = tp - lag(tp),
         srp_diff = srp - lag(srp),
         tn_diff = tn - lag(tn),
         nox_diff = nox - lag(nox)) %>%
  mutate(tp_scale = scale(tp_diff, center = TRUE, scale = TRUE),
         srp_scale = scale(srp_diff, center = TRUE, scale = TRUE),
         tn_scale = scale(tn_diff, center = TRUE, scale = TRUE),
         nox_scale = scale(nox_diff, center = TRUE, scale = TRUE))

fieldE = hort_field %>% #ref, high
  filter(pond_id == "E") %>%
  filter(!(is.na(tp))) %>%
  mutate(tp_diff = tp - lag(tp),
         srp_diff = srp - lag(srp),
         tn_diff = tn - lag(tn),
         nox_diff = nox - lag(nox)) %>%
  mutate(tp_scale = scale(tp_diff, center = TRUE, scale = TRUE),
         srp_scale = scale(srp_diff, center = TRUE, scale = TRUE),
         tn_scale = scale(tn_diff, center = TRUE, scale = TRUE),
         nox_scale = scale(nox_diff, center = TRUE, scale = TRUE))

fieldF = hort_field %>% #ref, low
  filter(pond_id == "F") %>%
  filter(!(is.na(tp))) %>%
  mutate(tp_diff = tp - lag(tp),
         srp_diff = srp - lag(srp),
         tn_diff = tn - lag(tn),
         nox_diff = nox - lag(nox)) %>%
  mutate(tp_scale = scale(tp_diff, center = TRUE, scale = TRUE),
         srp_scale = scale(srp_diff, center = TRUE, scale = TRUE),
         tn_scale = scale(tn_diff, center = TRUE, scale = TRUE),
         nox_scale = scale(nox_diff, center = TRUE, scale = TRUE))

#Colors for data visualization
#Ref: lty = 3, pulse: lty = 1
low_col = "#4AA651" #Pond B, Pond F
int_col ="#2c7fb8" #Pond A, pond D
high_col = "#081d58" #Pond C, Pond E

#Plot the nutrient data ==============================
windows(height = 10, width = 9)
par(mfrow = c(4,3), omi = c(0.7,0.7,0.3,0.2), mai = c(0.1,0.1,0.1,0.1))

# ===== TOTAL PHOSPHORUS ===========
plot(fieldB$doy, fieldB$tp_diff, type = "l",
     ylim = c(-50,50),
     xaxt = 'n',
     col = low_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-100,-100,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-100,-100,200,200), col = "gray75", border= NA)
lines(fieldB$doy, fieldB$tp_diff, col = low_col, lwd = 2, lty = 1)
lines(fieldF$doy, fieldF$tp_diff, col = low_col, lwd = 2, lty = 3); box()
mtext(side = 3, line = 0.3, "Low Coupling", font = 2)
mtext(side = 2, line = 2.7, "Total P (ug/L)", font = 2)

plot(fieldA$doy, fieldA$tp_diff, type = "l", 
     ylim = c(-50,50),
      xaxt = 'n', yaxt = 'n',
     col = int_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-100,-100,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-100,-100,200,200), col = "gray75", border= NA)
lines(fieldA$doy, fieldA$tp_diff, col = int_col, lwd = 2, lty = 1)
lines(fieldD$doy, fieldD$tp_diff, col = int_col, lwd = 2, lty = 3); box()
mtext(side = 3, line = 0.3, "Int. Coupling", font = 2)

plot(fieldC$doy, fieldC$tp_diff, type = "l",
     ylim = c(-50,50),
     xaxt = 'n', yaxt = 'n',
     col = high_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-100,-100,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-100,-100,200,200), col = "gray75", border= NA)
lines(fieldC$doy, fieldC$tp_diff, col = high_col, lwd = 2, lty = 1)
lines(fieldE$doy, fieldE$tp_diff, col = high_col, lwd = 2, lty = 3); box()
mtext(side = 3, line = 0.3, "High Coupling", font = 2)

# ===== DISSOLVED PHOSPHORUS ===========
plot(fieldB$doy, fieldB$srp_diff, type = "l", 
     ylim = c(-10,10), xaxt = 'n',
     pch = 20, col = low_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldB$doy, fieldB$srp_diff, col = low_col, lwd = 2, lty = 1)
lines(fieldF$doy, fieldF$srp_diff, col = low_col, lwd = 2, lty = 3); box()
mtext(side = 2, line = 2.7, "Dissolved P (ug/L)", font = 2)

plot(fieldA$doy, fieldA$srp_diff, type = "l", 
     ylim = c(-10,10), xaxt = 'n', yaxt = 'n',
     pch = 20, col = int_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldA$doy, fieldA$srp_diff, col = int_col, lwd = 2, lty = 1)
lines(fieldD$doy, fieldD$srp_diff, col = int_col, lwd = 2, lty = 3); box()

plot(fieldC$doy, fieldC$srp_diff, type = "l", 
     ylim = c(-10,10), xaxt = 'n', yaxt = 'n',
     pch = 20, col = high_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldC$doy, fieldC$srp_diff, col = high_col, lwd = 2, lty = 1)
lines(fieldE$doy, fieldE$srp_diff, col = high_col, lwd = 2, lty = 3); box()

# ===== TOTAL NITROGEN ===========
plot(fieldB$doy, fieldB$tn_diff, type = "l", 
     ylim = c(-1,1), xaxt = 'n',
     pch = 20, col = low_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldB$doy, fieldB$tn_diff, col = low_col, lwd = 2, lty = 1)
lines(fieldF$doy, fieldF$tn_diff, col = low_col, lwd = 2, lty = 3); box()
mtext(side = 2, line = 2.7, "Total N (mg/L)", font = 2)

plot(fieldA$doy, fieldA$tn_diff, type = "l", 
     ylim = c(-1,1), xaxt = 'n', yaxt = 'n',
     pch = 20, col = int_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldA$doy, fieldA$tn_diff, col = int_col, lwd = 2, lty = 1)
lines(fieldD$doy, fieldD$tn_diff, col = int_col, lwd = 2, lty = 3); box()

plot(fieldC$doy, fieldC$tn_diff, type = "l", 
     ylim = c(-1,1), xaxt = 'n', yaxt = 'n',
     pch = 20, col = high_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldC$doy, fieldC$tn_diff, col = high_col, lwd = 2, lty = 1)
lines(fieldE$doy, fieldE$tn_diff, col = high_col, lwd = 2, lty = 3); box()

# ===== NITRATE & AMMONIUM ===========
plot(fieldB$doy, fieldB$nox_diff, type = "l", 
     ylim = c(-1,1), 
     pch = 20, col = low_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldB$doy, fieldB$nox_diff, col = low_col, lwd = 2, lty = 1)
lines(fieldF$doy, fieldF$nox_diff, col = low_col, lwd = 2, lty = 3); box()
mtext(side = 2, line = 2.7, "Dissolved N (mg/L)", font = 2)

plot(fieldA$doy, fieldA$nox_diff, type = "l", 
     ylim = c(-1,1), yaxt = 'n',
     pch = 20, col = int_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldA$doy, fieldA$nox_diff, col = int_col, lwd = 2, lty = 1)
lines(fieldD$doy, fieldD$nox_diff, col = int_col, lwd = 2, lty = 3); box()
mtext(side = 1, line = 2.7, "Day of Year, 2020", font = 2)

plot(fieldC$doy, fieldC$nox_diff, type = "l", 
     ylim = c(-1,1), yaxt = 'n',
     pch = 20, col = high_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldC$doy, fieldC$nox_diff, col = high_col, lwd = 2, lty = 1)
lines(fieldE$doy, fieldE$nox_diff, col = high_col, lwd = 2, lty = 3); box()
