## Hort Farm Ecosystem Resilience Project ###
# Code originally written by GM Wilkinson January 2022

#Experimental Context =================================
# Exploratory time series of: 
  # Nutrient concentrations
  # Nutrient ratios
  # DOC (when we get the data)

# Read in the relevant data and packages
library(tidyverse)
hort_field = read_csv('hort20_surface_dat.csv')

# Because we will be plotting by pond, make some separate data frames to make life easier
fieldA = hort_field %>% #pulse, int
  filter(pond_id == "A") %>%
  filter(!(is.na(tp)))

fieldB = hort_field %>% #pulse, low
  filter(pond_id == "B") %>%
  filter(!(is.na(tp)))

fieldC = hort_field %>% #pulse, high
  filter(pond_id == "C") %>%
  filter(!(is.na(tp)))

fieldD = hort_field %>% #ref, int
  filter(pond_id == "D") %>%
  filter(!(is.na(tp)))

fieldE = hort_field %>% #ref, high
  filter(pond_id == "E") %>%
  filter(!(is.na(tp)))

fieldF = hort_field %>% #ref, low
  filter(pond_id == "F") %>%
  filter(!(is.na(tp)))

#Colors for data visualization
#Ref: lty = 3, pulse: lty = 1
low_col = "#4AA651" #Pond B, Pond F
int_col ="#2c7fb8" #Pond A, pond D
high_col = "#081d58" #Pond C, Pond E

#Plot the nutrient data ==============================
windows(height = 10, width = 9)
par(mfrow = c(4,3), omi = c(0.7,0.7,0.3,0.2), mai = c(0.1,0.1,0.1,0.1))

# ===== TOTAL PHOSPHORUS ===========
plot(fieldB$doy, log10(fieldB$tp), type = "l",
     ylim = c(log10(10), log10(200)), xaxt = 'n',
     col = low_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldB$doy, log10(fieldB$tp), col = low_col, lwd = 2, lty = 1)
lines(fieldF$doy, log10(fieldF$tp), col = low_col, lwd = 2, lty = 3); box()
mtext(side = 3, line = 0.3, "Low Coupling", font = 2)
mtext(side = 2, line = 2.7, "Total P (ug/L)", font = 2)

plot(fieldA$doy, log10(fieldA$tp), type = "l", 
     ylim = c(log10(10), log10(200)), xaxt = 'n', yaxt = 'n',
     col = int_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldA$doy, log10(fieldA$tp), col = int_col, lwd = 2, lty = 1)
lines(fieldD$doy, log10(fieldD$tp), col = int_col, lwd = 2, lty = 3); box()
mtext(side = 3, line = 0.3, "Int. Coupling", font = 2)

plot(fieldC$doy, log10(fieldC$tp), type = "l",
     ylim = c(log10(10), log10(200)), xaxt = 'n', yaxt = 'n',
     col = high_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldC$doy, log10(fieldC$tp), col = high_col, lwd = 2, lty = 1)
lines(fieldE$doy, log10(fieldE$tp), col = high_col, lwd = 2, lty = 3); box()
mtext(side = 3, line = 0.3, "High Coupling", font = 2)

# ===== DISSOLVED PHOSPHORUS ===========
plot(fieldB$doy, fieldB$srp, type = "l", 
     ylim = c(0,10), xaxt = 'n',
     pch = 20, col = low_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldB$doy, fieldB$srp, col = low_col, lwd = 2, lty = 1)
lines(fieldF$doy, fieldF$srp, col = low_col, lwd = 2, lty = 3); box()
mtext(side = 2, line = 2.7, "Dissolved P (ug/L)", font = 2)

plot(fieldA$doy, fieldA$srp, type = "l", 
     ylim = c(0,10), xaxt = 'n', yaxt = 'n',
     pch = 20, col = int_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldA$doy, fieldA$srp, col = int_col, lwd = 2, lty = 1)
lines(fieldD$doy, fieldD$srp, col = int_col, lwd = 2, lty = 3); box()

plot(fieldC$doy, fieldC$srp, type = "l", 
     ylim = c(0,10), xaxt = 'n', yaxt = 'n',
     pch = 20, col = high_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldC$doy, fieldC$srp, col = high_col, lwd = 2, lty = 1)
lines(fieldE$doy, fieldE$srp, col = high_col, lwd = 2, lty = 3); box()

# ===== TOTAL NITROGEN ===========
plot(fieldB$doy, fieldB$tn, type = "l", 
     ylim = c(0,1), xaxt = 'n',
     pch = 20, col = low_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldB$doy, fieldB$tn, col = low_col, lwd = 2, lty = 1)
lines(fieldF$doy, fieldF$tn, col = low_col, lwd = 2, lty = 3); box()
mtext(side = 2, line = 2.7, "Total N (mg/L)", font = 2)

plot(fieldA$doy, fieldA$tn, type = "l", 
     ylim = c(0,1), xaxt = 'n', yaxt = 'n',
     pch = 20, col = int_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldA$doy, fieldA$tn, col = int_col, lwd = 2, lty = 1)
lines(fieldD$doy, fieldD$tn, col = int_col, lwd = 2, lty = 3); box()

plot(fieldC$doy, fieldC$tn, type = "l", 
     ylim = c(0,1), xaxt = 'n', yaxt = 'n',
     pch = 20, col = high_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldC$doy, fieldC$tn, col = high_col, lwd = 2, lty = 1)
lines(fieldE$doy, fieldE$tn, col = high_col, lwd = 2, lty = 3); box()

# ===== NITRATE & AMMONIUM ===========
plot(fieldB$doy, fieldB$nox, type = "l", 
     ylim = c(0,0.4), 
     pch = 20, col = low_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldB$doy, fieldB$nox, col = low_col, lwd = 2, lty = 1)
lines(fieldF$doy, fieldF$nox, col = low_col, lwd = 2, lty = 3); box()
mtext(side = 2, line = 2.7, "Dissolved N (mg/L)", font = 2)

plot(fieldA$doy, fieldA$nox, type = "l", 
     ylim = c(0,0.4), yaxt = 'n',
     pch = 20, col = int_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldA$doy, fieldA$nox, col = int_col, lwd = 2, lty = 1)
lines(fieldD$doy, fieldD$nox, col = int_col, lwd = 2, lty = 3); box()
mtext(side = 1, line = 2.7, "Day of Year, 2020", font = 2)

plot(fieldC$doy, fieldC$nox, type = "l", 
     ylim = c(0,0.4), yaxt = 'n',
     pch = 20, col = high_col, lwd = 2, lty = 1)
polygon(c(176,211,211,176), c(-10,-10,200,200), col = "gray90", border= NA)
polygon(c(211,239,239,211), c(-10,-10,200,200), col = "gray75", border= NA)
lines(fieldC$doy, fieldC$nox, col = high_col, lwd = 2, lty = 1)
lines(fieldE$doy, fieldE$nox, col = high_col, lwd = 2, lty = 3); box()
