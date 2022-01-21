## Hort Farm Ecosystem Resilience Project ###
# Code originally written by GM Wilkinson January 2022

#Experimental Context =================================
# Exploratory time series of: 
# Nutrient concentrations
# Nutrient ratios
# DOC (when we get the data)

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

hort_field = read_csv('hort20_surface_dat.csv')

# Because we will be plotting by pond, make some separate data frames to make life easier
fieldA = hort_field %>% #pulse, int
  filter(pond_id == "A") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))

fieldB = hort_field %>% #pulse, low
  filter(pond_id == "B") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))

fieldC = hort_field %>% #pulse, high
  filter(pond_id == "C") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))

fieldD = hort_field %>% #ref, int
  filter(pond_id == "D") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))

fieldE = hort_field %>% #ref, high
  filter(pond_id == "E") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))

fieldF = hort_field %>% #ref, low
  filter(pond_id == "F") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))


# GAM-ing the nutrients for pattern - not analysis...
#=============================================================

# Low Coupling Treatment===========
#Total P GAM - Pond A
tp_A_gam <- gam(tp ~ s(doy, k = 40),data = fieldA, method = 'REML')
summary(tp_A_gam) # gam.check(tp_A_gam)
#Total P GAM - Pond D
tp_D_gam <- gam(tp ~ s(doy, k = 40),data = fieldD, method = 'REML')
summary(tp_D_gam) # gam.check(tp_D_gam)

# Intermediate Coupling Treatment===========
#Total P GAM - Pond C
tp_C_gam <- gam(tp ~ s(doy, k = 40),data = fieldC, method = 'REML')
summary(tp_C_gam) # gam.check(tp_C_gam)
#Total P GAM - Pond E
tp_E_gam <- gam(tp ~ s(doy, k = 40),data = fieldE, method = 'REML')
summary(tp_E_gam) # gam.check(tp_E_gam)

# High Coupling Treatment===========
#Total P GAM - Pond B
tp_B_gam <- gam(tp ~ s(doy, k = 40),data = fieldB, method = 'REML')
summary(tp_B_gam) # gam.check(tp_B_gam)
#Total P GAM - Pond F
tp_F_gam <- gam(tp ~ s(doy, k = 40),data = fieldF, method = 'REML')
summary(tp_F_gam) # gam.check(tp_F_gam)


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


#============================
# Set up the Plot
windows(height = 7, width = 6.5)
par(mfrow = c(3,3), omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the Total P GAM for POND B ===============
plot(tp_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tp_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B,  ylim = c(0,60),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,100), lty = 3)
lines(c(211,211), c(-10,100), lty = 3)
lines(c(223,223), c(-10,100), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(tp_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tp_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n",
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Total P (ug/L)", cex = 1.25)
mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND A ===============
plot(tp_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tp_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim = c(0,150),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,200), lty = 3)
lines(c(211,211), c(-10,200), lty = 3)
lines(c(223,223), c(-10,200), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(tp_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tp_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n",
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND C ===============
plot(tp_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tp_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim = c(0,60),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,100), lty = 3)
lines(c(211,211), c(-10,100), lty = 3)
lines(c(223,223), c(-10,100), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(tp_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tp_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n",
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25)


# GAM-ing the nutrients for pattern - not analysis...
#=============================================================

# Low Coupling Treatment===========
#Total N GAM - Pond A
tn_A_gam <- gam(tn ~ s(doy, k = 40),data = fieldA, method = 'REML')
summary(tn_A_gam) # gam.check(tn_A_gam)
#Total N GAM - Pond D
tn_D_gam <- gam(tn ~ s(doy, k = 40),data = fieldD, method = 'REML')
summary(tn_D_gam) # gam.check(tn_D_gam)

# Intermediate Coupling Treatment===========
#Total N GAM - Pond C
tn_C_gam <- gam(tn ~ s(doy, k = 40),data = fieldC, method = 'REML')
summary(tn_C_gam) # gam.check(tn_C_gam)
#Total N GAM - Pond E
tn_E_gam <- gam(tn ~ s(doy, k = 40),data = fieldE, method = 'REML')
summary(tn_E_gam) # gam.check(tn_E_gam)

# High Coupling Treatment===========
#Total N GAM - Pond B
tn_B_gam <- gam(tn ~ s(doy, k = 40),data = fieldB, method = 'REML')
summary(tn_B_gam) # gam.check(tn_B_gam)
#Total N GAM - Pond F
tn_F_gam <- gam(tn ~ s(doy, k = 40),data = fieldF, method = 'REML')
summary(tn_F_gam) # gam.check(tn_F_gam)

#Plot of the Total N GAM for POND B ===============
plot(tn_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tn_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim = c(0,1),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,100), lty = 3)
lines(c(211,211), c(-10,100), lty = 3)
lines(c(223,223), c(-10,100), lty = 2, lwd = 2)

#Plot of the Total N GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(tn_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tn_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n",
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Total N (mg/L)", cex = 1.25)

#==================================================
#Plot of the Total N GAM for POND A ===============
plot(tn_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tn_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim = c(0,1),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,200), lty = 3)
lines(c(211,211), c(-10,200), lty = 3)
lines(c(223,223), c(-10,200), lty = 2, lwd = 2)

#Plot of the Total N GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(tn_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tn_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n",
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)

#==================================================
#Plot of the Total N GAM for POND C ===============
plot(tn_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tn_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim = c(0,1),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,100), lty = 3)
lines(c(211,211), c(-10,100), lty = 3)
lines(c(223,223), c(-10,100), lty = 2, lwd = 2)

#Plot of the Total N GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(tn_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(tn_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n",
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)


# GAM-ing the nutrients for pattern - not analysis...
#=============================================================

# Low Coupling Treatment===========
#NP Ratio GAM - Pond A
np_ratio_A_gam <- gam(np_ratio ~ s(doy, k = 40),data = fieldA, method = 'REML')
summary(np_ratio_A_gam) # gam.check(np_ratio_A_gam)
#NP Ratio GAM - Pond D
np_ratio_D_gam <- gam(np_ratio ~ s(doy, k = 40),data = fieldD, method = 'REML')
summary(np_ratio_D_gam) # gam.check(np_ratio_D_gam)

# Intermediate Coupling Treatment===========
#NP Ratio GAM - Pond C
np_ratio_C_gam <- gam(np_ratio ~ s(doy, k = 40),data = fieldC, method = 'REML')
summary(np_ratio_C_gam) # gam.check(np_ratio_C_gam)
#NP Ratio GAM - Pond E
np_ratio_E_gam <- gam(np_ratio ~ s(doy, k = 40),data = fieldE, method = 'REML')
summary(np_ratio_E_gam) # gam.check(np_ratio_E_gam)

# High Coupling Treatment===========
#NP Ratio GAM - Pond B
np_ratio_B_gam <- gam(np_ratio ~ s(doy, k = 40),data = fieldB, method = 'REML')
summary(np_ratio_B_gam) # gam.check(np_ratio_B_gam)
#NP Ratio GAM - Pond F
np_ratio_F_gam <- gam(np_ratio ~ s(doy, k = 40),data = fieldF, method = 'REML')
summary(np_ratio_F_gam) # gam.check(np_ratio_F_gam)

#Plot of the NP Ratio GAM for POND B ===============
plot(np_ratio_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(np_ratio_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim = c(0,10),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,100), lty = 3)
lines(c(211,211), c(-10,100), lty = 3)
lines(c(223,223), c(-10,100), lty = 2, lwd = 2)

#Plot of the NP Ratio GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(np_ratio_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(np_ratio_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n",
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "N:P Ratio", cex = 1.25)

#==================================================
#Plot of the NP Ratio GAM for POND A ===============
plot(np_ratio_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(np_ratio_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A,  ylim = c(0,10),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,200), lty = 3)
lines(c(211,211), c(-10,200), lty = 3)
lines(c(223,223), c(-10,200), lty = 2, lwd = 2)

#Plot of the NP Ratio GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(np_ratio_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(np_ratio_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n",
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 1, line = 3.5, "Day of Year, 2020", cex = 1.25)


#==================================================
#Plot of the NP Ratio GAM for POND C ===============
plot(np_ratio_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(np_ratio_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C, ylim = c(0,10),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,100), lty = 3)
lines(c(211,211), c(-10,100), lty = 3)
lines(c(223,223), c(-10,100), lty = 2, lwd = 2)

#Plot of the NP Ratio GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(np_ratio_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(np_ratio_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n",
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)

