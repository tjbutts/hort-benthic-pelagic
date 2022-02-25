## NEP GAM Time Series ## 

## GAM Code originally written by Grace Wilkinson 
## Adapted for chlorophyll data by Tyler Butts 

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

hort_metabolism

## Separate by grouping - total biomass, cladocerans, copepods, rotifers ## 
nep = hort_metabolism %>% 
  select(pond_id, doy, NEP)
nep

## nep by pond## ===========================

# Because we will be plotting by pond, make some separate data frames to make life easier
nepA = nep %>% #pulse, int
  filter(pond_id == "A") 

nepB = nep %>% #pulse, low
  filter(pond_id == "B") 

nepC = nep %>% #pulse, high
  filter(pond_id == "C") 

nepD = nep %>% #ref, int
  filter(pond_id == "D") 

nepE = nep %>% #ref, high
  filter(pond_id == "E") 

nepF = nep %>% #ref, low
  filter(pond_id == "F") 

# GAM-ing the NEP for pattern - not analysis...
#=============================================================

# Int Coupling Treatment===========
#Total P GAM - Pond A
nep_A_gam <- gam(NEP~ s(doy, k = 90),data = nepA, method = 'REML')
summary(nep_A_gam) 
gam.check(nep_A_gam)
#Total P GAM - Pond D
nep_D_gam <- gam(NEP~ s(doy, k = 90),data = nepD, method = 'REML')
summary(nep_D_gam) # gam.check(nep_D_gam)

# High Coupling Treatment===========
#Total P GAM - Pond C
nep_C_gam <- gam(NEP~ s(doy, k = 90),data = nepC, method = 'REML')
summary(nep_C_gam) # gam.check(nep_C_gam)
#Total P GAM - Pond E
nep_E_gam <- gam(NEP~ s(doy, k = 90),data = nepE, method = 'REML')
summary(nep_E_gam) # gam.check(nep_E_gam)

# Low Coupling Treatment===========
#Total P GAM - Pond B
nep_B_gam <- gam(NEP~ s(doy, k = 90),data = nepB, method = 'REML')
summary(nep_B_gam) # gam.check(nep_B_gam)
#Total P GAM - Pond F
nep_F_gam <- gam(NEP~ s(doy, k = 90),data = nepF, method = 'REML')
summary(nep_F_gam) # gam.check(nep_F_gam)

# Log Scale #
# Intermediate Coupling Treatment===========
#Total P GAM - Pond A
nep_A_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = algA, method = 'REML')
summary(nep_A_gam2) # gam.check(nep_A_gam2)
#Total P GAM - Pond D
nep_D_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = algD, method = 'REML')
summary(nep_D_gam2) # gam.check(nep_D_gam2)

# High Coupling Treatment===========
#Total P GAM - Pond C
nep_C_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = algC, method = 'REML')
summary(nep_C_gam2) # gam.check(nep_C_gam2)
#Total P GAM - Pond E
nep_E_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = algE, method = 'REML')
summary(nep_E_gam2) # gam.check(nep_E_gam2)

# Low Coupling Treatment===========
#Total P GAM - Pond B
nep_B_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = algB, method = 'REML')
summary(nep_B_gam2) # gam.check(nep_B_gam2)
#Total P GAM - Pond F
nep_F_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = algF, method = 'REML')
summary(nep_F_gam2) # gam.check(nep_F_gam2)

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
windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the Total P GAM for POND B ===============
plot(nep_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(-13,13), xlim=c(140, 245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(nep_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n", xlim=c(140, 245), ylim=c(-13,13),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "NEP", cex = 1.25)
mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND A ===============
plot(nep_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(-13,13), xlim = c(140,245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(nep_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n", xlim = c(140, 245), ylim=c(-13,13), 
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND C ===============
plot(nep_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(-13,13), xlim=c(140, 245), 
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(nep_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n", ylim=c(-13,13), xlim=c(140,245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25) 

