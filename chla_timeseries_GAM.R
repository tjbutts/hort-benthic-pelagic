## Chlorophyll GAM Time Series ## 

## GAM Code originally written by Grace Wilkinson 
## Adapted for chlorophyll data by Tyler Butts 

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

hort_field

## Separate by grouping - total biomass, cladocerans, copepods, rotifers ## 
chl = hort_field %>% 
  select(pond_id, doy, chla)
chl

## chl by pond## ===========================

# Because we will be plotting by pond, make some separate data frames to make life easier
algA = chl %>% #pulse, int
  filter(pond_id == "A") 

algB = chl %>% #pulse, low
  filter(pond_id == "B") 

algC = chl %>% #pulse, high
  filter(pond_id == "C") 

algD = chl %>% #ref, int
  filter(pond_id == "D") 

algE = chl %>% #ref, high
  filter(pond_id == "E") 

algF = chl %>% #ref, low
  filter(pond_id == "F") 

# GAM-ing the chlorophyll for pattern - not analysis...
#=============================================================

# Int Coupling Treatment===========
#Total P GAM - Pond A
alg_A_gam <- gam(chla~ s(doy, k = 50),data = algA, method = 'REML')
summary(alg_A_gam) # gam.check(alg_A_gam)
#Total P GAM - Pond D
alg_D_gam <- gam(chla~ s(doy, k = 50),data = algD, method = 'REML')
summary(alg_D_gam) # gam.check(alg_D_gam)

# High Coupling Treatment===========
#Total P GAM - Pond C
alg_C_gam <- gam(chla~ s(doy, k = 50),data = algC, method = 'REML')
summary(alg_C_gam) # gam.check(alg_C_gam)
#Total P GAM - Pond E
alg_E_gam <- gam(chla~ s(doy, k = 50),data = algE, method = 'REML')
summary(alg_E_gam) # gam.check(alg_E_gam)

# Low Coupling Treatment===========
#Total P GAM - Pond B
alg_B_gam <- gam(chla~ s(doy, k = 50),data = algB, method = 'REML')
summary(alg_B_gam) # gam.check(alg_B_gam)
#Total P GAM - Pond F
alg_F_gam <- gam(chla~ s(doy, k = 50),data = algF, method = 'REML')
summary(alg_F_gam) # gam.check(alg_F_gam)

# Log Scale #
# Intermediate Coupling Treatment===========
#Total P GAM - Pond A
alg_A_gam2 <- gam(chla~ s(doy, k = 50),Gamma(link='log') ,data = algA, method = 'REML')
summary(alg_A_gam2) # gam.check(alg_A_gam2)
#Total P GAM - Pond D
alg_D_gam2 <- gam(chla~ s(doy, k = 50),Gamma(link='log') ,data = algD, method = 'REML')
summary(alg_D_gam2) # gam.check(alg_D_gam2)

# High Coupling Treatment===========
#Total P GAM - Pond C
alg_C_gam2 <- gam(chla~ s(doy, k = 50),Gamma(link='log') ,data = algC, method = 'REML')
summary(alg_C_gam2) # gam.check(alg_C_gam2)
#Total P GAM - Pond E
alg_E_gam2 <- gam(chla~ s(doy, k = 50),Gamma(link='log') ,data = algE, method = 'REML')
summary(alg_E_gam2) # gam.check(alg_E_gam2)

# Low Coupling Treatment===========
#Total P GAM - Pond B
alg_B_gam2 <- gam(chla~ s(doy, k = 50),Gamma(link='log') ,data = algB, method = 'REML')
summary(alg_B_gam2) # gam.check(alg_B_gam2)
#Total P GAM - Pond F
alg_F_gam2 <- gam(chla~ s(doy, k = 50),Gamma(link='log') ,data = algF, method = 'REML')
summary(alg_F_gam2) # gam.check(alg_F_gam2)

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
col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')

#Plot of the Total P GAM for POND B ===============
plot(alg_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(0, 35), xlim=c(140, 245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n", xlim=c(140, 245), ylim=c(0, 35),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Chl-a Biomass (ug/L)", cex = 1.25)
mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)
rect(185,-2,190,50, col=col, border=NA)

#==================================================
#Plot of the Total P GAM for POND A ===============
plot(alg_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(0, 35), xlim = c(140,245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n", xlim = c(140, 245), ylim=c(0, 35), 
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)
rect(185,-2,190,50, col=col, border=NA)

#==================================================
#Plot of the Total P GAM for POND C ===============
plot(alg_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(0, 35), xlim=c(140, 245), 
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n", ylim=c(0, 35), xlim=c(140,245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25) 
rect(185,-2,190,50, col=col, border=NA)

# Using a Log Scale # 
# Set up the Plot
windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the Total P gam2 for POND B ===============
plot(alg_B_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_B_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(log(1),log(35)), xlim=c(140, 245), yaxt='n',
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
axis(side=2,
     at=c(log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=0.8)


#Plot of the Total P gam2 for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_F_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_F_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n", ylim=c(log(1),log(35)), xlim=c(140, 245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F, yaxt='n', 
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Total Zooplankton (ug/L)", cex = 1.25)
mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)


#Plot of the Total P gam2 for POND A ===============
plot(alg_A_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_A_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(log(1),log(35)), xlim=c(140, 245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
axis(side=2,
     at=c(log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=0.8)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P gam2 for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_D_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_D_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n", ylim=c(log(1),log(35)), xlim=c(140, 245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#Plot of the Total P gam2 for POND C ===============
plot(alg_C_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_C_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(log(1),log(35)), xlim=c(140, 245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
axis(side=2,
     at=c(log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=0.8)


#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P gam2 for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_E_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_E_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n",ylim=c(log(1),log(35)), xlim=c(140, 245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25)

## Cyanofluor Data ##===========================
hort_field 

chl = hort_field %>%
  select(pond_id, doy, cyanofluor_chl)

# Because we will be plotting by pond, make some separate data frames to make life easier
algA = chl %>% #pulse, int
  filter(pond_id == "A") 

algB = chl %>% #pulse, low
  filter(pond_id == "B") 

algC = chl %>% #pulse, high
  filter(pond_id == "C") 

algD = chl %>% #ref, int
  filter(pond_id == "D") 

algE = chl %>% #ref, high
  filter(pond_id == "E") 

algF = chl %>% #ref, low
  filter(pond_id == "F") 

# GAM-ing the chlorophyll for pattern - not analysis...
#=============================================================

# Intermediate Coupling Treatment===========
#Total P GAM - Pond A
alg_A_gam <- gam(cyanofluor_chl~ s(doy, k = 50),data = algA, method = 'REML')
summary(alg_A_gam) 
gam.check(alg_A_gam)
#Total P GAM - Pond D
alg_D_gam <- gam(cyanofluor_chl~ s(doy, k = 50),data = algD, method = 'REML')
summary(alg_D_gam) 
gam.check(alg_D_gam)

# High Coupling Treatment===========
#Total P GAM - Pond C
alg_C_gam <- gam(cyanofluor_chl~ s(doy, k = 50),data = algC, method = 'REML')
summary(alg_C_gam) 
gam.check(alg_C_gam)
#Total P GAM - Pond E
alg_E_gam <- gam(cyanofluor_chl~ s(doy, k = 50),data = algE, method = 'REML')
summary(alg_E_gam) 
gam.check(alg_E_gam)

# Low Coupling Treatment===========
#Total P GAM - Pond B
alg_B_gam <- gam(cyanofluor_chl~ s(doy, k = 50),data = algB, method = 'REML')
summary(alg_B_gam) 
gam.check(alg_B_gam)
#Total P GAM - Pond F
alg_F_gam <- gam(cyanofluor_chl~ s(doy, k = 50),data = algF, method = 'REML')
summary(alg_F_gam) 
gam.check(alg_F_gam)

# Log Scale #
# Intermediate Coupling Treatment===========
#Total P GAM - Pond A
alg_A_gam2 <- gam(cyanofluor_chl~ s(doy, k = 50),Gamma(link='log') ,data = algA, method = 'REML')
summary(alg_A_gam2) # gam.check(alg_A_gam2)
#Total P GAM - Pond D
alg_D_gam2 <- gam(cyanofluor_chl~ s(doy, k = 50),Gamma(link='log') ,data = algD, method = 'REML')
summary(alg_D_gam2) # gam.check(alg_D_gam2)

# High Coupling Treatment===========
#Total P GAM - Pond C
alg_C_gam2 <- gam(cyanofluor_chl~ s(doy, k = 50),Gamma(link='log') ,data = algC, method = 'REML')
summary(alg_C_gam2) # gam.check(alg_C_gam2)
#Total P GAM - Pond E
alg_E_gam2 <- gam(cyanofluor_chl~ s(doy, k = 50),Gamma(link='log') ,data = algE, method = 'REML')
summary(alg_E_gam2) # gam.check(alg_E_gam2)

# Low Coupling Treatment===========
#Total P GAM - Pond B
alg_B_gam2 <- gam(cyanofluor_chl~ s(doy, k = 50),Gamma(link='log') ,data = algB, method = 'REML')
summary(alg_B_gam2) # gam.check(alg_B_gam2)
#Total P GAM - Pond F
alg_F_gam2 <- gam(cyanofluor_chl~ s(doy, k = 50),Gamma(link='log') ,data = algF, method = 'REML')
summary(alg_F_gam2) # gam.check(alg_F_gam2)

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
plot(alg_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(200,10000), xlim=c(140, 245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n", xlim=c(140, 245), ylim=c(200,10000),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Chlorophyll (RFUs)", cex = 1.25)
mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND A ===============
plot(alg_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(200,10000), xlim = c(140,245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n", xlim = c(140, 245), ylim=c(200,10000), 
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND C ===============
plot(alg_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(200,10000), xlim=c(140, 245), 
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(alg_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n", ylim=c(200,10000), xlim=c(140,245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25) 

#============================
# Set up the Plot
windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the Total P GAM for POND B ===============
plot(alg_B_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_B_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(log(200),log(10000)), xlim=c(140, 245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_F_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_F_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n", xlim=c(140, 245), ylim=c(log(200),log(10000)),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Chlorophyll (RFUs)", cex = 1.25)
mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND A ===============
plot(alg_A_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_A_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(log(200),log(10000)), xlim = c(140,245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_D_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_D_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n", xlim = c(140, 245), ylim=c(log(200),log(10000)), 
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND C ===============
plot(alg_C_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_C_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(log(200),log(10000)), xlim=c(140, 245), 
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,20000), lty = 3)
lines(c(211,211), c(-10,20000), lty = 3)
lines(c(223,223), c(-10,20000), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(alg_E_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(alg_E_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n", ylim=c(log(200),log(10000)), xlim=c(140,245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25) 

