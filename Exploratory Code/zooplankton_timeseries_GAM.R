## Zooplankton GAM Time Series Exploration ## 
## GAM Code originally written by Grace Wilkinson 
## Adapted for Zooplankton data by Tyler Butts 

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

hort_zoop

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

## Separate by grouping - total biomass, cladocerans, copepods, rotifers ## 
tot = hort_zoop %>%
  group_by(pond_id, doy) %>%
  summarise(tot_biomass = sum(biomass)) %>%
  ungroup()

clad = hort_zoop %>%
  filter(group=='SmCladocera' | group == 'Bosmina' | group == 'LgCladocera' | group == 'Ceriodaphnia' | group == 'Chydorus' |
           group == 'Daphnia' | group == 'Simocephalus') %>% 
  group_by(pond_id, doy) %>%
  summarise(tot_biomass = sum(biomass)) %>%
  ungroup()

cope = hort_zoop %>%
  filter(group =='Calanoid' | group == 'Cyclopoid' | group == 'Nauplii') %>% 
  group_by(pond_id, doy) %>% 
  summarise(tot_biomass = sum(biomass)) %>%
  ungroup()

roti = hort_zoop %>% 
  filter(group == 'Rotifer') %>%
  group_by(pond_id, doy) %>%
  summarise(tot_biomass = sum(biomass)) %>%
  ungroup()

## Total Zooplankton ## ===========================
  
# Because we will be plotting by pond, make some separate data frames to make life easier
zoopA = tot %>% #pulse, int
  filter(pond_id == "A") 

zoopB = tot %>% #pulse, low
  filter(pond_id == "B") 

zoopC = tot %>% #pulse, high
  filter(pond_id == "C") 

zoopD = tot %>% #ref, int
  filter(pond_id == "D") 

zoopE = tot %>% #ref, high
  filter(pond_id == "E") 

zoopF = tot %>% #ref, low
  filter(pond_id == "F") 

# GAM-ing the zooplankton for pattern - not analysis...

# Low Coupling Treatment===========
#Total P GAM - Pond A
zp_A_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopA, method = 'REML')
summary(zp_A_gam) # gam.check(zp_A_gam)
#Total P GAM - Pond D
zp_D_gam <- gam(tot_biomass~ s(doy, k = 16),data = zoopD, method = 'REML')
summary(zp_D_gam) # gam.check(zp_D_gam)

# Intermediate Coupling Treatment===========
#Total P GAM - Pond C
zp_C_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopC, method = 'REML')
summary(zp_C_gam) # gam.check(zp_C_gam)
#Total P GAM - Pond E
zp_E_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopE, method = 'REML')
summary(zp_E_gam) # gam.check(zp_E_gam)

# High Coupling Treatment===========
#Total P GAM - Pond B
zp_B_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopB, method = 'REML')
summary(zp_B_gam) # gam.check(zp_B_gam)
#Total P GAM - Pond F
zp_F_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopF, method = 'REML')
summary(zp_F_gam) # gam.check(zp_F_gam)

# Log Scale # 
# Intermediate Coupling Treatment===========
#Total P GAM - Pond A
zp_A_gam2 <- gam(tot_biomass~ s(doy, k = 17),Gamma(link = 'log') ,data = zoopA, method = 'REML')
summary(zp_A_gam2) 
gam.check(zp_A_gam2)
#Total P GAM - Pond D
zp_D_gam2 <- gam(tot_biomass~ s(doy, k = 16), Gamma(link = 'log') ,data = zoopD, method = 'REML')
summary(zp_D_gam2) 
gam.check(zp_D_gam2)

# High Coupling Treatment===========
#Total P GAM - Pond C
zp_C_gam2 <- gam(tot_biomass~ s(doy, k = 17), Gamma(link = 'log') ,data = zoopC, method = 'REML')
summary(zp_C_gam2) 
gam.check(zp_C_gam2)
#Total P GAM - Pond E
zp_E_gam2 <- gam(tot_biomass~ s(doy, k = 17), Gamma(link = 'log') ,data = zoopE, method = 'REML')
summary(zp_E_gam2) 
gam.check(zp_E_gam2)

# Low Coupling Treatment===========
#Total P GAM - Pond B
zp_B_gam2 <- gam(tot_biomass~ s(doy, k = 17), Gamma(link = 'log') ,data = zoopB, method = 'REML')
summary(zp_B_gam2) 
gam.check(zp_B_gam2)
#Total P GAM - Pond F
zp_F_gam2 <- gam(tot_biomass~ s(doy, k = 17),Gamma(link = 'log') ,data = zoopF, method = 'REML')
summary(zp_F_gam2) 
gam.check(zp_F_gam2)


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

ref_col = rgb(155, 155, 155, max=255, alpha = 100) # Reference

# Set up the Plot #==============
windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the Total P GAM for POND B ===============
plot(zp_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(0,600), xlim=c(140, 245),
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
#lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim=c(140, 245), ylim=c(0,600),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Total Zooplankton (ug/L)", cex = 1.25)
#mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#Plot of the Total P GAM for POND A ===============
plot(zp_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim = c(0,600), xlim = c(140,245),
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
#lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim = c(140, 245), ylim = c(0,600), 
     cex = 0.75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#Plot of the Total P GAM for POND C ===============
plot(zp_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim = c(0,600), xlim=c(140, 245), 
     cex = 0.75, pch = 16, lwd = 2, lty = 1, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
#lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", ylim=c(0,600), xlim=c(140,245),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25)

# Using a Log Scale #=========================
# Set up the Plot
windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the Total P gam2 for POND F ===============
plot(zp_F_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(zp_F_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", ylim=c(log(1),log(1000)), xlim=c(140, 245),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col, yaxt='n', 
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Total Zooplankton (ug/L)", cex = 1.25)
#mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#Plot of the Total P gam2 for POND B ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_B_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(zp_B_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(log(1),log(1000)), xlim=c(140, 245), yaxt='n',
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
#lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
axis(side=2,
     at=c(log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=0.8)


#Plot of the Total P gam2 for POND D ===============
plot(zp_D_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(zp_D_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", ylim=c(log(1),log(1000)), xlim=c(140, 245),
     cex = .75, pch = 17, lwd = 1, lty = 1, col = ref_col, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
#lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P gam2 for POND A ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_A_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(zp_A_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(log(1),log(1000)), xlim=c(140, 245),
     cex = 0.75, pch = 16, lwd = 0.5, lty = 1, col = int_col, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
axis(side=2,
     at=c(log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=0.8)


#Plot of the Total P gam2 for POND C ===============
plot(zp_E_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(zp_E_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n",ylim=c(log(1),log(1000)), xlim=c(140, 245),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
#lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P gam2 for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_C_gam2, select = 1, 
     seWithMean = TRUE, shift = coef(zp_C_gam2)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(log(1),log(1000)), xlim=c(140, 245),
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = high_col, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
axis(side=2,
     at=c(log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=0.8)

## Clad Zooplankton ## ===========================

# Because we will be plotting by pond, make some separate data frames to make life easier
zoopA = clad %>% #pulse, int
  filter(pond_id == "A") 

zoopB = clad %>% #pulse, low
  filter(pond_id == "B") 

zoopC = clad %>% #pulse, high
  filter(pond_id == "C") 

zoopD = clad %>% #ref, int
  filter(pond_id == "D") 

zoopE = clad %>% #ref, high
  filter(pond_id == "E") 

zoopF = clad %>% #ref, low
  filter(pond_id == "F") 

# GAM-ing the zooplankton for pattern - not analysis...

# Low Coupling Treatment===========
#Total P GAM - Pond A
zp_A_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopA, method = 'REML')
summary(zp_A_gam) # gam.check(zp_A_gam)
#Total P GAM - Pond D
zp_D_gam <- gam(tot_biomass~ s(doy, k = 16),data = zoopD, method = 'REML')
summary(zp_D_gam) # gam.check(zp_D_gam)

# Intermediate Coupling Treatment===========
#Total P GAM - Pond C
zp_C_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopC, method = 'REML')
summary(zp_C_gam) # gam.check(zp_C_gam)
#Total P GAM - Pond E
zp_E_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopE, method = 'REML')
summary(zp_E_gam) # gam.check(zp_E_gam)

# High Coupling Treatment===========
#Total P GAM - Pond B
zp_B_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopB, method = 'REML')
summary(zp_B_gam) # gam.check(zp_B_gam)
#Total P GAM - Pond F
zp_F_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopF, method = 'REML')
summary(zp_F_gam) # gam.check(zp_F_gam)

# Set up the Plot #=================
windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the Total P GAM for POND B ===============
plot(zp_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(0,600), xlim=c(140, 245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n", xlim=c(140, 245), ylim=c(0,600),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Cladocerans (ug/L)", cex = 1.25)
mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#Plot of the Total P GAM for POND A ===============
plot(zp_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim = c(0,600), xlim = c(140,245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n", xlim = c(140, 245), ylim = c(0,600), 
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#Plot of the Total P GAM for POND C ===============
plot(zp_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim = c(0,600), xlim=c(140, 245), 
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n", ylim=c(0,600), xlim=c(140,245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25)

# Cope Biomass #========================

zoopA = cope %>% #pulse, int
  filter(pond_id == "A") 

zoopB = cope %>% #pulse, low
  filter(pond_id == "B") 

zoopC = cope %>% #pulse, high
  filter(pond_id == "C") 

zoopD = cope %>% #ref, int
  filter(pond_id == "D") 

zoopE = cope %>% #ref, high
  filter(pond_id == "E") 

zoopF = cope %>% #ref, low
  filter(pond_id == "F") 

# Low Coupling Treatment===========
#Total P GAM - Pond A
zp_A_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopA, method = 'REML')
summary(zp_A_gam) # gam.check(zp_A_gam)
#Total P GAM - Pond D
zp_D_gam <- gam(tot_biomass~ s(doy, k = 16),data = zoopD, method = 'REML')
summary(zp_D_gam) # gam.check(zp_D_gam)

# Intermediate Coupling Treatment===========
#Total P GAM - Pond C
zp_C_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopC, method = 'REML')
summary(zp_C_gam) # gam.check(zp_C_gam)
#Total P GAM - Pond E
zp_E_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopE, method = 'REML')
summary(zp_E_gam) # gam.check(zp_E_gam)

# High Coupling Treatment===========
#Total P GAM - Pond B
zp_B_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopB, method = 'REML')
summary(zp_B_gam) # gam.check(zp_B_gam)
#Total P GAM - Pond F
zp_F_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopF, method = 'REML')
summary(zp_F_gam) # gam.check(zp_F_gam)

# Set up the Plot #=================
windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the Total P GAM for POND B ===============
plot(zp_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(0,600), xlim=c(140, 245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n", xlim=c(140, 245), ylim=c(0,600),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Cladocerans (ug/L)", cex = 1.25)
mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#Plot of the Total P GAM for POND A ===============
plot(zp_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim = c(0,600), xlim = c(140,245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n", xlim = c(140, 245), ylim = c(0,600), 
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#Plot of the Total P GAM for POND C ===============
plot(zp_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim = c(0,600), xlim=c(140, 245), 
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n", ylim=c(0,600), xlim=c(140,245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25)

# Cope Biomass #========================

zoopA = roti %>% #pulse, int
  filter(pond_id == "A") 

zoopB = roti %>% #pulse, low
  filter(pond_id == "B") 

zoopC = roti %>% #pulse, high
  filter(pond_id == "C") 

zoopD = roti %>% #ref, int
  filter(pond_id == "D") 

zoopE = roti %>% #ref, high
  filter(pond_id == "E") 

zoopF = roti %>% #ref, low
  filter(pond_id == "F") 

# Low Coupling Treatment===========
#Total P GAM - Pond A
zp_A_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopA, method = 'REML')
summary(zp_A_gam) # gam.check(zp_A_gam)
#Total P GAM - Pond D
zp_D_gam <- gam(tot_biomass~ s(doy, k = 16),data = zoopD, method = 'REML')
summary(zp_D_gam) # gam.check(zp_D_gam)

# Intermediate Coupling Treatment===========
#Total P GAM - Pond C
zp_C_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopC, method = 'REML')
summary(zp_C_gam) # gam.check(zp_C_gam)
#Total P GAM - Pond E
zp_E_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopE, method = 'REML')
summary(zp_E_gam) # gam.check(zp_E_gam)

# High Coupling Treatment===========
#Total P GAM - Pond B
zp_B_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopB, method = 'REML')
summary(zp_B_gam) # gam.check(zp_B_gam)
#Total P GAM - Pond F
zp_F_gam <- gam(tot_biomass~ s(doy, k = 17),data = zoopF, method = 'REML')
summary(zp_F_gam) # gam.check(zp_F_gam)

# Set up the Plot #=================
windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the Total P GAM for POND B ===============
plot(zp_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(0,150), xlim=c(140, 245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n", xlim=c(140, 245), ylim=c(0,150),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "Cladocerans (ug/L)", cex = 1.25)
mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#Plot of the Total P GAM for POND A ===============
plot(zp_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim = c(0,150), xlim = c(140,245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n", xlim = c(140, 245), ylim = c(0,150), 
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#Plot of the Total P GAM for POND C ===============
plot(zp_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim = c(0,150), xlim=c(140, 245), 
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(zp_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(zp_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n", ylim=c(0,150), xlim=c(140,245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25)

