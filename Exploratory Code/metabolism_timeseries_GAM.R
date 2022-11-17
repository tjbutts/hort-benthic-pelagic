## Metabolism GAM Time Series ## 

## GAM Code originally written by Grace Wilkinson 
## Adapted for chlorophyll data by Tyler Butts 

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

hort_metabolism_clean = read_csv('daily-metabolism_data (2).csv') %>%
  filter(!(flag==1))
hort_metabolism_clean

## Separate by grouping - total biomass, cladocerans, copepods, rotifers ## 
nep = hort_metabolism_clean %>% 
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
nep_A_gam <- gam(NEP~ s(doy, k = 75),data = nepA, method = 'REML')
summary(nep_A_gam) 
gam.check(nep_A_gam)
#Total P GAM - Pond D
nep_D_gam <- gam(NEP~ s(doy, k = 75),data = nepD, method = 'REML')
summary(nep_D_gam) # gam.check(nep_D_gam)

# High Coupling Treatment===========
#Total P GAM - Pond C
nep_C_gam <- gam(NEP~ s(doy, k = 75),data = nepC, method = 'REML')
summary(nep_C_gam) # gam.check(nep_C_gam)
#Total P GAM - Pond E
nep_E_gam <- gam(NEP~ s(doy, k = 75),data = nepE, method = 'REML')
summary(nep_E_gam) # gam.check(nep_E_gam)

# Low Coupling Treatment===========
#Total P GAM - Pond B
nep_B_gam <- gam(NEP~ s(doy, k = 75),data = nepB, method = 'REML')
summary(nep_B_gam) # gam.check(nep_B_gam)
#Total P GAM - Pond F
nep_F_gam <- gam(NEP~ s(doy, k = 75),data = nepF, method = 'REML')
summary(nep_F_gam) # gam.check(nep_F_gam)

# Log Scale #
# Intermediate Coupling Treatment===========
#Total P GAM - Pond A
nep_A_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = nepA, method = 'REML')
summary(nep_A_gam2) # gam.check(nep_A_gam2)
#Total P GAM - Pond D
nep_D_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = nepD, method = 'REML')
summary(nep_D_gam2) # gam.check(nep_D_gam2)

# High Coupling Treatment===========
#Total P GAM - Pond C
nep_C_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = nepC, method = 'REML')
summary(nep_C_gam2) # gam.check(nep_C_gam2)
#Total P GAM - Pond E
nep_E_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = nepE, method = 'REML')
summary(nep_E_gam2) # gam.check(nep_E_gam2)

# Low Coupling Treatment===========
#Total P GAM - Pond B
nep_B_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = nepB, method = 'REML')
summary(nep_B_gam2) # gam.check(nep_B_gam2)
#Total P GAM - Pond F
nep_F_gam2 <- gam(NEP~ s(doy, k = 50),Gamma(link='log') ,data = nepF, method = 'REML')
summary(nep_F_gam2) # gam.check(nep_F_gam2)

#Plotting Colors
#Colors for data visualization
#Ref: lty = 3, pulse: lty = 1
low_col_B = rgb(74, 166, 81, max = 255, alpha = 180) #Pond B, Pond F
low_col_F = rgb(74, 166, 81, max = 255, alpha = 100) #Pond B, Pond F
low_col = rgb(74, 166, 81, max = 255, alpha = 255) #Pond B, Pond F
ref_col = rgb(155, 155, 155, max=255, alpha = 100) # Reference

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
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
#lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(nep_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim=c(140, 245), ylim=c(-13,13),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "NEP", cex = 1.25)
#mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND A ===============
plot(nep_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(-13,13), xlim = c(140,245),
     cex = 0.75, pch = 16, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
#lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(nep_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim = c(140, 245), ylim=c(-13,13), 
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND C ===============
plot(nep_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(-13,13), xlim=c(140, 245), 
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
#lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(nep_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", ylim=c(-13,13), xlim=c(140,245),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25) 


# Respiration #=======================
hort_metabolism_clean

## Separate by grouping - total biomass, cladocerans, copepods, rotifers ## 
resp = hort_metabolism_clean %>% 
  select(pond_id, doy, R)
resp
max(resp$R) #-0.041
min(resp$R) #-22


# Because we will be plotting by pond, make some separate data frames to make life easier
respA = resp %>% #pulse, int
  filter(pond_id == "A") 

respB = resp %>% #pulse, low
  filter(pond_id == "B") 

respC = resp %>% #pulse, high
  filter(pond_id == "C") 

respD = resp %>% #ref, int
  filter(pond_id == "D") 

respE = resp %>% #ref, high
  filter(pond_id == "E") 

respF = resp %>% #ref, low
  filter(pond_id == "F") 

# GAM-ing the resp for pattern - not analysis...
#=============================================================

# Int Coupling Treatment===========
#Total P GAM - Pond A
resp_A_gam <- gam(R~ s(doy, k = 75),data = respA, method = 'REML')
summary(resp_A_gam) 
gam.check(resp_A_gam)
#Total P GAM - Pond D
resp_D_gam <- gam(R~ s(doy, k = 75),data = respD, method = 'REML')
summary(resp_D_gam) # gam.check(resp_D_gam)

# High Coupling Treatment===========
#Total P GAM - Pond C
resp_C_gam <- gam(R~ s(doy, k = 75),data = respC, method = 'REML')
summary(resp_C_gam) # gam.check(resp_C_gam)
#Total P GAM - Pond E
resp_E_gam <- gam(R~ s(doy, k = 75),data = respE, method = 'REML')
summary(resp_E_gam) # gam.check(resp_E_gam)

# Low Coupling Treatment===========
#Total P GAM - Pond B
resp_B_gam <- gam(R~ s(doy, k = 75),data = respB, method = 'REML')
summary(resp_B_gam) # gam.check(resp_B_gam)
#Total P GAM - Pond F
resp_F_gam <- gam(R~ s(doy, k = 75),data = respF, method = 'REML')
summary(resp_F_gam) # gam.check(resp_F_gam)

# Log Scale # 

# Intermediate Coupling Treatment===========
#Total P GAM - Pond A
resp_A_gam2 <- gam(R~ s(doy, k = 90),Gamma(link='log') ,data = respA, method = 'REML')
summary(resp_A_gam2) # gam.check(resp_A_gam2)
#Total P GAM - Pond D
resp_D_gam2 <- gam(R~ s(doy, k = 90),Gamma(link='log') ,data = respD, method = 'REML')
summary(resp_D_gam2) # gam.check(resp_D_gam2)

# High Coupling Treatment===========
#Total P GAM - Pond C
resp_C_gam2 <- gam(R~ s(doy, k = 90),Gamma(link='log') ,data = respC, method = 'REML')
summary(resp_C_gam2) # gam.check(resp_C_gam2)
#Total P GAM - Pond E
resp_E_gam2 <- gam(R~ s(doy, k = 90),Gamma(link='log') ,data = respE, method = 'REML')
summary(resp_E_gam2) # gam.check(resp_E_gam2)

# Low Coupling Treatment===========
#Total P GAM - Pond B
resp_B_gam2 <- gam(R~ s(doy, k = 90),Gamma(link='log') ,data = respB, method = 'REML')
summary(resp_B_gam2) # gam.check(resp_B_gam2)
#Total P GAM - Pond F
resp_F_gam2 <- gam(R~ s(doy, k = 90),Gamma(link='log') ,data = respF, method = 'REML')
summary(resp_F_gam2) # gam.check(resp_F_gam2)

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

ref_col

#============================
# Set up the Plot
windows(height = 3, width = 6.5)
par(mfrow =c(1,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the R GAM for POND B ===============
plot(resp_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(-24,0), xlim=c(140, 245),
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
#lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(resp_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim=c(140, 245), ylim=c(-24,0),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "R", cex = 1.25)
#mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND A ===============
plot(resp_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(-24,0), xlim = c(140,245),
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
#lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(resp_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim = c(140, 245), ylim=c(-24,0), 
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND C ===============
plot(resp_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(-24,0), xlim=c(140, 245), 
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
#lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(resp_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", ylim=c(-24,0), xlim=c(140,245),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25) 

# Gross Primary Production #=======================
hort_metabolism_clean

## Separate by grouping - total biomass, cladocerans, copepods, rotifers ## 
gpp = hort_metabolism_clean %>% 
  select(pond_id, doy, GPP)
gpp
max(gpp$GPP) #22
min(gpp$GPP) #0

## nep by pond## ===========================

# Because we will be plotting by pond, make some separate data frames to make life easier
gppA = gpp %>% #pulse, int
  filter(pond_id == "A") 

gppB = gpp %>% #pulse, low
  filter(pond_id == "B") 

gppC = gpp %>% #pulse, high
  filter(pond_id == "C") 

gppD = gpp %>% #ref, int
  filter(pond_id == "D") 

gppE = gpp %>% #ref, high
  filter(pond_id == "E") 

gppF = gpp %>% #ref, low
  filter(pond_id == "F") 

# GAM-ing the gpp for pattern - not analysis...
#=============================================================

# Int Coupling Treatment===========
#Total P GAM - Pond A
gpp_A_gam <- gam(GPP~ s(doy, k = 75),data = gppA, method = 'REML')
summary(gpp_A_gam) 
gam.check(gpp_A_gam)
#Total P GAM - Pond D
gpp_D_gam <- gam(GPP~ s(doy, k = 75),data = gppD, method = 'REML')
summary(gpp_D_gam) # gam.check(gpp_D_gam)

# High Coupling Treatment===========
#Total P GAM - Pond C
gpp_C_gam <- gam(GPP~ s(doy, k = 75),data = gppC, method = 'REML')
summary(gpp_C_gam) # gam.check(gpp_C_gam)
#Total P GAM - Pond E
gpp_E_gam <- gam(GPP~ s(doy, k = 75),data = gppE, method = 'REML')
summary(gpp_E_gam) # gam.check(gpp_E_gam)

# Low Coupling Treatment===========
#Total P GAM - Pond B
gpp_B_gam <- gam(GPP~ s(doy, k = 75),data = gppB, method = 'REML')
summary(gpp_B_gam) # gam.check(gpp_B_gam)
#Total P GAM - Pond F
gpp_F_gam <- gam(GPP~ s(doy, k = 75),data = gppF, method = 'REML')
summary(gpp_F_gam) # gam.check(gpp_F_gam)

# Log Scale # 

# Intermediate Coupling Treatment===========
#Total P GAM - Pond A
gpp_A_gam2 <- gam(GPP~ s(doy, k = 90),Gamma(link='log') ,data = gppA, method = 'REML')
summary(gpp_A_gam2) # gam.check(gpp_A_gam2)
#Total P GAM - Pond D
gpp_D_gam2 <- gam(GPP~ s(doy, k = 90),Gamma(link='log') ,data = gppD, method = 'REML')
summary(gpp_D_gam2) # gam.check(gpp_D_gam2)

# High Coupling Treatment===========
#Total P GAM - Pond C
gpp_C_gam2 <- gam(GPP~ s(doy, k = 90),Gamma(link='log') ,data = gppC, method = 'REML')
summary(gpp_C_gam2) # gam.check(gpp_C_gam2)
#Total P GAM - Pond E
gpp_E_gam2 <- gam(GPP~ s(doy, k = 90),Gamma(link='log') ,data = gppE, method = 'REML')
summary(gpp_E_gam2) # gam.check(gpp_E_gam2)

# Low Coupling Treatment===========
#Total P GAM - Pond B
gpp_B_gam2 <- gam(GPP~ s(doy, k = 90),Gamma(link='log') ,data = gppB, method = 'REML')
summary(gpp_B_gam2) # gam.check(gpp_B_gam2)
#Total P GAM - Pond F
gpp_F_gam2 <- gam(GPP~ s(doy, k = 90),Gamma(link='log') ,data = gppF, method = 'REML')
summary(gpp_F_gam2) # gam.check(gpp_F_gam2)

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
plot(gpp_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(0,22), xlim=c(140, 245),
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
#lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(gpp_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim=c(140, 245), ylim=c(0,22),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "GPP", cex = 1.25)
#mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND A ===============
plot(gpp_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(0,22), xlim = c(140,245),
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
#lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(gpp_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim = c(140, 245), ylim=c(0,22), 
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 3, line = 0.5, "Int. Coupling", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND C ===============
plot(gpp_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(0,22), xlim=c(140, 245), 
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
#lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(gpp_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", ylim=c(0,22), xlim=c(140,245),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 3, line = 0.5, "High Coupling", cex = 1.25) 

# Set up a 3x3 array #=====================
windows(height = 6, width = 6.5)
par(mfrow =c(3,3),omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

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

#Plot of the Total P GAM for POND B ===============
plot(resp_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(-24,0), xlim=c(140, 245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(resp_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n", xlim=c(140, 245), ylim=c(-24,0),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "R", cex = 1.25)


#==================================================
#Plot of the Total P GAM for POND A ===============
plot(resp_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(-24,0), xlim = c(140,245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(resp_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n", xlim = c(140, 245), ylim=c(-24,0), 
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)


#==================================================
#Plot of the Total P GAM for POND C ===============
plot(resp_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(-24,0), xlim=c(140, 245), 
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(resp_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(resp_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n", ylim=c(-24,0), xlim=c(140,245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)

#Plot of the Total P GAM for POND B ===============
plot(gpp_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(0,22), xlim=c(140, 245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot
plot(gpp_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_F, yaxt = "n", xlim=c(140, 245), ylim=c(0,22),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = low_col_F,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "GPP", cex = 1.25)

#==================================================
#Plot of the Total P GAM for POND A ===============
plot(gpp_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(0,22), xlim = c(140,245),
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND D ===============
par(new = TRUE) #add new smooth to the same plot
plot(gpp_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_D, yaxt = "n", xlim = c(140, 245), ylim=c(0,22), 
     cex = 1, pch = 17, lwd = 2, lty = 1, col = int_col_D,
     xlab = "", ylab = "", cex.axis= 1.2)


#==================================================
#Plot of the Total P GAM for POND C ===============
plot(gpp_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(0,22), xlim=c(140, 245), 
     cex = 0.75, pch = 19, lwd = 2, lty = 3, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-100,700), lty = 2, lwd = 2)

#Plot of the Total P GAM for POND E ===============
par(new = TRUE) #add new smooth to the same plot
plot(gpp_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_E, yaxt = "n", ylim=c(0,22), xlim=c(140,245),
     cex = 1, pch = 17, lwd = 2, lty = 1, col = high_col_E,
     xlab = "", ylab = "", cex.axis= 1.2)
 
