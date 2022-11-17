## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022

#============================================#
# STEP 4: Ecosystem Metabolism 
#           & Response Detection Algorithm 
#============================================#

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

metab

# Assess distribution of missing DOYs between ponds

# data # 
doy = as.data.frame(c(145:240)) %>% rename(doy = 'c(145:240)')
dat_y = select(metab, pond_id, doy, GPP) 
# Can use GPP, R, or NEP as the flag removes all data from a DOY if GPP or R is erroneous

dat = as_tibble(merge(gpp_y, doy, by = 'doy', all=TRUE))
dat

windows(width=6, height = 6)
stripchart(doy ~ pond_id, data = dat)
mtext('Metabolism Data', side=3, line=1)

## Separate by Metabolism Parameters ## 
# NEP GAMs #============================
nep = metab %>% 
  select(pond_id, doy, NEP)
nep 

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


# GAM - Intermediate Pulse 
nep_A_gam <- gam(NEP~ s(doy, k = 75),data = nepA, method = 'REML')
summary(nep_A_gam) # Smoothing term significant 
gam.check(nep_A_gam)

# GAM - Intermediate Reference 
nep_D_gam <- gam(NEP~ s(doy, k = 75),data = nepD, method = 'REML')
summary(nep_D_gam) # Smoothing term significant 
gam.check(nep_D_gam)

# GAM - High Pulse 
nep_C_gam <- gam(NEP~ s(doy, k = 75),data = nepC, method = 'REML')
summary(nep_C_gam) # Smoothing term significant 
gam.check(nep_C_gam)

# GAM - High Reference 
nep_E_gam <- gam(NEP~ s(doy, k = 75),data = nepE, method = 'REML')
summary(nep_E_gam) # Smoothing term significant 
gam.check(nep_E_gam)

# GAM - Low Pulse 
nep_B_gam <- gam(NEP~ s(doy, k = 75),data = nepB, method = 'REML')
summary(nep_B_gam) # Smoothing term significant 
gam.check(nep_B_gam)

# GAM - Low Reference 
nep_F_gam <- gam(NEP~ s(doy, k = 75),data = nepF, method = 'REML')
summary(nep_F_gam) # Smoothing term significant 
gam.check(nep_F_gam)

# R GAMs #============================
r = metab %>% 
  select(pond_id, doy, R)
r

# Because we will be plotting by pond, make some separate data frames to make life easier
rA = r %>% #pulse, int
  filter(pond_id == "A") 

rB = r %>% #pulse, low
  filter(pond_id == "B") 

rC = r %>% #pulse, high
  filter(pond_id == "C") 

rD = r %>% #ref, int
  filter(pond_id == "D") 

rE = r %>% #ref, high
  filter(pond_id == "E") 

rF = r %>% #ref, low
  filter(pond_id == "F") 


# GAM - Intermediate Pulse 
r_A_gam <- gam(R~ s(doy, k = 75),data = rA, method = 'REML')
summary(r_A_gam) # Smoothing term significant 
gam.check(r_A_gam)

# GAM - Intermediate Reference 
r_D_gam <- gam(R~ s(doy, k = 75),data = rD, method = 'REML')
summary(r_D_gam) # Smoothing term significant 
gam.check(r_D_gam)

# GAM - High Pulse 
r_C_gam <- gam(R~ s(doy, k = 75),data = rC, method = 'REML')
summary(r_C_gam) # Smoothing term significant 
gam.check(r_C_gam)

# GAM - High Reference 
r_E_gam <- gam(R~ s(doy, k = 90),data = rE, method = 'REML')
summary(r_E_gam) # Smoothing term significant 
gam.check(r_E_gam) # k is significant, does not meet assumption 

# GAM - Low Pulse 
r_B_gam <- gam(R~ s(doy, k = 75),data = rB, method = 'REML')
summary(r_B_gam) # Smoothing term significant 
gam.check(r_B_gam)

# GAM - Low Reference 
r_F_gam <- gam(R~ s(doy, k = 75),data = rF, method = 'REML')
summary(r_F_gam) # Smoothing term significant 
gam.check(r_F_gam)

# GPP GAMs #============================
gpp = metab %>% 
  select(pond_id, doy, GPP)
gpp 

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


# GAM - Intermediate Pulse 
gpp_A_gam <- gam(GPP~ s(doy, k = 75),data = gppA, method = 'REML')
summary(gpp_A_gam) # Smoothing term significant 
gam.check(gpp_A_gam)

# GAM - Intermediate Reference 
gpp_D_gam <- gam(GPP~ s(doy, k = 75),data = gppD, method = 'REML')
summary(gpp_D_gam) # Smoothing term significant 
gam.check(gpp_D_gam)

# GAM - High Pulse 
gpp_C_gam <- gam(GPP~ s(doy, k = 75),data = gppC, method = 'REML')
summary(gpp_C_gam) # Smoothing term significant 
gam.check(gpp_C_gam)

# GAM - High Reference 
gpp_E_gam <- gam(GPP~ s(doy, k = 90),data = gppE, method = 'REML')
summary(gpp_E_gam) # Smoothing term significant 
gam.check(gpp_E_gam) # Doesn't meet assumptions

# GAM - Low Pulse 
gpp_B_gam <- gam(GPP~ s(doy, k = 75),data = gppB, method = 'REML')
summary(gpp_B_gam) # Smoothing term significant 
gam.check(gpp_B_gam)

# GAM - Low Reference 
gpp_F_gam <- gam(GPP~ s(doy, k = 75),data = gppF, method = 'REML')
summary(gpp_F_gam) # Smoothing term significant 
gam.check(gpp_F_gam)


## Plotting Metabolism ##===============
windows(height = 9, width = 6)
par(mfrow =c(3,3),omi = c(3,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Colors for data visualization
#Ref: lty = 1, pulse: lty = 1
low_col_B = rgb(74, 166, 81, max = 255, alpha = 180) #Pond B, Pond F
low_col_F = rgb(74, 166, 81, max = 255, alpha = 100) #Pond B, Pond F
low_col = rgb(74, 166, 81, max = 255, alpha = 255) #Pond B, Pond F
ref_col = rgb(155, 155, 155, max=255, alpha = 100) # Reference
black_col = rgb(0,0,0, max=255, alpha = 100) # Black

int_col_A = rgb(44, 127, 184, max = 255, alpha = 180) #Pond A, pond D
int_col_D = rgb(44, 127, 184, max = 255, alpha = 100) #Pond A, pond D
int_col = rgb(44, 127, 184, max = 255, alpha = 255) #Pond A, pond D

high_col_C = rgb(8, 29, 88, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(8, 29, 88, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(8, 29, 88, max = 255, alpha = 255) #Pond C, Pond E
plot(nep_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(-8,7), xlim=c(140, 245),
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(nep_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim=c(140, 245), ylim=c(-8,7),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "NEP", cex = 1.25)
mtext(side = 3, line = 1, 'Low Coupling', cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)

plot(nep_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(-8,7), xlim = c(140,245),
     cex = 0.75, pch = 16, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(nep_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim = c(140, 245), ylim=c(-8,7), 
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 1, 'Intermediate', cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)

plot(nep_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(-8,7), xlim=c(140, 245), 
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(nep_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(nep_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", ylim=c(-8,7), xlim=c(140,245),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 3, line = 1, 'High Coupling', cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)

## GPP ## 
plot(gpp_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(0,20), xlim=c(140, 245),
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(gpp_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim=c(140, 245), ylim=c(0,20),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "GPP", cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)

plot(gpp_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(0,20), xlim = c(140,245),
     cex = 0.75, pch = 16, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(gpp_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim = c(140, 245), ylim=c(0,20), 
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)

plot(gpp_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(0,20), xlim=c(140, 245), 
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(gpp_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(gpp_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", ylim=c(0,20), xlim=c(140,245),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)

## R ## 
plot(r_B_gam, select = 1, 
     seWithMean = TRUE, shift = coef(r_B_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = low_col_B, ylim=c(-20,0), xlim=c(140, 245),
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = low_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(r_F_gam, select = 1, 
     seWithMean = TRUE, shift = coef(r_F_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim=c(140, 245), ylim=c(-20,0),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3, "GPP", cex = 1.25)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)

plot(r_A_gam, select = 1, 
     seWithMean = TRUE, shift = coef(r_A_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = int_col_A, ylim=c(-20,0), xlim = c(140,245),
     cex = 0.75, pch = 16, lwd = 2, lty = 3, col = int_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(r_D_gam, select = 1, 
     seWithMean = TRUE, shift = coef(r_D_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", xlim = c(140, 245), ylim=c(-20,0), 
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext('Day of Year, 2020', side = 1, cex=1.25, line = 3)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)

plot(r_C_gam, select = 1, 
     seWithMean = TRUE, shift = coef(r_C_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = high_col_C,  ylim=c(-20,0), xlim=c(140, 245), 
     cex = 0.75, pch = 16, lwd = .5, lty = 1, col = high_col,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(r_E_gam, select = 1, 
     seWithMean = TRUE, shift = coef(r_E_gam)[1],
     se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = FALSE,
     shade.col = ref_col, yaxt = "n", ylim=c(-20,0), xlim=c(140,245),
     cex = .75, pch = 17, lwd = .5, lty = 1, col = ref_col,
     xlab = "", ylab = "", cex.axis= 1.2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)

