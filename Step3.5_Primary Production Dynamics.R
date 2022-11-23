## Step3_Primary Production Dynamics ##

## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022

#============================================#
# STEP 3: Chlorophyll-a time series 
#           & Response Detection Algorithm
#============================================#

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)


# ========= PLOTTING COLORS ===== # 
low_col_B = rgb(74, 166, 81, max = 255, alpha = 180) #Pond B, Pond F
low_col_F = rgb(74, 166, 81, max = 255, alpha = 100) #Pond B, Pond F
low_col = rgb(74, 166, 81, max = 255, alpha = 255) #Pond B, Pond F
ref_col = rgb(155, 155, 155, max=255, alpha = 100) # Reference
black_col = rgb(0,0,0, max=255, alpha = 100) # Black
transparent = rgb(255,255,255, max=255, alpha = 0)

int_col_A = rgb(44, 127, 184, max = 255, alpha = 180) #Pond A, pond D
int_col_D = rgb(44, 127, 184, max = 255, alpha = 100) #Pond A, pond D
int_col = rgb(44, 127, 184, max = 255, alpha = 255) #Pond A, pond D

high_col_C = rgb(8, 29, 88, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(8, 29, 88, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(8, 29, 88, max = 255, alpha = 255) #Pond C, Pond E

# Data # 
hort_sonde # Daily profile data 

# LOESS regression of chlorophyll for pattern - not analysis ##==========================
# Separate to just chlorophyll-a measurements 
chl = hort_sonde %>%
  select(pond_id, doy, chla)
chl

# Chlorophyll-a by pond (easier to apply LOESS fit if separated) #
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

set.seed(55)

#======= LOW ========# 
# loess low pulse 
alg_B_loess = loess(chla ~ doy, data = algB, span = 0.10) # 10% span
alg_B_loess

# get smoothed output
alg_B_smooth = predict(alg_B_loess, se = TRUE)

# loess low reference
alg_F_loess = loess(chla ~ doy, data = algF, span = 0.10) # 10% span
alg_F_loess

# get smoothed output
alg_F_smooth = predict(alg_F_loess, se = TRUE) 

#======= INTERMEDIATE ========# 
# loess intermediate pulse
alg_A_loess = loess(chla ~ doy, data = algA, span = 0.10) # 10% span
alg_A_loess

# get smoothed output
alg_A_smooth = predict(alg_A_loess, se = TRUE)

# loess intermediate reference
alg_D_loess = loess(chla ~ doy, data = algD, span = 0.10) # 10% span
alg_D_loess

# get smoothed output
alg_D_smooth = predict(alg_D_loess, se = TRUE) 

#======= HIGH ========# 
# loess intermediate pulse
alg_C_loess = loess(chla ~ doy, data = algC, span = 0.10) # 10% span
alg_C_loess

# get smoothed output
alg_C_smooth = predict(alg_C_loess, se = TRUE)

# loess intermediate reference
alg_E_loess = loess(chla ~ doy, data = algE, span = 0.10) # 10% span
alg_E_loess

# get smoothed output
alg_E_smooth = predict(alg_E_loess, se = TRUE) 


# Check Plots of Chlorophyll #========================
windows(height = 3, width = 6)

#pdf(file = "C:/Users/tjbut/Downloads/plot-test.pdf", 
 #   height = 3, 
  #  width = 6)
par(mfrow =c(1,3), mai = c(0.3,0.1,0.1,0.1), omi = c(0.5,0.7,0.5,0.1))

plot(algF$chla, x=algF$doy, type = 'p', pch = 20, cex=1.5, xlab = '',
     ylab = '', xlim=c(140, 245), ylim=c(0, 35), col = ref_col)
polygon(c(142:240, 240:142), c(alg_F_smooth$fit - alg_F_smooth$se.fit, 
                               rev(alg_F_smooth$fit + alg_F_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(alg_F_smooth$fit, x=algF$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(algB$chla, x=algB$doy, type = 'p', pch = 20, cex=1.5, xlab = '',
     ylab = '', xlim=c(140, 245), ylim=c(0, 35), col = low_col_F)
polygon(c(142:240, 240:142), c(alg_B_smooth$fit - alg_B_smooth$se.fit, 
                               rev(alg_B_smooth$fit + alg_B_smooth$se.fit)), 
        col = low_col_F, border = NA)
lines(alg_B_smooth$fit, x=algB$doy, col=low_col_B, lwd = 2)
mtext(side = 2, line = 3.2, 
      expression('Chlorophyll-'~italic(a)), cex = 9/12)
mtext(side = 2, line = 2, 
      expression('Biomass' ~"("*mu*g~L^-1*")"), cex = 9/12)



#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

plot(algD$chla, x=algD$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 35), col = ref_col, col.axis = transparent)
polygon(c(142:240, 240:142), c(alg_D_smooth$fit - alg_D_smooth$se.fit, 
                               rev(alg_D_smooth$fit + alg_D_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(alg_D_smooth$fit, x=algD$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(algA$chla, x=algA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
      xlim=c(140, 245), ylim=c(0, 35), col = int_col_D, yaxt= 'n')
polygon(c(142:240, 240:142), c(alg_A_smooth$fit - alg_A_smooth$se.fit, 
                               rev(alg_A_smooth$fit + alg_A_smooth$se.fit)), 
        col = int_col_D, border = NA)
lines(alg_A_smooth$fit, x=algA$doy, col=int_col_D, lwd = 2)
mtext(side = 1, line = 3, "Day of Year, 2020", cex = 10/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

plot(algE$chla, x=algE$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 35), col = ref_col, col.axis = transparent)
polygon(c(142:240, 240:142), c(alg_E_smooth$fit - alg_E_smooth$se.fit, 
                               rev(alg_E_smooth$fit + alg_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(alg_E_smooth$fit, x=algE$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(algC$chla, x=algC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 35), col = high_col_E, yaxt = 'n')
polygon(c(142:240, 240:142), c(alg_C_smooth$fit - alg_C_smooth$se.fit, 
                               rev(alg_C_smooth$fit + alg_C_smooth$se.fit)), 
        col = high_col_E, border = NA)
lines(alg_C_smooth$fit, x=algC$doy, col=high_col_E, lwd = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# Create the pdf of the plot 
# dev.off()

# LOESS regression of GPP for pattern - not analysis ##==========================

# Data # 
metab # Metabolism data

# Separate to just GPP measurements 
gpp = metab %>%
  select(pond_id, doy, GPP)
gpp

# GPP by pond (easier to apply LOESS fit if separated) #
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

set.seed(55)

#======= LOW ========# 
# loess low pulse 
gpp_B_loess = loess(GPP ~ doy, data = gppB, span = 0.10) # 10% span
gpp_B_loess # 89 obsv

# get smoothed output
gpp_B_smooth = predict(gpp_B_loess, se = TRUE)

# loess low reference
gpp_F_loess = loess(GPP ~ doy, data = gppF, span = 0.10) # 10% span
gpp_F_loess # 92 obsv

# get smoothed output
gpp_F_smooth = predict(gpp_F_loess, se = TRUE) 

#======= INTERMEDIATE ========# 
# loess intermediate pulse
gpp_A_loess = loess(GPP ~ doy, data = gppA, span = 0.10) # 10% span
gpp_A_loess

# get smoothed output
gpp_A_smooth = predict(gpp_A_loess, se = TRUE)

# loess intermediate reference
gpp_D_loess = loess(GPP ~ doy, data = gppD, span = 0.10) # 10% span
gpp_D_loess

# get smoothed output
gpp_D_smooth = predict(gpp_D_loess, se = TRUE) 

#======= HIGH ========# 
# loess intermediate pulse
gpp_C_loess = loess(GPP ~ doy, data = gppC, span = 0.10) # 10% span
gpp_C_loess

# get smoothed output
gpp_C_smooth = predict(gpp_C_loess, se = TRUE)

# loess intermediate reference
gpp_E_loess = loess(GPP ~ doy, data = gppE, span = 0.10) # 10% span
gpp_E_loess

# get smoothed output
gpp_E_smooth = predict(gpp_E_loess, se = TRUE) 

# Check Plots for GPP #========================
windows(height = 3, width = 6)

#pdf(file = "C:/Users/tjbut/Downloads/plot-test.pdf", 
#   height = 3, 
#  width = 6)
par(mfrow =c(1,3), mai = c(0.3,0.1,0.1,0.1), omi = c(0.5,0.7,0.5,0.1))

plot(gppF$GPP, x=gppF$doy, type = 'p', pch = 20, cex=1.5, xlab = '',
     ylab = '', xlim=c(140, 245), ylim=c(0, 20), col = ref_col)
polygon(c(gppF$doy, rev(gppF$doy)), c(gpp_F_smooth$fit - gpp_F_smooth$se.fit, 
                               rev(gpp_F_smooth$fit + gpp_F_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(gpp_F_smooth$fit, x=gppF$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(gppB$GPP, x=gppB$doy, type = 'p', pch = 20, cex=1.5, xlab = '',
     ylab = '', xlim=c(140, 245), ylim=c(0, 20), col = low_col_F)
polygon(c(gppB$doy, rev(gppB$doy)), c(gpp_B_smooth$fit - gpp_B_smooth$se.fit, 
                               rev(gpp_B_smooth$fit + gpp_B_smooth$se.fit)), 
        col = low_col_F, border = NA)
lines(gpp_B_smooth$fit, x=gppB$doy, col=low_col_B, lwd = 2)
mtext(side = 2, line = 2, 'Gross Primary Production', cex = 9/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

plot(gppD$GPP, x=gppD$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 20), col = ref_col, col.axis = transparent)
polygon(c(gppD$doy, rev(gppD$doy)), c(gpp_D_smooth$fit - gpp_D_smooth$se.fit, 
                               rev(gpp_D_smooth$fit + gpp_D_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(gpp_D_smooth$fit, x=gppD$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(gppA$GPP, x=gppA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 20), col = int_col_D, yaxt= 'n')
polygon(c(gppA$doy, rev(gppA$doy)), c(gpp_A_smooth$fit - gpp_A_smooth$se.fit, 
                               rev(gpp_A_smooth$fit + gpp_A_smooth$se.fit)), 
        col = int_col_D, border = NA)
lines(gpp_A_smooth$fit, x=gppA$doy, col=int_col_D, lwd = 2)
mtext(side = 1, line = 3, "Day of Year, 2020", cex = 10/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

plot(gppE$GPP, x=gppE$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 20), col = ref_col, col.axis = transparent)
polygon(c(gppE$doy, rev(gppE$doy)), c(gpp_E_smooth$fit - gpp_E_smooth$se.fit, 
                               rev(gpp_E_smooth$fit + gpp_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(gpp_E_smooth$fit, x=gppE$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(gppC$GPP, x=gppC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 20), col = high_col_E, yaxt = 'n')
polygon(c(gppC$doy, rev(gppC$doy)), c(gpp_C_smooth$fit - gpp_C_smooth$se.fit, 
                               rev(gpp_C_smooth$fit + gpp_C_smooth$se.fit)), 
        col = high_col_E, border = NA)
lines(gpp_C_smooth$fit, x=gppC$doy, col=high_col_E, lwd = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)


# LOESS regression of R for pattern - not analysis ##==========================

# Data # 
metab # Metabolism data 

# Separate to just respiration measurements 
resp = metab %>%
  select(pond_id, doy, R)
resp

# R by pond (easier to apply LOESS fit if separated) #
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

set.seed(55)

#======= LOW ========# 
# loess low pulse 
resp_B_loess = loess(R ~ doy, data = respB, span = 0.10) # 10% span
resp_B_loess # 89 obsv

# get smoothed output
resp_B_smooth = predict(resp_B_loess, se = TRUE)

# loess low reference
resp_F_loess = loess(R ~ doy, data = respF, span = 0.10) # 10% span
resp_F_loess # 92 obsv

# get smoothed output
resp_F_smooth = predict(resp_F_loess, se = TRUE) 

#======= INTERMEDIATE ========# 
# loess intermediate pulse
resp_A_loess = loess(R ~ doy, data = respA, span = 0.10) # 10% span
resp_A_loess

# get smoothed output
resp_A_smooth = predict(resp_A_loess, se = TRUE)

# loess intermediate reference
resp_D_loess = loess(R ~ doy, data = respD, span = 0.10) # 10% span
resp_D_loess

# get smoothed output
resp_D_smooth = predict(resp_D_loess, se = TRUE) 

#======= HIGH ========# 
# loess intermediate pulse
resp_C_loess = loess(R ~ doy, data = respC, span = 0.10) # 10% span
resp_C_loess

# get smoothed output
resp_C_smooth = predict(resp_C_loess, se = TRUE)

# loess intermediate reference
resp_E_loess = loess(R ~ doy, data = respE, span = 0.10) # 10% span
resp_E_loess

# get smoothed output
resp_E_smooth = predict(resp_E_loess, se = TRUE) 

# Check Plots for R #========================
windows(height = 3, width = 6)

#pdf(file = "C:/Users/tjbut/Downloads/plot-test.pdf", 
#   height = 3, 
#  width = 6)
par(mfrow =c(1,3), mai = c(0.3,0.1,0.1,0.1), omi = c(0.5,0.7,0.5,0.1))

plot(respF$R, x=respF$doy, type = 'p', pch = 20, cex=1.5, xlab = '',
     ylab = '', xlim=c(140, 245), ylim=c(-20, 0), col = ref_col)
polygon(c(respF$doy, rev(respF$doy)), c(resp_F_smooth$fit - resp_F_smooth$se.fit, 
                                      rev(resp_F_smooth$fit + resp_F_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(resp_F_smooth$fit, x=respF$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(respB$R, x=respB$doy, type = 'p', pch = 20, cex=1.5, xlab = '',
     ylab = '', xlim=c(140, 245), ylim=c(-20, 0), col = low_col_F)
polygon(c(respB$doy, rev(respB$doy)), c(resp_B_smooth$fit - resp_B_smooth$se.fit, 
                                      rev(resp_B_smooth$fit + resp_B_smooth$se.fit)), 
        col = low_col_F, border = NA)
lines(resp_B_smooth$fit, x=respB$doy, col=low_col_B, lwd = 2)
mtext(side = 2, line = 2, 'Respiration', cex = 9/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)

plot(respD$R, x=respD$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(-20, 0), col = ref_col, col.axis = transparent)
polygon(c(respD$doy, rev(respD$doy)), c(resp_D_smooth$fit - resp_D_smooth$se.fit, 
                                      rev(resp_D_smooth$fit + resp_D_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(resp_D_smooth$fit, x=respD$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(respA$R, x=respA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(-20, 0), col = int_col_D, yaxt= 'n')
polygon(c(respA$doy, rev(respA$doy)), c(resp_A_smooth$fit - resp_A_smooth$se.fit, 
                                      rev(resp_A_smooth$fit + resp_A_smooth$se.fit)), 
        col = int_col_D, border = NA)
lines(resp_A_smooth$fit, x=respA$doy, col=int_col_D, lwd = 2)
mtext(side = 1, line = 3, "Day of Year, 2020", cex = 10/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)

plot(respE$R, x=respE$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(-20, 0), col = ref_col, col.axis = transparent)
polygon(c(respE$doy, rev(respE$doy)), c(resp_E_smooth$fit - resp_E_smooth$se.fit, 
                                      rev(resp_E_smooth$fit + resp_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(resp_E_smooth$fit, x=respE$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(respC$R, x=respC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(-20, 0), col = high_col_E, yaxt = 'n')
polygon(c(respC$doy, rev(respC$doy)), c(resp_C_smooth$fit - resp_C_smooth$se.fit, 
                                      rev(resp_C_smooth$fit + resp_C_smooth$se.fit)), 
        col = high_col_E, border = NA)
lines(resp_C_smooth$fit, x=respC$doy, col=high_col_E, lwd = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
