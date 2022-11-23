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

# LOESS regression of chlorophyll for pattern - not analysis ##==========================
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


# Plot Chlorophyll #========================
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
lines(alg_F_smooth$fit, x=algF$doy, col=ref_col, lwd = 1.5)

par(new=T) # add new smooth to same plot 

plot(algB$chla, x=algB$doy, type = 'p', pch = 20, cex=1.5, xlab = '',
     ylab = '', xlim=c(140, 245), ylim=c(0, 35), col = low_col_F)
polygon(c(142:240, 240:142), c(alg_B_smooth$fit - alg_B_smooth$se.fit, 
                               rev(alg_B_smooth$fit + alg_B_smooth$se.fit)), 
        col = low_col_F, border = NA)
lines(alg_B_smooth$fit, x=algB$doy, col=low_col_B, lwd = 1.5)
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
lines(alg_D_smooth$fit, x=algD$doy, col=ref_col, lwd = 1.5)

par(new=T) # add new smooth to same plot 

plot(algA$chla, x=algA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
      xlim=c(140, 245), ylim=c(0, 35), col = int_col_D, yaxt= 'n')
polygon(c(142:240, 240:142), c(alg_A_smooth$fit - alg_A_smooth$se.fit, 
                               rev(alg_A_smooth$fit + alg_A_smooth$se.fit)), 
        col = int_col_D, border = NA)
lines(alg_A_smooth$fit, x=algA$doy, col=int_col_D, lwd = 1.5)
mtext(side = 1, line = 3, "Day of Year, 2020", cex = 10/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

plot(algE$chla, x=algE$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 35), col = ref_col, col.axis = transparent)
polygon(c(142:240, 240:142), c(alg_E_smooth$fit - alg_E_smooth$se.fit, 
                               rev(alg_E_smooth$fit + alg_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(alg_E_smooth$fit, x=algE$doy, col=ref_col, lwd = 1.5)

par(new=T) # add new smooth to same plot 

plot(algC$chla, x=algC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 35), col = high_col_E, yaxt = 'n')
polygon(c(142:240, 240:142), c(alg_C_smooth$fit - alg_C_smooth$se.fit, 
                               rev(alg_C_smooth$fit + alg_C_smooth$se.fit)), 
        col = high_col_E, border = NA)
lines(alg_C_smooth$fit, x=algC$doy, col=high_col_E, lwd = 1.5)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# Create the pdf of the plot 
# dev.off()

