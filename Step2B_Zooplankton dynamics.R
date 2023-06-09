## Step2B_Food Web Context - Dynamics ##

## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022

#============================================#
# STEP 2: Zooplankton, Zoobenthos, Periphyton 
#============================================#

# Plotting #==================

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse) 

# ========= COLLATE DATA =========== #

## Zooplankton Biomass ##======================
hort_zoop 

daily_zp_biomass = hort_zoop %>% 
  group_by(pond_id, doy) %>%
  summarize(biomass = sum(biomass)) %>%
  ungroup()
daily_zp_biomass

low_zoop = daily_zp_biomass %>%
  filter(pond_id == 'B' | pond_id == 'F') %>% 
  mutate(treatment = case_when(
    pond_id == 'B' ~ 'pulsed', 
    pond_id == 'F' ~ 'reference'))
low_zoop

int_zoop = daily_zp_biomass %>%
  filter(pond_id == 'A' | pond_id == 'D') %>% 
  mutate(treatment = case_when(
    pond_id == 'A' ~ 'pulsed', 
    pond_id == 'D' ~ 'reference'))
int_zoop

high_zoop = daily_zp_biomass %>%
  filter(pond_id == 'C' | pond_id == 'E') %>% 
  mutate(treatment = case_when(
    pond_id == 'C' ~ 'pulsed', 
    pond_id == 'E' ~ 'reference'))
high_zoop

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

# LOESS regression of zooplankton for pattern - not analysis ##==========================
# Separate to single pond measurements # 
zpB = low_zoop %>% #pulse, low 
  filter(treatment == 'pulsed')
zpF = low_zoop %>% #reference, low
  filter(treatment == 'reference')

zpA = int_zoop %>% #pulse, intermediate
  filter(treatment == 'pulsed')
zpD = int_zoop %>% #reference, intermediate
  filter(treatment == 'reference')

zpC = high_zoop %>% #pulse, high
  filter(treatment == 'pulsed')
zpE = high_zoop %>% #reference, high 
  filter(treatment == 'reference')

## Chlorophyll Final plot ========================
# Set plot dimensions # 
plot(log(zpF$biomass), x=zpF$doy, type = 'o', pch = 20, cex=1.5, xlab = '', col.axis = transparent,lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = ref_col, yaxt='n', cex.axis = 1.2 )
mtext(side = 3, line = 0.1, 'Low Coupling', cex = 11/12)
axis(side=2,
     at=c( log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08),log(0.09), log(0.1),
           log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9),
           log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('0.01', '', '', '', '', '', '', '', '',
                '0.1', '', '', '', '', '', '', '', '','1', '', '', '', '', '', '', '', '', 
                '10', '', '','','','','','','','100','','','','','','','800'),
     las=0)

par(new=T) # add new smooth to same plot 

plot(log(zpB$biomass), x=zpB$doy, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = low_col_F, col.axis = transparent)
mtext(side = 2, line = 3.2, 
      expression('Chlorophyll-'~italic(a)), cex = 11/12)
mtext(side = 2, line = 2, 
      expression('Biomass' ~"("*mu*g~L^-1*")"), cex = 11/12)
text(140, log(800), 'A', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

plot(log(zpD$biomass), x=zpD$doy, type = 'o', pch = 20, cex=1.5, xlab = '', col.axis = transparent,lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = ref_col, yaxt='n', cex.axis = 1.2 )
mtext(side = 3, line = 0.1, 'Intermediate Coupling', cex = 11/12)
axis(side=2,
     at=c( log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08),log(0.09), log(0.1),
           log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9),
           log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('0.01', '', '', '', '', '', '', '', '',
                '0.1', '', '', '', '', '', '', '', '','1', '', '', '', '', '', '', '', '', 
                '10', '', '','','','','','','','100','','','','','','','800'),
     las=0)

par(new=T) # add new smooth to same plot 

plot(log(zpA$biomass), x=zpA$doy, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = int_col_A, col.axis = transparent)
mtext(side = 2, line = 3.2, 
      expression('Chlorophyll-'~italic(a)), cex = 11/12)
mtext(side = 2, line = 2, 
      expression('Biomass' ~"("*mu*g~L^-1*")"), cex = 11/12)
text(140, log(800), 'B', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

plot(log(zpE$biomass), x=zpE$doy, type = 'o', pch = 20, cex=1.5, xlab = '', col.axis = transparent,lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = ref_col, yaxt='n', cex.axis = 1.2 )
mtext(side = 3, line = 0.1, 'High Coupling', cex = 11/12)
axis(side=2,
     at=c( log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08),log(0.09), log(0.1),
           log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9),
           log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('0.01', '', '', '', '', '', '', '', '',
                '0.1', '', '', '', '', '', '', '', '','1', '', '', '', '', '', '', '', '', 
                '10', '', '','','','','','','','100','','','','','','','800'),
     las=0)

par(new=T) # add new smooth to same plot 

plot(log(zpC$biomass), x=zpC$doy, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = high_col_C, col.axis = transparent)
mtext(side = 2, line = 3.2, 
      expression('Chlorophyll-'~italic(a)), cex = 11/12)
mtext(side = 2, line = 2, 
      expression('Biomass' ~"("*mu*g~L^-1*")"), cex = 11/12)
text(140, log(800), 'C', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
