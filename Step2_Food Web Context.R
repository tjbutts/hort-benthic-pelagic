## Step2_Food Web Context ##

## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022

#============================================#
# STEP 2: Zooplankton, Zoobenthos, Periphyton 
#============================================#

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

## Zoobenthos Density ##========================
hort_mivdensity

daily_miv_density = hort_mivdensity %>%
  filter(gear == 'HS') %>%
  group_by(pond_id, doy) %>%
  summarize(density = sum(density)) %>%
  ungroup()
daily_miv_density

max(daily_miv_density$density) # 7845.07
min(daily_miv_density$density) # 352.1127

low_miv = daily_miv_density %>%
  filter(pond_id == 'B' | pond_id == 'F') %>% 
  mutate(treatment = case_when(
    pond_id == 'B' ~ 'pulsed', 
    pond_id == 'F' ~ 'reference'))
low_miv

int_miv = daily_miv_density %>%
  filter(pond_id == 'A' | pond_id == 'D') %>% 
  mutate(treatment = case_when(
    pond_id == 'A' ~ 'pulsed', 
    pond_id == 'D' ~ 'reference'))
int_miv

high_miv = daily_miv_density %>%
  filter(pond_id == 'C' | pond_id == 'E') %>% 
  mutate(treatment = case_when(
    pond_id == 'C' ~ 'pulsed', 
    pond_id == 'E' ~ 'reference'))
high_miv

## Periphyton Biomass##======================
hort_periphy

daily_peri_areabiom = hort_periphy %>%
  group_by(pond_id, collect) %>%
  summarize(biomass_area = sum(biomass_area_cm2)) %>%
  ungroup()
daily_peri_areabiom

max(daily_peri_areabiom$biomass_area) # 0.3273
min(daily_peri_areabiom$biomass_area) # 0.0130

low_peri = daily_peri_areabiom %>%
  filter(pond_id == 'B' | pond_id == 'F') %>% 
  mutate(treatment = case_when(
    pond_id == 'B' ~ 'pulsed', 
    pond_id == 'F' ~ 'reference'))
low_peri

int_peri = daily_peri_areabiom %>%
  filter(pond_id == 'A' | pond_id == 'D') %>% 
  mutate(treatment = case_when(
    pond_id == 'A' ~ 'pulsed', 
    pond_id == 'D' ~ 'reference'))
int_peri

high_peri = daily_peri_areabiom %>%
  filter(pond_id == 'C' | pond_id == 'E') %>% 
  mutate(treatment = case_when(
    pond_id == 'C' ~ 'pulsed', 
    pond_id == 'E' ~ 'reference'))
high_peri

# ========= PLOTTING COLORS ======== # 
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

############# Boxplot Figure #######################

# Set Margins for plot # 
# Window for checking plot 
windows(height = 6, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure2.pdf", 
 #   height = 6, 
 #    width = 6)

# Set dimensions for figure array # 
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Plot Zooplankton Biomass #===============
boxplot(log(biomass)~treatment, data = low_zoop, ylim = c(log(1), log(800)), 
        yaxt = 'n', col=c(low_col_B, ref_col), 
        ylab = 'ZP Biomass (ug/L)', col.axis = transparent, at = c(3,5), 
        cex.axis = 1.5)
mtext(side = 3, line = 0.1, 'Low Coupling', cex = 11/12)
axis(side=2,
     at=c( log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','','','','','','','800'),
     las=0)
mtext(side = 2, line = 3.2, 
      expression('Zooplankton'), cex = 11/12)
mtext(side = 2, line = 1.8, 
      expression('Biomass' ~"("*mu*g~L^-1*")"), cex = 11/12)
# Add data points # 
stripchart(log(biomass)~treatment, vertical = TRUE, data = low_zoop, at = c(3,5),
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
text(2.55, log(800), 'A', font = 2)

boxplot(log(biomass)~treatment, data = int_zoop, ylim = c(log(1), log(800)), col.axis = transparent,
        yaxt = 'n', col=c(int_col_A, ref_col), 
        ylab = '', at = c(3,5))
mtext(side = 3, line = 0.1, 'Intermediate', cex = 11/12)
# Add data points # 
stripchart(log(biomass)~treatment, vertical = TRUE, data = int_zoop, at = c(3,5),
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c( log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('', '', '', '', '', '', '', '', '', '', '', '','','','','','','','','','','','','','',''),
     las=0, cex.axis=.8)
text(2.55, log(800), 'B', font = 2)

boxplot(log(biomass)~treatment, data = high_zoop, ylim = c(log(1), log(800)), 
        yaxt = 'n', col=c(high_col_C, ref_col), col.axis = transparent, 
        ylab = '', at = c(3,5))
mtext(side = 3, line = 0.1, 'High Coupling', cex = 11/12)

# Add data points # 
stripchart(log(biomass)~treatment, vertical = TRUE, data = low_zoop, at = c(3,5), 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c( log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('', '', '', '', '', '', '', '', '', '', '', '','','','','','','','','','','','','','',''),
     las=0, cex.axis=.8)
text(2.55, log(800), 'C', font = 2)

## Plot Zoobenthos Density ##=======================

boxplot(log(density)~treatment, data = low_miv, ylim = c(log(300), log(14000)), 
        yaxt = 'n', col=c(low_col_B, ref_col), col.axis = transparent,  
        ylab = '', at = c(3,5))
# Add data points # 
stripchart(log(density)~treatment, vertical = TRUE, data = low_miv, at = c(3,5),
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex=1.5)
axis(side=2,
     at=c(log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000), log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('300','','','','','','','1000', '','','','','','','','','',''),
     las=0)
mtext(side = 2, line = 3.5, 
      expression('Macroinvertebrate'), cex = 11/12)
mtext(side = 2, line = 1.8, 
      expression('Density' ~"(#"~L^-1*")"), cex = 11/12)
text(2.55, log(14000), 'D', font = 2)

boxplot(log(density)~treatment, data = int_miv, ylim = c(log(300), log(14000)), 
        yaxt = 'n', col=c(int_col_A, ref_col), col.axis = transparent,  
        ylab = '', at = c(3,5))
# Add data points # 
stripchart(log(density)~treatment, vertical = TRUE, data = int_miv, at = c(3,5),
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000), log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','','','','','','','', '','','','','','','','','',''),
     las=0)
text(2.55, log(14000), 'E', font = 2)

boxplot(log(density)~treatment, data = high_miv, ylim = c(log(300), log(14000)), 
        yaxt = 'n', col=c(high_col_C, ref_col), col.axis = transparent,  
        ylab = '', at = c(3,5))
# Add data points # 
stripchart(log(density)~treatment, vertical = TRUE, data = high_miv, at = c(3,5), 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000), log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','','','','','','','', '','','','','','','','','',''),
     las=0)
text(2.55, log(14000), 'F', font = 2)

## Plot Periphyton areal biomass ##================= 
boxplot(biomass_area~treatment, data = low_peri, ylim = c(0, 0.4), 
        col=c(low_col_B, ref_col), at = c(3,5), yaxt = 'n',
        ylab = '', cex.axis = 1.4)
axis(side = 2, 
     at = c(0, 0.1, 0.2, 0.3, 0.4), 
     labels = c('0', '0.1', '0.2', '0.3', '0.4'), 
     las=0)
# Add data points # 
stripchart(biomass_area~treatment, vertical = TRUE, data = low_peri, at = c(3,5), 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex=1.5)
mtext(side = 2, line = 3.3, 
      expression('Periphyton Areal'), cex = 11/12)
mtext(side = 2, line = 1.8, 
      expression('Biomass' ~"("*mu*g~cm^-2*")"), cex = 11/12)
text(2.55, 0.4, 'G', font = 2)

boxplot(biomass_area~treatment, data = int_peri, ylim = c(0,0.4), 
        col=c(int_col_A, ref_col), yaxt = 'n', at = c(3,5),
        ylab = '', cex.axis = 1.4)
axis(side = 2, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = F)
# Add data points # 
stripchart(biomass_area~treatment, vertical = TRUE, data = int_peri, at=c(3,5), 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
text(2.55, 0.4, 'H', font = 2)

boxplot(biomass_area~treatment, data = high_peri, ylim = c(0,0.4), 
        col=c(high_col_C, ref_col), yaxt = 'n', at=c(3,5),  
        ylab = '', cex.axis = 1.4)
axis(side = 2, at = c(0, 0.1, 0.2, 0.3, 0.4), labels = F)
# Add data points # 
stripchart(biomass_area~treatment, vertical = TRUE, data = high_peri, at = c(3,5), 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
text(2.55, 0.4, 'I', font = 2)

# Load plot into specified directory # 
#dev.off()
