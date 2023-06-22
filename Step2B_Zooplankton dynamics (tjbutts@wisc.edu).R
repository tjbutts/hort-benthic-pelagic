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

# Macroinvertebrate biomass #=======================
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

zpA = int_zoop %>% #pulse, int 
  filter(treatment == 'pulsed')
zpD = int_zoop %>% #reference, int
  filter(treatment == 'reference')

zpC = high_zoop %>% #pulse, high 
  filter(treatment == 'pulsed')
zpE = high_zoop %>% #reference, high 
  filter(treatment == 'reference')


## Zooplankton Final Plot ========================
# Set plot dimensions # 
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

# Low # 
plot(log(zpF$biomass), x=zpF$doy, type = 'o', pch = 20, cex=1.5, xlab = '', col.axis = transparent, lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = ref_col, yaxt='n', cex.axis = 1.2 )
axis(side=2,
      at=c(log(0.01),log(0.02),log(0.03),log(0.04),log(0.05),log(0.06),log(0.07),log(0.08),log(0.09),
           log(.1),log(.2),log(.3),log(.4),log(.5),log(.6),log(.7),log(.8),log(.9),
           log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '0.1', '', '', '', '', '', '', '', '',
                '1', '', '', '', '', '', '', '', '', 
                '10', '', '','','','','','','','100','','','','','','','800'),
     las=0, cex.axis=1.2)
mtext(side = 3, line = 0.1, 'Low Coupling', cex = 11/12)

par(new=T) # add new smooth to same plot 

plot(log(zpB$biomass), x=zpB$doy, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = low_col_B, col.axis = transparent)
mtext(side = 2, line = 3.2, 
      expression('Zooplankton'), cex = 11/12)
mtext(side = 2, line = 2, 
      expression('Biomass' ~"("*mu*g~L^-1*")"), cex = 11/12)
text(141, log(800), 'A', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# Intermediate #
plot(log(zpD$biomass), x=zpD$doy, type = 'o', pch = 20, cex=1.5, xlab = '', col.axis = transparent, lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = ref_col, yaxt='n', cex.axis = 1.2 )
axis(side=2,
     at=c(log(0.01),log(0.02),log(0.03),log(0.04),log(0.05),log(0.06),log(0.07),log(0.08),log(0.09),
          log(.1),log(.2),log(.3),log(.4),log(.5),log(.6),log(.7),log(.8),log(.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
          log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
                '', '', '', '', '', '', '', '', '', 
                '', '', '','','','','','','','','','','','','','',''),
     las=0, cex.axis=1.2)
mtext(side = 3, line = 0.1, 'Intermediate Coupling', cex = 11/12)

par(new=T) # add new smooth to same plot 

plot(log(zpA$biomass), x=zpA$doy, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = int_col_A, col.axis = transparent)
text(141, log(800), 'B', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# High # 
plot(log(zpE$biomass), x=zpE$doy, type = 'o', pch = 20, cex=1.5, xlab = '', col.axis = transparent, lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = ref_col, yaxt='n', cex.axis = 1.2 )
axis(side=2,
     at=c(log(0.01),log(0.02),log(0.03),log(0.04),log(0.05),log(0.06),log(0.07),log(0.08),log(0.09),
          log(.1),log(.2),log(.3),log(.4),log(.5),log(.6),log(.7),log(.8),log(.9),
          log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
          log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
                '', '', '', '', '', '', '', '', '', 
                '', '', '','','','','','','','','','','','','','',''),
     las=0, cex.axis=1.2)
mtext(side = 3, line = 0.1, 'High Coupling', cex = 11/12)

par(new=T) # add new smooth to same plot 

plot(log(zpC$biomass), x=zpC$doy, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim=c(log(0.01), log(800)), col = high_col_C, col.axis = transparent)
text(141, log(800), 'C', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# MIV # 
mivB = low_miv %>% #pulse, low 
  filter(treatment == 'pulsed')
mivF = low_miv %>% #reference, low
  filter(treatment == 'reference')

mivA = int_miv %>% #pulse, int 
  filter(treatment == 'pulsed')
mivD = int_miv %>% #reference, int
  filter(treatment == 'reference')

mivC = high_miv %>% #pulse, high 
  filter(treatment == 'pulsed')
mivE = high_miv %>% #reference, high 
  filter(treatment == 'reference')

# Low # 
plot(log(mivF$density), x=mivF$doy, type = 'o', pch = 20, cex=1.5, xlab = '', col.axis = transparent, lwd = 3,
     ylab = '', xlim=c(140, 245),  ylim = c(log(300), log(8000)), col = ref_col, yaxt='n', cex.axis = 1.2 )
axis(side=2,
     at=c(log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000)), #Where the tick marks should be drawn
     labels = c('300','','','','','','','1000', '','','','','','',''),
     las=0)

par(new=T) # add new smooth to same plot 

plot(log(mivB$density), x=mivB$doy, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim = c(log(300), log(8000)), col = low_col_B, col.axis = transparent)
mtext(side = 2, line = 3.2, 
      expression('Macroinvertebrate'), cex = 11/12)
mtext(side = 2, line = 1.6, 
      expression('Density' ~"("~`#`~m^-2*")"), cex = 11/12)
text(141, log(8000), 'D', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# Intermediate #
plot(log(mivD$density), x=mivD$doy, type = 'o', pch = 20, cex=1.5, xlab = '', col.axis = transparent, lwd = 3,
     ylab = '', xlim=c(140, 245), ylim = c(log(300), log(8000)), col = ref_col, yaxt='n', cex.axis = 1.2 )
axis(side=2,
     at=c(log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000), log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','','','','','','','', '','','','','','','','','',''),
     las=0)

par(new=T) # add new smooth to same plot 

plot(log(mivA$density), x=mivA$doy, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim = c(log(300), log(8000)), col = int_col_A, col.axis = transparent)
text(141, log(8000), 'E', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# High # 
plot(log(mivE$density), x=mivE$doy, type = 'o', pch = 20, cex=1.5, xlab = '', col.axis = transparent, lwd = 3,
     ylab = '', xlim=c(140, 245), ylim = c(log(300), log(8000)), col = ref_col, yaxt='n', cex.axis = 1.2 )
axis(side=2,
     at=c(log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000), log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','','','','','','','', '','','','','','','','','',''),
     las=0)


par(new=T) # add new smooth to same plot 

plot(log(mivC$density), x=mivC$doy, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim = c(log(300), log(8000)), col = high_col_C, col.axis = transparent)
text(141, log(8000), 'F', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# Periphyton # 
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

# periphyton # 
periB = low_peri %>% #pulse, low 
  filter(treatment == 'pulsed')
periF = low_peri %>% #reference, low
  filter(treatment == 'reference')

periA = int_peri %>% #pulse, int 
  filter(treatment == 'pulsed')
periD = int_peri %>% #reference, int
  filter(treatment == 'reference')

periC = high_peri %>% #pulse, high 
  filter(treatment == 'pulsed')
periE = high_peri %>% #reference, high 
  filter(treatment == 'reference')

# Low # 
plot(periF$biomass_area, x=periF$collect, type = 'o', pch = 20, cex=1.5, xlab = '',  lwd = 3,
     ylab = '', xlim=c(140, 245),   ylim = c(0, 0.4),  col = ref_col, yaxt='n')
axis(side = 2, 
     at = c(0, 0.1, 0.2, 0.3, 0.4), 
     labels = c('0', '0.1', '0.2', '0.3', '0.4'), 
     las=0)

par(new=T) # add new smooth to same plot 

plot(periB$biomass_area, x=periB$collect, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245),  ylim = c(0, 0.4),  col = low_col_B, col.axis = transparent)
mtext(side = 2, line = 3, 
      expression('Periphyton Areal'), cex = 11/12)
mtext(side = 2, line = 1.8, 
      expression('Biomass' ~"("*mu*g~cm^-2*")"), cex = 11/12)
text(141, 0.4, 'G', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

# Intermediate #
plot(periD$biomass_area, x=periD$collect, type = 'o', pch = 20, cex=1.5, xlab = '',  lwd = 3,
     ylab = '', xlim=c(140, 245), ylim = c(0, 0.4), col = ref_col, yaxt='n')
axis(side = 2, 
     at = c(0, 0.1, 0.2, 0.3, 0.4), 
     labels = c('', '', '', '', ''), 
     las=0)

par(new=T) # add new smooth to same plot 

plot(periA$biomass_area, x=periA$collect, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim = c(0, 0.4), col = int_col_A, col.axis = transparent)
text(141, 0.4, 'H', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
mtext(side = 1, 'Day of Year, 2020', line = 2)

# High # 
plot(periE$biomass_area, x=periE$collect, type = 'o', pch = 20, cex=1.5, xlab = '', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim = c(0, 0.4), col = ref_col, yaxt='n')
axis(side = 2, 
     at = c(0, 0.1, 0.2, 0.3, 0.4), 
     labels = c('', '', '', '', ''), 
     las=0)


par(new=T) # add new smooth to same plot 

plot(periC$biomass_area, x=periC$collect, type = 'o', pch = 20, cex=1.5, xlab = '', yaxt = 'n', lwd = 3,
     ylab = '', xlim=c(140, 245), ylim = c(0, 0.4), col = high_col_C, col.axis = transparent)
text(141, 0.4, 'I', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
