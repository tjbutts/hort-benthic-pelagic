## Step2_Create Food Web Array ## 

# Create an array with 3 columns and 5 rows 
  # Zoobenthos, Zooplankton, Periphyton, Macrophytes, Nutrients 

# Create boxplots for each row (one for pulsed, one for reference)

# First Generate a 3x1 Plot for each dataset 
library(ggplot2)
library(scales)
library(gridExtra)
library(ggpubr)

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

## Plot the boxplots ## 
windows(height = 9, width = 6)
par(mfrow =c(3,3),omi = c(1.5,0.8,1.5,0.1), mai = c(0.3,0.3,0.1,0.1))
boxplot(log(biomass)~treatment, data = low_zoop, ylim = c(log(1), log(800)), 
        yaxt = 'n', col=c(low_col_B, low_col_F), 
        ylab = 'ZP Biomass (ug/L)')
axis(side=2,
     at=c( log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','','','','','','','800'),
     las=2, cex.axis=.8)
mtext('ZP Biomass (ug/L)', side = 2, line =3, cex=1)
# Add data points # 
stripchart(log(biomass)~treatment, vertical = TRUE, data = low_zoop, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)

boxplot(log(biomass)~treatment, data = int_zoop, ylim = c(log(1), log(800)), 
        yaxt = 'n', col=c(int_col_A, int_col_D), 
        ylab = '')
# Add data points # 
stripchart(log(biomass)~treatment, vertical = TRUE, data = int_zoop, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c( log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','','','','','','','800'),
     las=2, cex.axis=.8)

boxplot(log(biomass)~treatment, data = high_zoop, ylim = c(log(1), log(800)), 
        yaxt = 'n', col=c(high_col_C, high_col_E), 
        ylab = '')
# Add data points # 
stripchart(log(biomass)~treatment, vertical = TRUE, data = low_zoop, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c( log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','','','','','','','800'),
     las=2, cex.axis=.8)


   
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

## Plot the boxplots ## 

boxplot(log(density)~treatment, data = low_miv, ylim = c(log(300), log(10000)), 
        yaxt = 'n', col=c(low_col_B, low_col_F), 
        ylab = '')
# Add data points # 
stripchart(log(density)~treatment, vertical = TRUE, data = low_miv, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex=1.5)
axis(side=2,
     at=c(log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000), log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('300','','','','','','','1000', '','','','','','','','','',''),
     las=2, cex.axis=.8)
mtext('ZB Density (#/L)', side = 2, line =3, cex=1)


boxplot(log(density)~treatment, data = int_miv, ylim = c(log(300), log(14000)), 
        yaxt = 'n', col=c(int_col_A, int_col_D), 
        ylab = '')
# Add data points # 
stripchart(log(density)~treatment, vertical = TRUE, data = int_miv, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000), log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('300','','','','','','','1000', '','','','','','','','','',''),
     las=2, cex.axis=.8)


boxplot(log(density)~treatment, data = high_miv, ylim = c(log(300), log(14000)), 
        yaxt = 'n', col=c(high_col_C, high_col_E), 
        ylab = '')
# Add data points # 
stripchart(log(density)~treatment, vertical = TRUE, data = high_miv, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c(log(300),log(400),log(500),log(600),log(700),log(800), log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000), log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('300','','','','','','','1000', '','','','','','','','','',''),
     las=2, cex.axis=.8)

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


## Plot the boxplots ## 
boxplot(biomass_area~treatment, data = low_peri, ylim = c(0, 0.4), col=c(low_col_B, low_col_F), 
        ylab = '')
# Add data points # 
stripchart(biomass_area~treatment, vertical = TRUE, data = low_peri, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex=1.5)
mtext('Periphyton (ug/m^2)', side = 2, line =3, cex=1)


boxplot(biomass_area~treatment, data = int_peri, ylim = c(0,0.4), 
        yaxt = 'n', col=c(int_col_A, int_col_D), 
        ylab = '')
# Add data points # 
stripchart(biomass_area~treatment, vertical = TRUE, data = int_peri, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)

boxplot(biomass_area~treatment, data = high_peri, ylim = c(0,0.4), 
         col=c(high_col_C, high_col_E), 
        ylab = '')
# Add data points # 
stripchart(biomass_area~treatment, vertical = TRUE, data = high_peri, 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
