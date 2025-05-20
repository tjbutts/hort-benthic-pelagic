# Code used to generate tables and figures in Appendix S1 #======================

# Pull nutrient data for Table 1 # 
nut = hort_field %>% 
  select(pond_id, doy, tp, srp, tn, nox, nhx) %>% 
  mutate(pond_id = case_when(pond_id == "A" ~ 'INTpulse', 
                             pond_id == "B" ~ 'LOWpulse',
                             pond_id == "C" ~ 'HIGHpulse',
                             pond_id == "D" ~ 'INTref', 
                             pond_id == "E" ~ 'HIGHref',
                             pond_id == "F" ~ 'LOWref')) %>% 
  pivot_longer(cols = c(-pond_id, -doy), 
               names_to = "variable",
               values_to = "concentration") %>% 
  pivot_wider(id_cols = c(doy, variable), 
              names_from = pond_id, 
              values_from = concentration) %>% 
  select(doy, variable, LOWpulse, LOWref, INTpulse, INTref, HIGHpulse, HIGHref)
nut

# Pull fish lengths # 
yep = hort_fish_bodysize %>% 
  filter(spp == 'YEP')
yep.m = yep %>% 
  summarize(mean = mean(length, na.rm = T), 
            sd = sd(length, na.rm = T))
yep.m

blg = hort_fish_bodysize %>% 
  filter(spp == 'BLG')
blg.m = blg %>% 
  summarize(mean = mean(length, na.rm = T), 
            sd = sd(length, na.rm = T))
blg.m

lmb = hort_fish_bodysize %>% 
  filter(spp == 'LMB')
lmb.m = lmb %>% 
  summarize(mean = mean(length, na.rm = T), 
            sd = sd(length, na.rm = T))
lmb.m

# Gastric Lavage Table #================== 
diet = hort_fish_gaslav

diet$pond[diet$pond == 'B' | diet$pond == 'F'] <- 'low' 
diet$pond[diet$pond == 'A' | diet$pond == 'D'] <- 'int' 
diet$pond[diet$pond == 'C' | diet$pond == 'E'] <- 'high' 
diet

diet_sum = diet %>% 
  group_by(pond, broad_taxa, fish_id) %>%
  summarize(sum_item = sum(abundance)) %>%
  ungroup() %>%
  arrange(fish_id)
diet_sum  

# Fish length - Weight if of interest # =====================
hort_fish_bodysize

# ========= FOOD WEB CONTEXT BOXPLOTS  =========== #

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

high_col_C = rgb(75, 31, 110, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(75, 31, 110, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(75, 31, 110, max = 255, alpha = 255) #Pond C, Pond E

col=rgb(255,48,48, max=255, alpha=75, names= 'firebrick1') # Extended heat period 

# Plot Zooplankton Biomass #===============
boxplot(log(biomass)~treatment, data = low_zoop, ylim = c(log(0.01), log(800)), 
        yaxt = 'n', col=c(low_col_B, ref_col), 
        ylab = 'ZP Biomass (ug/L)', col.axis = transparent, at = c(3,5), 
        cex.axis = 1.5)
mtext(side = 3, line = 0.1, 'Low Complexity', cex = 11/12)
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
axis(side=2, 
     at=c(log(1), log(10), log(100), log(800)),
     labels = c('1', '10', '100','800'),
     las=0)
axis(side=2, 
     at=c(log(800)),
     labels = c('800'),
     las=0)

mtext(side = 2, line = 3.2, 
      expression('Zooplankton'), cex = 11/12)
mtext(side = 2, line = 1.8, 
      expression('Biomass' ~"("*mu*g~L^-1*")"), cex = 11/12)
# Add data points # 
stripchart(log(biomass)~treatment, vertical = TRUE, data = low_zoop, at = c(3,5),
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
text(2.55, log(800), 'A', font = 2)

boxplot(log(biomass)~treatment, data = int_zoop, ylim = c(log(0.01), log(800)), col.axis = transparent,
        yaxt = 'n', col=c(int_col_A, ref_col), 
        ylab = '', at = c(3,5))
mtext(side = 3, line = 0.1, 'Intermediate', cex = 11/12)
# Add data points # 
stripchart(log(biomass)~treatment, vertical = TRUE, data = int_zoop, at = c(3,5),
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c( log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08),log(0.09), log(0.1),
           log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9),
           log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('', '', '', '', '', '', '', '', '',
                '', '', '', '', '', '', '', '', '','', '', '', '', '', '', '', '', '', 
                '', '', '','','','','','','','','','','','','','',''),
     las=0)
text(2.55, log(800), 'B', font = 2)

boxplot(log(biomass)~treatment, data = high_zoop, ylim = c(log(0.01), log(800)), 
        yaxt = 'n', col=c(high_col_C, ref_col), col.axis = transparent, 
        ylab = '', at = c(3,5))
mtext(side = 3, line = 0.1, 'High Complexity', cex = 11/12)

# Add data points # 
stripchart(log(biomass)~treatment, vertical = TRUE, data = low_zoop, at = c(3,5), 
           method = 'jitter', add = TRUE, pch = 20, col = 'black', cex = 1.5)
axis(side=2,
     at=c( log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08),log(0.09), log(0.1),
           log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9),
           log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('', '', '', '', '', '', '', '', '',
                '', '', '', '', '', '', '', '', '','', '', '', '', '', '', '', '', '', 
                '', '', '','','','','','','','','','','','','','',''),
     las=0)
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
axis(side=2,
     at = c(log(1000), log(10000)),
     labels = c('1000', '10000'),
     las=0)

mtext(side = 2, line = 3.5, 
      expression('Macroinvertebrate'), cex = 11/12)
mtext(side = 2, line = 1.8, 
      expression('Density' ~"(#"~m^-2*")"), cex = 11/12)
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

# Because we will be plotting by pond, make some separate data frames to make life easier
fieldA = hort_field %>% #pulse, int
  filter(pond_id == "A") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))

fieldB = hort_field %>% #pulse, low
  filter(pond_id == "B") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))

fieldC = hort_field %>% #pulse, high
  filter(pond_id == "C") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))

fieldD = hort_field %>% #ref, int
  filter(pond_id == "D") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))

fieldE = hort_field %>% #ref, high
  filter(pond_id == "E") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))

fieldF = hort_field %>% #ref, low
  filter(pond_id == "F") %>%
  filter(!(is.na(tp))) %>%
  mutate(np_ratio = ((tn * 14.01) / 1000) / ((tp * 30.97) / 1000000))

# Total P # ========================
#======= LOW ========# 
# loess low pulse 
field_B_loess = loess(tp ~ doy, data = fieldB, span = 0.20) # 20% span
field_B_loess

# get smoothed output
field_B_smooth = predict(field_B_loess, se = TRUE)

# loess low reference
field_F_loess = loess(tp ~ doy, data = fieldF, span = 0.2) # 10% span
field_F_loess

# get smoothed output
field_F_smooth = predict(field_F_loess, se = TRUE) 

#======= INTERMEDIATE ========# 
# loess intermediate pulse
field_A_loess = loess(tp ~ doy, data = fieldA, span = 0.2) # 10% span
field_A_loess

# get smoothed output
field_A_smooth = predict(field_A_loess, se = TRUE)

# loess intermediate reference
field_D_loess = loess(tp ~ doy, data = fieldD, span = 0.2) # 10% span
field_D_loess

# get smoothed output
field_D_smooth = predict(field_D_loess, se = TRUE) 

#======= HIGH ========# 
# loess intermediate pulse
field_C_loess = loess(tp ~ doy, data = fieldC, span = 0.2) # 10% span
field_C_loess

# get smoothed output
field_C_smooth = predict(field_C_loess, se = TRUE)

# loess intermediate reference
field_E_loess = loess(tp ~ doy, data = fieldE, span = 0.2) # 10% span
field_E_loess

# get smoothed output
field_E_smooth = predict(field_E_loess, se = TRUE) 


# Window for checking plot 
windows(height = 4, width = 5) 

# Set dimensions for figure array # 
par(mfrow =c(2,3), mar = c(1,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

## TP Final plot ========================


# Set plot dimensions # 
plot(fieldF$tp, x=fieldF$doy, type = 'p', pch = 20, cex=1.5, xlab = '', col.axis = transparent,
     ylab = '', xlim=c(140, 245), ylim=c(0,90), col = ref_col, yaxt='n', cex.axis = 1.2 )
polygon(c(fieldF$doy, rev(fieldF$doy)), c(field_F_smooth$fit - field_F_smooth$se.fit, 
                               rev(field_F_smooth$fit + field_F_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(field_F_smooth$fit, x=fieldF$doy, col=ref_col, lwd = 2)
mtext(side = 3, line = 0.1, 'Low Complexity', cex = 11/12)
mtext(side = 2, line = 3.2, 
      expression('Total Phosphorus'), cex = 11/12)
mtext(side = 2, line = 1.6, 
      expression("  ("*mu*g~L^-1*")"), cex = 11/12)

par(new=T) # add new smooth to same plot 

plot(fieldB$tp, x=fieldB$doy, type = 'p', pch = 20, cex=1.5, xlab = '', yaxt = 'n',
     ylab = '', xlim=c(140, 245), ylim=c(0,90), col = low_col_F, col.axis = transparent)
polygon(c(fieldB$doy, rev(fieldB$doy)), c(field_B_smooth$fit - field_B_smooth$se.fit, 
                               rev(field_B_smooth$fit + field_B_smooth$se.fit)), 
        col = low_col_F, border = NA)
lines(field_B_smooth$fit, x=fieldB$doy, col=low_col_B, lwd = 2)

axis(side = 2, at = c(0,10, 20, 30, 40, 50, 60, 70, 80, 90), cex.axis = 1) 
text(141, 90, 'A', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,500, col=col, border=NA)

plot(fieldD$tp, x=fieldD$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,90), col = ref_col, col.axis = transparent)
polygon(c(fieldD$doy, rev(fieldD$doy)), c(field_D_smooth$fit - field_D_smooth$se.fit, 
                               rev(field_D_smooth$fit + field_D_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(field_D_smooth$fit, x=fieldD$doy, col=ref_col, lwd = 2)
mtext(side = 3, line = 0.1, 'Intermediate', cex = 11/12)

par(new=T) # add new smooth to same plot 

plot(fieldA$tp, x=fieldA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,90), col = int_col_D, yaxt= 'n', col.axis = transparent)
polygon(c(fieldA$doy, rev(fieldA$doy)), c(field_A_smooth$fit - field_A_smooth$se.fit, 
                               rev(field_A_smooth$fit + field_A_smooth$se.fit)), 
        col = int_col_D, border = NA)
lines(field_A_smooth$fit, x=fieldA$doy, col=int_col_D, lwd = 2)
text(141, 90, 'B', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,500, col=col, border=NA)

plot(fieldE$tp, x=fieldE$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,90), col = ref_col, col.axis = transparent)
polygon(c(fieldE$doy, rev(fieldE$doy)), c(field_E_smooth$fit - field_E_smooth$se.fit, 
                               rev(field_E_smooth$fit + field_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(field_E_smooth$fit, x=fieldE$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(fieldC$tp, x=fieldC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,90), col = high_col_E, yaxt = 'n', col.axis = transparent)
polygon(c(fieldC$doy, rev(fieldC$doy)), c(field_C_smooth$fit - field_C_smooth$se.fit, 
                               rev(field_C_smooth$fit + field_C_smooth$se.fit)), 
        col = high_col_E, border = NA)
lines(field_C_smooth$fit, x=fieldC$doy, col=high_col_E, lwd = 2)
text(141, 90, 'C', font = 2)
mtext(side = 3, line = 0.1, 'High Complexity', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,500, col=col, border=NA)

## TN Final Plot ## ===========================
# Total N # ========================
#======= LOW ========# 
# loess low pulse 
field_B_loess = loess(tn ~ doy, data = fieldB, span = 0.20) # 10% span
field_B_loess

# get smoothed output
field_B_smooth = predict(field_B_loess, se = TRUE)

# loess low reference
field_F_loess = loess(tn ~ doy, data = fieldF, span = 00.20) # 10% span
field_F_loess

# get smoothed output
field_F_smooth = predict(field_F_loess, se = TRUE) 

#======= INTERMEDIATE ========# 
# loess intermediate pulse
field_A_loess = loess(tn ~ doy, data = fieldA, span = 00.20) # 10% span
field_A_loess

# get smoothed output
field_A_smooth = predict(field_A_loess, se = TRUE)

# loess intermediate reference
field_D_loess = loess(tn ~ doy, data = fieldD, span = 00.20) # 10% span
field_D_loess

# get smoothed output
field_D_smooth = predict(field_D_loess, se = TRUE) 

#======= HIGH ========# 
# loess intermediate pulse
field_C_loess = loess(tn ~ doy, data = fieldC, span = 00.20) # 10% span
field_C_loess

# get smoothed output
field_C_smooth = predict(field_C_loess, se = TRUE)

# loess intermediate reference
field_E_loess = loess(tn ~ doy, data = fieldE, span = 00.20) # 10% span
field_E_loess

# get smoothed output
field_E_smooth = predict(field_E_loess, se = TRUE) 


# Set plot dimensions # 
plot(fieldF$tn, x=fieldF$doy, type = 'p', pch = 20, cex=1.5, xlab = '', yaxt = 'n',
     ylab = '', xlim=c(140, 245), ylim=c(0,1.2), col = ref_col, yaxt='n')
polygon(c(fieldF$doy, rev(fieldF$doy)), c(field_F_smooth$fit - field_F_smooth$se.fit, 
                               rev(field_F_smooth$fit + field_F_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(field_F_smooth$fit, x=fieldF$doy, col=ref_col, lwd = 2)

mtext(side = 2, line = 3.2, 
      expression('Total Nitrogen'), cex = 11/12)
mtext(side = 2, line = 1.6, 
      expression("  ("~"mg"~L^-1*")"), cex = 11/12)

par(new=T) # add new smooth to same plot 

plot(fieldB$tn, x=fieldB$doy, type = 'p', pch = 20, cex=1.5, xlab = '', yaxt = 'n',
     ylab = '', xlim=c(140, 245), ylim=c(0,1.2), col = low_col_F, col.axis = transparent)
polygon(c(fieldB$doy, rev(fieldB$doy)), c(field_B_smooth$fit - field_B_smooth$se.fit, 
                               rev(field_B_smooth$fit + field_B_smooth$se.fit)), 
        col = low_col_F, border = NA)
lines(field_B_smooth$fit, x=fieldB$doy, col=low_col_B, lwd = 2)

axis(side = 2, at = c(0,0.2, 0.4,0.6,0.8,1.0,1.2), cex.axis = 1) 
text(141, 1.2, 'D', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

plot(fieldD$tn, x=fieldD$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,1.2), col = ref_col, col.axis = transparent)
polygon(c(fieldD$doy, rev(fieldD$doy)), c(field_D_smooth$fit - field_D_smooth$se.fit, 
                               rev(field_D_smooth$fit + field_D_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(field_D_smooth$fit, x=fieldD$doy, col=ref_col, lwd = 2)


par(new=T) # add new smooth to same plot 

plot(fieldA$tn, x=fieldA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,1.2), col = int_col_D, yaxt= 'n', yaxt = 'n')
polygon(c(fieldA$doy, rev(fieldA$doy)), c(field_A_smooth$fit - field_A_smooth$se.fit, 
                               rev(field_A_smooth$fit + field_A_smooth$se.fit)), 
        col = int_col_D, border = NA)
lines(field_A_smooth$fit, x=fieldA$doy, col=int_col_D, lwd = 2)
text(141, 1.2, 'E', font = 2)
mtext('Day of Year, 2020', side = 1, cex = 11/12, line = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
axis(side = 2, at = c(0,0.2, 0.4,0.6,0.8,1.0,1.2), cex.axis = 1, labels = F)

plot(fieldE$tn, x=fieldE$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,1.2), col = ref_col, yaxt = 'n')
polygon(c(fieldE$doy, rev(fieldE$doy)), c(field_E_smooth$fit - field_E_smooth$se.fit, 
                               rev(field_E_smooth$fit + field_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(field_E_smooth$fit, x=fieldE$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(fieldC$tn, x=fieldC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,1.2), col = high_col_E, yaxt = 'n', col.axis = transparent)
polygon(c(fieldC$doy, rev(fieldC$doy)), c(field_C_smooth$fit - field_C_smooth$se.fit, 
                               rev(field_C_smooth$fit + field_C_smooth$se.fit)), 
        col = high_col_E, border = NA)
lines(field_C_smooth$fit, x=fieldC$doy, col=high_col_E, lwd = 2)
text(141, 1.2, 'F', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
axis(side = 2, at = c(0,0.2, 0.4,0.6,0.8,1.0,1.2), cex.axis = 1, labels = F) 

# Figure 3 with heat wave + derecho denoted # 

# Create figure with heat wave and derecho for reference #==================
# Window for checking plot 
windows(height = 8, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure3.pdf", 
# height = 8, 
#width = 6)

# Set dimensions for figure array # 
par(mfrow =c(4,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

col=rgb(255,48,48, max=255, alpha=75, names= 'firebrick1')

## Chlorophyll Final plot ========================
# Set plot dimensions # 
plot(algF$chla_10_30, x=algF$doy, type = 'p', pch = 20, cex=1.5, xlab = '', col.axis = transparent,
     ylab = '', xlim=c(140, 245), ylim=c(0, 35), col = ref_col, yaxt='n', cex.axis = 1.2 )
polygon(c(142:241, 241:142), c(alg_F_smooth$fit - alg_F_smooth$se.fit, 
                               rev(alg_F_smooth$fit + alg_F_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(alg_F_smooth$fit, x=algF$doy, col=ref_col, lwd = 2)
mtext(side = 3, line = 0.1, 'Low Complexity', cex = 11/12)

par(new=T) # add new smooth to same plot 

plot(algB$chla_10_30, x=algB$doy, type = 'p', pch = 20, cex=1.5, xlab = '', yaxt = 'n',
     ylab = '', xlim=c(140, 245), ylim=c(0, 35), col = low_col_F, col.axis = transparent)
polygon(c(142:241, 241:142), c(alg_B_smooth$fit - alg_B_smooth$se.fit, 
                               rev(alg_B_smooth$fit + alg_B_smooth$se.fit)), 
        col = low_col_F, border = NA)
lines(alg_B_smooth$fit, x=algB$doy, col=low_col_B, lwd = 2)
mtext(side = 2, line = 3.2, 
      expression('Chlorophyll-'~italic(a)), cex = 11/12)
mtext(side = 2, line = 2, 
      expression('Concentration' ~"("*mu*g~L^-1*")"), cex = 11/12)
axis(side = 2, at = c(0,5,10,15,20,25,30,35), cex.axis = 1) 
text(141, 35, 'A', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-2,190,50, col=col, border=NA)

plot(algD$chla_10_30, x=algD$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 35), col = ref_col, col.axis = transparent)
polygon(c(142:241, 241:142), c(alg_D_smooth$fit - alg_D_smooth$se.fit, 
                               rev(alg_D_smooth$fit + alg_D_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(alg_D_smooth$fit, x=algD$doy, col=ref_col, lwd = 2)
mtext(side = 3, line = 0.1, 'Intermediate', cex = 11/12)

par(new=T) # add new smooth to same plot 

plot(algA$chla_10_30, x=algA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 35), col = int_col_D, yaxt= 'n', col.axis = transparent)
polygon(c(142:241, 241:142), c(alg_A_smooth$fit - alg_A_smooth$se.fit, 
                               rev(alg_A_smooth$fit + alg_A_smooth$se.fit)), 
        col = int_col_D, border = NA)
lines(alg_A_smooth$fit, x=algA$doy, col=int_col_D, lwd = 2)
text(141, 35, 'B', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-2,190,50, col=col, border=NA)

plot(algE$chla_10_30, x=algE$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 35), col = ref_col, col.axis = transparent)
polygon(c(142:241, 241:142), c(alg_E_smooth$fit - alg_E_smooth$se.fit, 
                               rev(alg_E_smooth$fit + alg_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(alg_E_smooth$fit, x=algE$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(algC$chla_10_30, x=algC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 35), col = high_col_E, yaxt = 'n', col.axis = transparent)
polygon(c(142:241, 241:142), c(alg_C_smooth$fit - alg_C_smooth$se.fit, 
                               rev(alg_C_smooth$fit + alg_C_smooth$se.fit)), 
        col = high_col_E, border = NA)
lines(alg_C_smooth$fit, x=algC$doy, col=high_col_E, lwd = 2)
text(141, 35, 'C', font = 2)
mtext(side = 3, line = 0.1, 'High Complexity', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-2,190,50, col=col, border=NA)

## GPP Final plot ##===================

plot(gppF$GPP, x=gppF$doy, type = 'p', pch = 20, cex=1.5, xlab = '',
     ylab = '', xlim=c(140, 245), ylim=c(0, 20), col = ref_col, yaxt = 'n', col.axis = transparent)
polygon(c(gppF$doy, rev(gppF$doy)), c(gpp_F_smooth$fit - gpp_F_smooth$se.fit, 
                                      rev(gpp_F_smooth$fit + gpp_F_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(gpp_F_smooth$fit, x=gppF$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(gppB$GPP, x=gppB$doy, type = 'p', pch = 20, cex=1.5, xlab = '',
     ylab = '', xlim=c(140, 245), ylim=c(0, 20), col = low_col_F, col.axis = transparent)
polygon(c(gppB$doy, rev(gppB$doy)), c(gpp_B_smooth$fit - gpp_B_smooth$se.fit, 
                                      rev(gpp_B_smooth$fit + gpp_B_smooth$se.fit)), 
        col = low_col_F, border = NA)
lines(gpp_B_smooth$fit, x=gppB$doy, col=low_col_B, lwd = 2)
mtext(side = 2, line = 3.2, 
      expression('GPP'), cex = 11/12)
mtext(side = 2, line = 2, 
      expression('mg O'[2]~L^-1*~d^-1*")"), cex = 11/12)
axis(side = 2, at = c(0,5,10,15,20))
text(141, 20, 'D', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-2,190,50, col=col, border=NA)

plot(gppD$GPP, x=gppD$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 20), col = ref_col, col.axis = transparent)
polygon(c(gppD$doy, rev(gppD$doy)), c(gpp_D_smooth$fit - gpp_D_smooth$se.fit, 
                                      rev(gpp_D_smooth$fit + gpp_D_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(gpp_D_smooth$fit, x=gppD$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(gppA$GPP, x=gppA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 20), col = int_col_D, yaxt= 'n', col.axis = transparent)
polygon(c(gppA$doy, rev(gppA$doy)), c(gpp_A_smooth$fit - gpp_A_smooth$se.fit, 
                                      rev(gpp_A_smooth$fit + gpp_A_smooth$se.fit)), 
        col = int_col_D, border = NA)
lines(gpp_A_smooth$fit, x=gppA$doy, col=int_col_D, lwd = 2)
text(141, 20, 'E', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-2,190,50, col=col, border=NA)

plot(gppE$GPP, x=gppE$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 20), col = ref_col, col.axis = transparent)
polygon(c(gppE$doy, rev(gppE$doy)), c(gpp_E_smooth$fit - gpp_E_smooth$se.fit, 
                                      rev(gpp_E_smooth$fit + gpp_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(gpp_E_smooth$fit, x=gppE$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(gppC$GPP, x=gppC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 20), col = high_col_E, yaxt = 'n', col.axis = transparent)
polygon(c(gppC$doy, rev(gppC$doy)), c(gpp_C_smooth$fit - gpp_C_smooth$se.fit, 
                                      rev(gpp_C_smooth$fit + gpp_C_smooth$se.fit)), 
        col = high_col_E, border = NA)
lines(gpp_C_smooth$fit, x=gppC$doy, col=high_col_E, lwd = 2)
text(141, 20, 'F', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-2,190,50, col=col, border=NA)

## R Final plot ##==========================

plot(respF$R, x=respF$doy, type = 'p', pch = 20, cex=1.5, xlab = '', yaxt = 'n', col.axis=transparent,
     ylab = '', xlim=c(140, 245), ylim=c(0,20), col = ref_col)
polygon(c(respF$doy, rev(respF$doy)), c(resp_F_smooth$fit - resp_F_smooth$se.fit, 
                                        rev(resp_F_smooth$fit + resp_F_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(resp_F_smooth$fit, x=respF$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(respB$R, x=respB$doy, type = 'p', pch = 20, cex=1.5, xlab = '', yaxt='n', col.axis=transparent,
     ylab = '', xlim=c(140, 245), ylim=c(0, 20), col = low_col_F)
polygon(c(respB$doy, rev(respB$doy)), c(resp_B_smooth$fit - resp_B_smooth$se.fit, 
                                        rev(resp_B_smooth$fit + resp_B_smooth$se.fit)), 
        col = low_col_F, border = NA)
lines(resp_B_smooth$fit, x=respB$doy, col=low_col_B, lwd = 2)
mtext(side = 2, line = 3.2, 
      expression('|R|'), cex = 11/12)
mtext(side = 2, line = 1.8, 
      expression('mg O'[2]~L^-1*~d^-1*")"), cex = 11/12)
axis(side =2, at = c(0, 5, 10, 15, 20))
text(141, 20, 'G', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-2,190,50, col=col, border=NA)

plot(respD$R, x=respD$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0, 20), col = ref_col, col.axis = transparent)
polygon(c(respD$doy, rev(respD$doy)), c(resp_D_smooth$fit - resp_D_smooth$se.fit, 
                                        rev(resp_D_smooth$fit + resp_D_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(resp_D_smooth$fit, x=respD$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(respA$R, x=respA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '', col.axis = transparent,
     xlim=c(140, 245), ylim=c(0, 20), col = int_col_D, yaxt= 'n')
polygon(c(respA$doy, rev(respA$doy)), c(resp_A_smooth$fit - resp_A_smooth$se.fit, 
                                        rev(resp_A_smooth$fit + resp_A_smooth$se.fit)), 
        col = int_col_D, border = NA)
lines(resp_A_smooth$fit, x=respA$doy, col=int_col_D, lwd = 2)
text(141,20, 'H', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-2,190,50, col=col, border=NA)

plot(respE$R, x=respE$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '', yaxt = 'n',
     xlim=c(140, 245), ylim=c(0, 20), col = ref_col, col.axis = transparent)
polygon(c(respE$doy, rev(respE$doy)), c(resp_E_smooth$fit - resp_E_smooth$se.fit, 
                                        rev(resp_E_smooth$fit + resp_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(resp_E_smooth$fit, x=respE$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(respC$R, x=respC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '', col.axis = transparent,
     xlim=c(140, 245), ylim=c(0, 20), col = high_col_E, yaxt = 'n')
polygon(c(respC$doy, rev(respC$doy)), c(resp_C_smooth$fit - resp_C_smooth$se.fit, 
                                        rev(resp_C_smooth$fit + resp_C_smooth$se.fit)), 
        col = high_col_E, border = NA)
lines(resp_C_smooth$fit, x=respC$doy, col=high_col_E, lwd = 2)
text(141,20, 'I', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-2,190,50, col=col, border=NA)

## NEP Final Plot ##==============================
plot(netpF$NEP, x=netpF$doy, type = 'p', pch = 20, cex=1.5, xlab = '', yaxt = 'n',
     ylab = '', xlim=c(140, 245), ylim=c(-8,8), col = ref_col)
polygon(c(netpF$doy, rev(netpF$doy)), c(netp_F_smooth$fit - netp_F_smooth$se.fit, 
                                        rev(netp_F_smooth$fit + netp_F_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(netp_F_smooth$fit, x=netpF$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(netpB$NEP, x=netpB$doy, type = 'p', pch = 20, cex=1.5, xlab = '', col.axis = transparent,
     ylab = '', xlim=c(140, 245), ylim=c(-8,8), col = low_col_F)
polygon(c(netpB$doy, rev(netpB$doy)), c(netp_B_smooth$fit - netp_B_smooth$se.fit, 
                                        rev(netp_B_smooth$fit + netp_B_smooth$se.fit)), 
        col = low_col_F, border = NA)
lines(netp_B_smooth$fit, x=netpB$doy, col=low_col_B, lwd = 2)
mtext(side = 2, line = 3.2, 
      expression('NEP'), cex = 11/12)
mtext(side = 2, line = 1.8, 
      expression('mg O'[2]~L^-1*~d^-1*")"), cex = 11/12)
axis(side = 2, at=c(-5, 0, 5))
text(141, 8, 'J', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
lines(c(100,300), c(0,0), lty = 1)
rect(185,-50,190,50, col=col, border=NA)

plot(netpD$NEP, x=netpD$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(-8,8), col = ref_col, col.axis = transparent)
polygon(c(netpD$doy, rev(netpD$doy)), c(netp_D_smooth$fit - netp_D_smooth$se.fit, 
                                        rev(netp_D_smooth$fit + netp_D_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(netp_D_smooth$fit, x=netpD$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(netpA$NEP, x=netpA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(-8,8), col = int_col_D, yaxt= 'n')
polygon(c(netpA$doy, rev(netpA$doy)), c(netp_A_smooth$fit - netp_A_smooth$se.fit, 
                                        rev(netp_A_smooth$fit + netp_A_smooth$se.fit)), 
        col = int_col_D, border = NA)
lines(netp_A_smooth$fit, x=netpA$doy, col=int_col_D, lwd = 2)
mtext(side = 1, line = 2.5, "Day of Year, 2020", cex = 11/12) 
text(141, 8, 'K', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
lines(c(100,300), c(0,0), lty = 1)
rect(185,-50,190,50, col=col, border=NA)

plot(netpE$NEP, x=netpE$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(-8,8), col = ref_col, col.axis = transparent)
polygon(c(netpE$doy, rev(netpE$doy)), c(netp_E_smooth$fit - netp_E_smooth$se.fit, 
                                        rev(netp_E_smooth$fit + netp_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(netp_E_smooth$fit, x=netpE$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(netpC$NEP, x=netpC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(-8,8), col = high_col_E, yaxt = 'n')
polygon(c(netpC$doy, rev(netpC$doy)), c(netp_C_smooth$fit - netp_C_smooth$se.fit, 
                                        rev(netp_C_smooth$fit + netp_C_smooth$se.fit)), 
        col = high_col_E, border = NA)
lines(netp_C_smooth$fit, x=netpC$doy, col=high_col_E, lwd = 2)
text(141, 8, 'L', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-100,700), lty = 3)
lines(c(211,211), c(-100,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
lines(c(100,300), c(0,0), lty = 1)
rect(185,-50,190,50, col=col, border=NA)

# Create the pdf of the plot 
#dev.off()

## RDA Sensitivity Analyses ##=========================

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
## Disturbhf - Walter et al. 2022 L&O  
if (!require(disturbhf)) install.packages('remotes')
remotes::install_github('jonathan-walter/disturbhf')
library(disturbhf)

## 5 days ##==========================

### Chlorophyll-a #===================
# data # 
hort_field

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy_low = hort_field %>%
  select(pond_id, doy, chla_10_30) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = chla_10_30)
testy_low

refy_low = hort_field %>%
  select(pond_id, doy, chla_10_30) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=chla_10_30)
refy_low

## Intermediate Coupling ## 
testy_int = hort_field %>%
  select(pond_id, doy, chla_10_30) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = chla_10_30)
testy_int

refy_int = hort_field %>%
  select(pond_id, doy, chla_10_30) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = chla_10_30)
refy_int

## High Coupling ## 
testy_high = hort_field %>%
  select(pond_id, doy, chla_10_30) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = chla_10_30)
testy_high

refy_high = hort_field %>%
  select(pond_id, doy, chla_10_30) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=chla_10_30)
refy_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low_5day = mwdistdiffz(testy = testy_low, refy = refy_low, 
                           wwidth = 5, 
                           ddiff_method = 'integral')
rda_int_5day = mwdistdiffz(testy = testy_int, refy = refy_int, 
                           wwidth = 5, 
                           ddiff_method = 'integral')
rda_high_5day = mwdistdiffz(testy = testy_high, refy = refy_high, 
                            wwidth = 5, 
                            ddiff_method = 'integral')

### Gross Primary Production #===================
# data - need to include NAs for rollign window # 
metab_gross = read_csv('daily-metabolism_data_robertcorrected.csv') 
metab_gross$doy # Look for missing day of years # 
# No missing days 

# Replace values with flag =1 with NAs # 
metab_gross$GPP[metab_gross$flag == 1] <- NA
metab_gross$R[metab_gross$flag == 1] <- NA
metab_gross$NEP[metab_gross$flag == 1] <- NA
metab_gross # False data now removed and replaced with NAs 

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy2_low = metab %>%
  select(pond_id, doy, GPP) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = GPP)
testy2_low

refy2_low = metab %>%
  select(pond_id, doy, GPP) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=GPP)
refy2_low

## Intermediate Coupling ## 
testy2_int = metab %>%
  select(pond_id, doy, GPP) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = GPP)
testy2_int

refy2_int = metab %>%
  select(pond_id, doy, GPP) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = GPP)
refy2_int

## High Coupling ## 
testy2_high = metab %>%
  select(pond_id, doy, GPP) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = GPP)
testy2_high

refy2_high = metab %>%
  select(pond_id, doy, GPP) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=GPP)
refy2_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low2 = mwdistdiffz(testy = testy2_low, refy = refy2_low, 
                       wwidth = 5, 
                       ddiff_method = 'integral')
rda_int2 = mwdistdiffz(testy = testy2_int, refy = refy2_int, 
                       wwidth = 5, 
                       ddiff_method = 'integral')
rda_high2 = mwdistdiffz(testy = testy2_high, refy = refy2_high, 
                        wwidth = 5, 
                        ddiff_method = 'integral')

### Respiration #===================
# data - need to include NAs for rollign window # 
metab_gross = read_csv('daily-metabolism_data_robertcorrected.csv') 
metab_gross$doy # Look for missing day of years # 
# No missing days 

# Replace values with flag =1 with NAs # 
metab_gross$GPP[metab_gross$flag == 1] <- NA
metab_gross$R[metab_gross$flag == 1] <- NA
metab_gross$NEP[metab_gross$flag == 1] <- NA
metab_gross # False data now removed and replaced with NAs 

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy3_low = metab %>%
  select(pond_id, doy, R) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = R)
testy3_low

refy3_low = metab %>%
  select(pond_id, doy, R) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=R)
refy3_low

## Intermediate Coupling ## 
testy3_int = metab %>%
  select(pond_id, doy, R) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = R)
testy3_int

refy3_int = metab %>%
  select(pond_id, doy, R) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = R)
refy3_int

## High Coupling ## 
testy3_high = metab %>%
  select(pond_id, doy, R) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = R)
testy3_high

refy3_high = metab %>%
  select(pond_id, doy, R) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=R)
refy3_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low3 = mwdistdiffz(testy = testy3_low, refy = refy3_low, 
                       wwidth = 5, 
                       ddiff_method = 'integral')
rda_int3 = mwdistdiffz(testy = testy3_int, refy = refy3_int, 
                       wwidth = 5, 
                       ddiff_method = 'integral')
rda_high3 = mwdistdiffz(testy = testy3_high, refy = refy3_high, 
                        wwidth = 5, 
                        ddiff_method = 'integral')

### Net Ecosystem Production #===================
# data - need to include NAs for rollign window # 
metab_gross = read_csv('daily-metabolism_data_robertcorrected.csv') 
metab_gross$doy # Look for missing day of years # 
# No missing days 

# Replace values with flag =1 with NAs # 
metab_gross$GPP[metab_gross$flag == 1] <- NA
metab_gross$R[metab_gross$flag == 1] <- NA
metab_gross$NEP[metab_gross$flag == 1] <- NA
metab_gross # False data now removed and replaced with NAs 

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy4_low = metab %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = NEP)
testy4_low

refy4_low = metab %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=NEP)
refy4_low

## Intermediate Coupling ## 
testy4_int = metab %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = NEP)
testy4_int

refy4_int = metab %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = NEP)
refy4_int

## High Coupling ## 
testy4_high = metab %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = NEP)
testy4_high

refy4_high = metab %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=NEP)
refy4_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low4 = mwdistdiffz(testy = testy4_low, refy = refy4_low, 
                       wwidth = 5, 
                       ddiff_method = 'integral')
rda_int4 = mwdistdiffz(testy = testy4_int, refy = refy4_int, 
                       wwidth = 5, 
                       ddiff_method = 'integral')
rda_high4 = mwdistdiffz(testy = testy4_high, refy = refy4_high, 
                        wwidth = 5, 
                        ddiff_method = 'integral')

#### Plot chl-a and GPP Response Detection Algorithm #=================

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

high_col_C = rgb(75, 31, 110, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(75, 31, 110, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(75, 31, 110, max = 255, alpha = 255) #Pond C, Pond E

col=rgb(255,48,48, max=255, alpha=75, names= 'firebrick1') # Extended heat period 

## ============ Plot Margins ================= ##
# Window for checking plot 
windows(height = 6, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure4.pdf", 
#   height = 4, 
#    width = 6)

# Set dimensions for figure array # 
par(mfrow =c(4,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0)) 

#### Chlorophyll-a RDA plot ##==========================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', col.axis = transparent, yaxt = 'n', 
     lwd=3, col=low_col, data=rda_low_5day)
axis(side = 2, at=c(-2, 0, 2, 4, 6))
mtext(side = 2, line = 3.2, 
      expression('Chlorophyll-'~italic(a)), cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)
text(141, 5.8, 'A', font = 2)
mtext(side = 3, line = 0.1, 'Low Complexity', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', col.axis = transparent,
     lwd=3, col=int_col, data=rda_int_5day)
mtext(side = 3, line = 0.1, 'Intermediate', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'B', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', col.axis = transparent, 
     lwd=3, col=high_col, data=rda_high_5day)
mtext(side = 3, line = 0.1, 'High Complexity', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'C', font = 2)

# Add in Labels 
text(155, 2.5, 'Response')
text(230, 0.9, 'Recovery')

#### GPP RDA plot ##============================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n',
     lwd=3, col=low_col, data=rda_low2, col.axis = transparent)
mtext(side = 2, line = 3.2, 'GPP', cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
axis(side = 2, at=c(-2, 0, 2, 4, 6))

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'D', font = 2)

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n', 
     lwd=3, col=int_col, data=rda_int2, col.axis = transparent)
axis(side = 2, at = c(-2, 0, 2, 4, 6), labels = F)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'E', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n', 
     lwd=3, col=high_col, data=rda_high2, col.axis = transparent)
axis(side = 2, at=c(-2, 0,2,4,6), labels = F)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'F', font = 2)

#### Respiration RDA plot ##==========================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n', col.axis = transparent, 
     lwd=3, col=low_col, data=rda_low3)
axis(side = 2, at=c(-2, 0, 2, 4, 6))
mtext(side = 2, line = 3.2, 
      expression('R'), cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)
text(141, 5.8, 'G', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), col.axis = transparent,
     ylab = '', xlab = '', 
     lwd=3, col=int_col, data=rda_int3)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'H', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), col.axis = transparent, 
     ylab = '', xlab = '', 
     lwd=3, col=high_col, data=rda_high3)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'I', font = 2)

#### Net Ecosystem Production RDA plot ##==========================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n', 
     lwd=3, col=low_col, data=rda_low4)
axis(side = 2, at=c(-2, 0, 2, 4, 6))
mtext(side = 2, line = 3.2, 
      expression('NEP'), cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)
text(141, 5.8, 'L', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', 
     lwd=3, col=int_col, data=rda_int4)
mtext(side = 1, line = 3.5, 'Last Day of Year in\n5-day rolling window', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'K', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', 
     lwd=3, col=high_col, data=rda_high4)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'L', font = 2)
# Create plot in specified file path # 
#dev.off()

## 10 days ##==========================

### Chlorophyll-a #===================
# data # 
hort_field

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy_low = hort_field %>%
  select(pond_id, doy, chla_10_30) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = chla_10_30)
testy_low

refy_low = hort_field %>%
  select(pond_id, doy, chla_10_30) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=chla_10_30)
refy_low

## Intermediate Coupling ## 
testy_int = hort_field %>%
  select(pond_id, doy, chla_10_30) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = chla_10_30)
testy_int

refy_int = hort_field %>%
  select(pond_id, doy, chla_10_30) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = chla_10_30)
refy_int

## High Coupling ## 
testy_high = hort_field %>%
  select(pond_id, doy, chla_10_30) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = chla_10_30)
testy_high

refy_high = hort_field %>%
  select(pond_id, doy, chla_10_30) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=chla_10_30)
refy_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 10 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low_10day = mwdistdiffz(testy = testy_low, refy = refy_low, 
                            wwidth = 10, 
                            ddiff_method = 'integral')
rda_int_10day = mwdistdiffz(testy = testy_int, refy = refy_int, 
                            wwidth = 10, 
                            ddiff_method = 'integral')
rda_high_10day = mwdistdiffz(testy = testy_high, refy = refy_high, 
                             wwidth = 10, 
                             ddiff_method = 'integral')

### Gross Primary Production #===================
# data - need to include NAs for rollign window # 
metab_gross = read_csv('daily-metabolism_data_robertcorrected.csv') 
metab_gross$doy # Look for missing day of years # 
# No missing days 

# Replace values with flag =1 with NAs # 
metab_gross$GPP[metab_gross$flag == 1] <- NA
metab_gross$R[metab_gross$flag == 1] <- NA
metab_gross$NEP[metab_gross$flag == 1] <- NA
metab_gross # False data now removed and replaced with NAs 

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy2_low = metab %>%
  select(pond_id, doy, GPP) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = GPP)
testy2_low

refy2_low = metab %>%
  select(pond_id, doy, GPP) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=GPP)
refy2_low

## Intermediate Coupling ## 
testy2_int = metab %>%
  select(pond_id, doy, GPP) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = GPP)
testy2_int

refy2_int = metab %>%
  select(pond_id, doy, GPP) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = GPP)
refy2_int

## High Coupling ## 
testy2_high = metab %>%
  select(pond_id, doy, GPP) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = GPP)
testy2_high

refy2_high = metab %>%
  select(pond_id, doy, GPP) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=GPP)
refy2_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low2 = mwdistdiffz(testy = testy2_low, refy = refy2_low, 
                       wwidth = 10, 
                       ddiff_method = 'integral')
rda_int2 = mwdistdiffz(testy = testy2_int, refy = refy2_int, 
                       wwidth = 10, 
                       ddiff_method = 'integral')
rda_high2 = mwdistdiffz(testy = testy2_high, refy = refy2_high, 
                        wwidth = 10, 
                        ddiff_method = 'integral')

### Respiration #===================
# data - need to include NAs for rollign window # 
metab_gross = read_csv('daily-metabolism_data_robertcorrected.csv') 
metab_gross$doy # Look for missing day of years # 
# No missing days 

# Replace values with flag =1 with NAs # 
metab_gross$GPP[metab_gross$flag == 1] <- NA
metab_gross$R[metab_gross$flag == 1] <- NA
metab_gross$NEP[metab_gross$flag == 1] <- NA
metab_gross # False data now removed and replaced with NAs 

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy3_low = metab %>%
  select(pond_id, doy, R) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = R)
testy3_low

refy3_low = metab %>%
  select(pond_id, doy, R) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=R)
refy3_low

## Intermediate Coupling ## 
testy3_int = metab %>%
  select(pond_id, doy, R) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = R)
testy3_int

refy3_int = metab %>%
  select(pond_id, doy, R) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = R)
refy3_int

## High Coupling ## 
testy3_high = metab %>%
  select(pond_id, doy, R) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = R)
testy3_high

refy3_high = metab %>%
  select(pond_id, doy, R) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=R)
refy3_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low3 = mwdistdiffz(testy = testy3_low, refy = refy3_low, 
                       wwidth = 10, 
                       ddiff_method = 'integral')
rda_int3 = mwdistdiffz(testy = testy3_int, refy = refy3_int, 
                       wwidth = 10, 
                       ddiff_method = 'integral')
rda_high3 = mwdistdiffz(testy = testy3_high, refy = refy3_high, 
                        wwidth = 10, 
                        ddiff_method = 'integral')

### Net Ecosystem Production #===================
# data - need to include NAs for rollign window # 
metab_gross = read_csv('daily-metabolism_data_robertcorrected.csv') 
metab_gross$doy # Look for missing day of years # 
# No missing days 

# Replace values with flag =1 with NAs # 
metab_gross$GPP[metab_gross$flag == 1] <- NA
metab_gross$R[metab_gross$flag == 1] <- NA
metab_gross$NEP[metab_gross$flag == 1] <- NA
metab_gross # False data now removed and replaced with NAs 

# Run Response Detection Analysis # 
# Create disturbed and reference time series # 
## Low Coupling ## 
testy4_low = metab %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'B') %>%
  rename(tt = doy, 
         yy = NEP)
testy4_low

refy4_low = metab %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'F') %>%
  rename(tt = doy, 
         yy=NEP)
refy4_low

## Intermediate Coupling ## 
testy4_int = metab %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'A') %>%
  rename(tt = doy, 
         yy = NEP)
testy4_int

refy4_int = metab %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'D') %>%
  rename(tt = doy, 
         yy = NEP)
refy4_int

## High Coupling ## 
testy4_high = metab %>%
  select(pond_id, doy, NEP) %>% 
  filter(pond_id == 'C') %>%
  rename(tt = doy, 
         yy = NEP)
testy4_high

refy4_high = metab %>%
  select(pond_id, doy, NEP) %>%
  filter(pond_id == 'E') %>%
  rename(tt = doy, 
         yy=NEP)
refy4_high

# Run Response Detection Analysis # 
## Rolling Window for disturbed time-series = 7 days 
## Using entire reference time series as the reference (no adaptive window)
## Calculating differeing by taking the integral of the absolute difference between the two time series
## Thresholds = response (2); recovery (0.5) as recommended in package 

rda_low4 = mwdistdiffz(testy = testy4_low, refy = refy4_low, 
                       wwidth = 10, 
                       ddiff_method = 'integral')
rda_int4 = mwdistdiffz(testy = testy4_int, refy = refy4_int, 
                       wwidth = 10, 
                       ddiff_method = 'integral')
rda_high4 = mwdistdiffz(testy = testy4_high, refy = refy4_high, 
                        wwidth = 10, 
                        ddiff_method = 'integral')

#### Plot chl-a and GPP Response Detection Algorithm #=================


## ============ Plot Margins ================= ##
# Window for checking plot 
windows(height = 6, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure4.pdf", 
#   height = 4, 
#    width = 6)

# Set dimensions for figure array # 
par(mfrow =c(4,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0)) 

#### Chlorophyll-a RDA plot ##==========================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', col.axis = transparent, yaxt = 'n', 
     lwd=3, col=low_col, data=rda_low_10day)
axis(side = 2, at=c(-2, 0, 2, 4, 6))
mtext(side = 2, line = 3.2, 
      expression('Chlorophyll-'~italic(a)), cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)
text(141, 5.8, 'A', font = 2)
mtext(side = 3, line = 0.1, 'Low Complexity', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', col.axis = transparent,
     lwd=3, col=int_col, data=rda_int_10day)
mtext(side = 3, line = 0.1, 'Intermediate', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'B', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', col.axis = transparent, 
     lwd=3, col=high_col, data=rda_high_10day)
mtext(side = 3, line = 0.1, 'High Complexity', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'C', font = 2)

# Add in Labels 
text(155, 2.5, 'Response')
text(230, 0.9, 'Recovery')

#### GPP RDA plot ##============================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n',
     lwd=3, col=low_col, data=rda_low2, col.axis = transparent)
mtext(side = 2, line = 3.2, 'GPP', cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
axis(side = 2, at=c(-2, 0, 2, 4, 6))

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'D', font = 2)

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n', 
     lwd=3, col=int_col, data=rda_int2, col.axis = transparent)
axis(side = 2, at = c(-2, 0, 2, 4, 6), labels = F)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'E', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n', 
     lwd=3, col=high_col, data=rda_high2, col.axis = transparent)
axis(side = 2, at=c(-2, 0,2,4,6), labels = F)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'F', font = 2)

#### Respiration RDA plot ##==========================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n', col.axis = transparent, 
     lwd=3, col=low_col, data=rda_low3)
axis(side = 2, at=c(-2, 0, 2, 4, 6))
mtext(side = 2, line = 3.2, 
      expression('R'), cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)
text(141, 5.8, 'G', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), col.axis = transparent,
     ylab = '', xlab = '', 
     lwd=3, col=int_col, data=rda_int3)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'H', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), col.axis = transparent, 
     ylab = '', xlab = '', 
     lwd=3, col=high_col, data=rda_high3)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'I', font = 2)

#### Net Ecosystem Production RDA plot ##==========================
## Low Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6), 
     ylab = '', xlab = '', yaxt = 'n', 
     lwd=3, col=low_col, data=rda_low4)
axis(side = 2, at=c(-2, 0, 2, 4, 6))
mtext(side = 2, line = 3.2, 
      expression('NEP'), cex = 11/12)
mtext(side = 2, line = 2, 'Z-scores', cex = 11/12)
text(141, 5.8, 'L', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 

## Intermediate Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', 
     lwd=3, col=int_col, data=rda_int4)
mtext(side = 1, line = 3.5, 'Last Day of Year in\n10-day rolling window', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5)) 
text(141, 5.8, 'K', font = 2)

## High Coupling ## 
plot(zz~wright, type='l', xlim=c(140,245), ylim=c(-2,6),
     ylab = '', xlab = '', 
     lwd=3, col=high_col, data=rda_high4)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)

#Add in the response and recovery thresholds 
abline(h=2, lwd=2) 
lines(x =c(176, 250), y = c(0.5, 0.5))
text(141, 5.8, 'L', font = 2)
# Create plot in specified file path # 
#dev.off()

# Zooplankton and Macroinvertebrate Compositional Analysis # ==========================
hort_zoop
hort_mivdensity
library(vegan)
library(tidyverse)

# Zoop nMDS Pre-nutrient Pulse; post nutrient pulse; total 
hort_zoop_wide = hort_zoop %>% 
  pivot_wider(id_cols = c(pond_id, doy), 
              names_from = taxon, 
              values_from = biomass)
hort_zoop_wide

# pre-pulse (pre DOY 176) 
prep_hzp = hort_zoop_wide %>% 
  filter(doy <= 175) # prior to first nutrient pulse

# post-2nd pulse (post DOY 176)
postp_hzp = hort_zoop_wide %>% 
  filter(doy >= 211)

# nMDS of zoops pre pulse # ===================
prep_hzp = prep_hzp %>% 
  mutate(group = case_when(pond_id == "B" | pond_id == "F" ~ 'low',
                           pond_id == "A" | pond_id == "D" ~ 'intermediate',
                           pond_id == "C" | pond_id == "E" ~ 'high')) %>% 
  relocate(pond_id, doy, group)
prep_hzp                     

prep_hzp.matp = prep_hzp[,4:length(prep_hzp)]         
prep_hzp.matp

prep_hzp.matp <- prep_hzp.matp[, colSums(prep_hzp.matp) != 0] # remove columns that equal zero (species does not occur over time series)

prep_hzp.m <- as.matrix(prep_hzp.matp) # make matrix 
set.seed(55)

# downweight rare species # 
prep_hzp.m.hell <- decostand(prep_hzp.m, method = "hellinger")
head(prep_hzp.m.hell)

nmds = metaMDS(prep_hzp.m.hell, distance = 'bray')
plot(nmds)

data.scores = as.data.frame(scores(nmds)$sites)
spp.scores = rownames_to_column(as.data.frame(scores(nmds)$species), var = "species")
spp.scores

# add explanatory columns # 
data.scores$pond_id = prep_hzp$pond_id
data.scores$group = factor(prep_hzp$group, levels = c("low", "intermediate", "high"))
data.scores

library(ggplot2)

low_col_B = rgb(74, 166, 81, max = 255, alpha = 180) #Pond B, Pond F
low_col_F = rgb(74, 166, 81, max = 255, alpha = 100) #Pond B, Pond F
low_col = rgb(74, 166, 81, max = 255, alpha = 255) #Pond B, Pond F
ref_col = rgb(155, 155, 155, max=255, alpha = 100) # Reference
black_col = rgb(0,0,0, max=255, alpha = 100) # Black
transparent = rgb(255,255,255, max=255, alpha = 0)

int_col_A = rgb(44, 127, 184, max = 255, alpha = 180) #Pond A, pond D
int_col_D = rgb(44, 127, 184, max = 255, alpha = 100) #Pond A, pond D
int_col = rgb(44, 127, 184, max = 255, alpha = 255) #Pond A, pond D

high_col_C = rgb(75, 31, 110, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(75, 31, 110, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(75, 31, 110, max = 255, alpha = 255) #Pond C, Pond E

# Select a subset of species to plot (e.g., top 5 based on some criteria, or specific ones of interest)
hort_zoop_filt <- hort_zoop %>% 
  filter(doy <= 175) %>% 
  group_by(taxon) %>% 
  summarize(mean = mean(biomass, na.rm = T)) %>% 
  arrange(desc(mean))
hort_zoop_filt # Top 5 highest average biomass: Calanoida, Daphnia, Cyclopoida, Nauplii, Chydorus 

selected_species <- spp.scores[spp.scores$species %in% c("Calanoida", "Daphnia", "Cyclopoida", "Nauplii", "Chydorus"), ]

# Overlay the species points and labels onto the existing plot

xx2 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) +
  # Add species points and text, combining into a single legend
  geom_text(data = selected_species, aes(x = NMDS1, y = NMDS2, label = species, color = "Species Scores"),
            color = "black", fontface = "bold", nudge_y = 0.08, nudge_x = -0.04) +
  geom_point(data = selected_species, aes(x = NMDS1, y = NMDS2, color = "Species Scores"), size = 4) +
  
  # Add group points with a different legend label
  geom_point(aes(shape = group), size = 2) +
  
  # Ellipses with group fill
  stat_ellipse(aes(group = group, fill = group), alpha = 0.3, geom = "polygon", size = 2) +
  
  # Custom theme
  theme(axis.text.y = element_text(colour = 'black', size = 12, face = 'bold'), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "none", 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key = element_blank()) + 
  ylim(-1.5, 1.8) +
  xlim(-1.5, 1.8) +
  
  # Custom labels for axes and legend
  labs(x = "NMDS1", y = "NMDS2", color = "Data Type", shape = "Group", fill = "Group") +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.2,
           size = 6, fontface = "bold") + # Customizing position and style
  
  # Manual colors for fill (groups) and points/labels
  scale_fill_manual(values = c(low_col, int_col, high_col)) +
  scale_color_manual(values = c("Species Scores" = "firebrick"))  # Customize legend for species
xx2


# nMDS of zoops post second pulse # ==============================
postp_hzp = postp_hzp %>% 
  mutate(group = case_when(pond_id == "B" | pond_id == "F" ~ 'low',
                           pond_id == "A" | pond_id == "D" ~ 'intermediate',
                           pond_id == "C" | pond_id == "E" ~ 'high')) %>% 
  relocate(pond_id, doy, group)
postp_hzp                     

postp_hzp.matp = postp_hzp[,4:length(postp_hzp)]         
postp_hzp.matp

postp_hzp.matp <- postp_hzp.matp[, colSums(postp_hzp.matp) != 0] # remove columns that equal zero (species does not occur over time series)

postp_hzp.m <- as.matrix(postp_hzp.matp) # make matrix 
set.seed(55)

# downweight rare species # 
postp_hzp.m.hell <- decostand(postp_hzp.m, method = "hellinger")
head(postp_hzp.m.hell)

nmds = metaMDS(postp_hzp.m.hell, distance = 'bray')
plot(nmds)

data.scores = as.data.frame(scores(nmds)$sites)
spp.scores = rownames_to_column(as.data.frame(scores(nmds)$species), var = "species")
spp.scores

# add explanatory columns # 
data.scores$pond_id = postp_hzp$pond_id
data.scores$group = factor(postp_hzp$group, levels = c("low", "intermediate", "high"))
data.scores

library(ggplot2)


# Select a subset of species to plot (e.g., top 5 based on some criteria, or specific ones of interest)
hort_zoop_filt <- hort_zoop %>% 
  filter(doy >= 211) %>% 
  group_by(taxon) %>% 
  summarize(mean = mean(biomass, na.rm = T)) %>% 
  arrange(desc(mean))
hort_zoop_filt # Top 5 highest average biomass: Calanoida, Daphnia, Cyclopoida, Nauplii, Chydorus 

selected_species <- spp.scores[spp.scores$species %in% c("Polyarthra", "Nauplii", "Cyclopoida", "Keratella.cochlearis", "Platyias"), ]

# Overlay the species points and labels onto the existing plot

xx3 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) +
  # Add species points and text, combining into a single legend
  geom_text(data = selected_species, aes(x = NMDS1, y = NMDS2, label = species, color = "Species Scores"),
            color = "black", fontface = "bold", nudge_y = 0.08, nudge_x = -0.04) +
  geom_point(data = selected_species, aes(x = NMDS1, y = NMDS2, color = "Species Scores"), size = 4) +
  
  # Add group points with a different legend label
  geom_point(aes(shape = group), size = 2) +
  
  # Ellipses with group fill
  stat_ellipse(aes(group = group, fill = group), alpha = 0.3, geom = "polygon", size = 2) +
  
  # Custom theme
  theme(axis.text.y = element_text(colour = 'black', size = 12, face = 'bold'), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key = element_blank()) + 
  ylim(-1.5, 1.8) +
  xlim(-1.5, 1.8) +
  
  # Custom labels for axes and legend
  labs(x = "NMDS1", y = "NMDS2", color = "Data Type", shape = "Group", fill = "Group") + 
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.2,
           size = 6, fontface = "bold") + # Customizing position and style
  
  # Manual colors for fill (groups) and points/labels
  scale_fill_manual(values = c(low_col, int_col, high_col)) +
  scale_color_manual(values = c("Species Scores" = "firebrick"))  # Customize legend for species
xx3


windows(height = 4, width = 6)
xx2

windows(height = 4, width = 8)
xx3

# Initial nutrient means # =====================
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

init_nut <- hort_field %>% 
  select(pond_id, doy, tp, srp, tn, nox, nhx) %>% 
  mutate(code = case_when(pond_id == "B" ~ "low-pulsed", 
                          pond_id == "F" ~ "low-reference",
                          pond_id == "A" ~ "int-pulsed",
                          pond_id == "D" ~ "int-reference",
                          pond_id == "C" ~ "high-pulsed",
                          pond_id == "E" ~ "high-reference")) %>% 
  filter(doy <= 175) %>% 
  group_by(code) %>% 
  summarize(mean.tp = mean(tp, na.rm = T), 
            sd.tp = sd(tp, na.rm = T), 
            
            mean.tn = mean(tn, na.rm = T),
            sd.tn = sd(tn, na.rm = T), 
            
            mean.srp = mean(srp, na.rm = T), 
            sd.srp = sd(srp, na.rm = T), 
            
            mean.nox = mean(nox, na.rm = T), 
            sd.nox = sd(nox, na.rm = T), 
            
            mean.nh4 = mean(nhx, na.rm = T), 
            sd.nh4 = sd(nhx, na.rm = T))
init_nut


# YSI Sond Profiles - surface v. bottom temperature # ======================
ysi_tb <- hort_ysi %>% 
  select(doy, pond_id, temp, depth_m) %>% 
  group_by(doy, pond_id) %>% 
  mutate(pos = case_when(depth_m == max(depth_m, na.rm = T) ~ "bottom", 
                         depth_m == min(depth_m, na.rm = T) ~ "top", 
                         depth_m > min(depth_m, na.rm = T) | depth_m < max(depth_m, na.rm = T) ~ "mid")) %>% 
  ungroup() %>% 
  filter(pos == "top" | pos == "bottom") %>% 
  distinct() # sometimes multiple top or bottom values; sometimes top value is negative (just means it's 0) # 
ysi_tb

ysi_tb_clean <- ysi_tb %>% 
  group_by(doy, pond_id, pos) %>% 
  slice_head(n = 1) %>% 
  select(-depth_m)
ysi_tb_clean

ysi_tb_wide <- ysi_tb_clean %>% 
  pivot_wider(id_cols = c(doy, pond_id), 
              names_from = pos, 
              values_from = temp)
ysi_tb_wide

# Plot by pond # 
low_col_B = rgb(74, 166, 81, max = 255, alpha = 180) #Pond B, Pond F
low_col_F = rgb(74, 166, 81, max = 255, alpha = 100) #Pond B, Pond F
low_col = rgb(74, 166, 81, max = 255, alpha = 255) #Pond B, Pond F
ref_col = rgb(155, 155, 155, max=255, alpha = 100) # Reference
black_col = rgb(0,0,0, max=255, alpha = 100) # Black
transparent = rgb(255,255,255, max=255, alpha = 0)

int_col_A = rgb(44, 127, 184, max = 255, alpha = 180) #Pond A, pond D
int_col_D = rgb(44, 127, 184, max = 255, alpha = 100) #Pond A, pond D
int_col = rgb(44, 127, 184, max = 255, alpha = 255) #Pond A, pond D

high_col_C = rgb(75, 31, 110, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(75, 31, 110, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(75, 31, 110, max = 255, alpha = 255) #Pond C, Pond E

col=rgb(255,48,48, max=255, alpha=75, names= 'firebrick1') # Extended heat period 

# B 
ysi_b <- ysi_tb_wide %>% 
  filter(pond_id == "B")
ysi_b
ysi_f <- ysi_tb_wide %>% 
  filter(pond_id == "F")
ysi_a <- ysi_tb_wide %>% 
  filter(pond_id == "A")
ysi_d <- ysi_tb_wide %>% 
  filter(pond_id == "D")
ysi_c <- ysi_tb_wide %>% 
  filter(pond_id == "C")
ysi_e <- ysi_tb_wide %>% 
  filter(pond_id == "E")

windows(height = 4, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure5.pdf", 
#   height = 4, 
#  width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3),  mar = c(0.5,1,1,0.5), oma = c(4,4,1,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0)) 

# Low Complexity  
plot(top ~ doy, data = ysi_b, type = "l", lwd = 2, col = low_col_B, ylim = c(15, 30))
points(bottom ~ doy, data = ysi_b, type = "l", lwd = 2, col = "gray40")
mtext(side = 2, "Water Temperature (C)", line = 1.8, cex = 0.8)
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
text(143, 30, "A", font = 2)
mtext(side = 2, "Pulsed", line = 3, cex = 0.8)
mtext(side = 3, "Low Complexity", line = 0.2, cex = 0.8)

plot(top ~ doy, data = ysi_a, type = "l", lwd = 2, col = int_col_A, ylim = c(15, 30))
points(bottom ~ doy, data = ysi_a, type = "l", lwd = 2, col = "gray40")
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
text(143, 30, "B", font = 2)
mtext(side = 3, "Intermediate", line = 0.2, cex = 0.8)

plot(top ~ doy, data = ysi_c, type = "l", lwd = 2, col = high_col_C, ylim = c(15, 30))
points(bottom ~ doy, data = ysi_c, type = "l", lwd = 2, col = "gray40")
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
text(143, 30, "C", font = 2)
mtext(side = 3, "High Complexity", line = 0.2, cex = 0.8)

plot(top ~ doy, data = ysi_f, type = "l", lwd = 2, col = low_col_F, ylim = c(15, 30))
points(bottom ~ doy, data = ysi_f, type = "l", lwd = 2, col = "gray40")
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
mtext(side = 2, "Water Temperature (C)", line = 1.8, cex = 0.8)
text(143, 30, "D", font = 2)
mtext(side = 2, "Reference", line = 3, cex = 0.8)

plot(top ~ doy, data = ysi_d, type = "l", lwd = 2, col = int_col_D, ylim = c(15, 30))
points(bottom ~ doy, data = ysi_d, type = "l", lwd = 2, col = "gray40")
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
mtext(side = 1, "Day of Year, 2020", line = 1.8, cex = 0.8)
text(143, 30, "E", font = 2)

plot(top ~ doy, data = ysi_e, type = "l", lwd = 2, col = high_col_E, ylim = c(15, 30))
points(bottom ~ doy, data = ysi_e, type = "l", lwd = 2, col = "gray40")
#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
lines(c(223,223), c(-10,700), lty = 2, lwd = 2)
rect(185,-5,190,50, col=col, border=NA)
text(143, 30, "F", font = 2)


