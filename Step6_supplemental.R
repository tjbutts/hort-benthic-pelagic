
# Gastric Lavage Table # 
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

############# Time Series Array ###################

## Zooplankton Time Series ##===================================
hort_zoop 

daily_zp_biomass = hort_zoop %>% 
  group_by(pond_id, doy) %>%
  summarize(biomass = sum(biomass)) %>%
  ungroup()
daily_zp_biomass

# Separate by pond # 
zoopA = daily_zp_biomass %>% #pulse, int
  filter(pond_id == "A") 

zoopB = daily_zp_biomass %>% #pulse, low
  filter(pond_id == "B") 

zoopC = daily_zp_biomass %>% #pulse, high
  filter(pond_id == "C") 

zoopD = daily_zp_biomass %>% #ref, int
  filter(pond_id == "D") 

zoopE = daily_zp_biomass %>% #ref, high
  filter(pond_id == "E") 

zoopF = daily_zp_biomass %>% #ref, low
  filter(pond_id == "F") 

windows(height = 6, width = 6)
par(mfrow =c(3,3), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Low Coupling # 
plot(zoopB$doy, log(zoopB$biomass), yaxt = "n", xlim=c(140, 245), ylim=c(log(.1), log(800)), col.axis = transparent,
     cex = .75, pch = 17, lty = 1, col = low_col_B, type = 'l', lwd = 2,
     xlab = "", ylab = "", cex.axis= 1.2, las =2)
par(new = TRUE) #add new smooth to the same plot
plot(zoopF$doy, log(zoopF$biomass),ylim=c(log(.1), log(800)), xlim=c(140, 245), col.axis = transparent,
     cex = 0.75, pch = 16,  lty = 3, col = 'gray40',type = 'l', lwd = 2, yaxt = 'n',
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3.2, 
      expression('Zooplankton'), cex = 11/12)
mtext(side = 2, line = 1.8, 
      expression('Biomass' ~"("*mu*g~L^-1*")"), cex = 11/12)
mtext(side = 3, line = 0.1, 'Low Coupling', cex = 11/12)
#legend('topright', legend = c('Disturbed', 'Reference'), lty = c(1,3), col = c(low_col_B, 'gray40'))
axis(side=2,
     at=c( log(.1),log(.2),log(.3),log(.4),log(.5),log(.6),log(.7),log(.8),log(.9),
           log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '','1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','','','','','','','800'),
     las=0, cex.axis=1.2)
text(141, log(800), 'A', font = 2)

# Intermediate # 
plot(zoopA$doy, log(zoopA$biomass), yaxt = "n", xlim=c(140, 245), ylim=c(log(.1), log(800)), col.axis = transparent,
     cex = .75, pch = 17, lty = 1, col = int_col_A, type = 'l', lwd = 2,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(zoopD$doy, log(zoopD$biomass),ylim=c(log(.1), log(800)), xlim=c(140, 245), col.axis = transparent,
     cex = 0.75, pch = 16,  lty = 3, col = 'gray40',type = 'l', lwd = 2, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 2, line = 3, "ZP Biomass (ug/L)", cex = 1.25)
mtext(side = 3, line = 0.1, 'Intermediate', cex = 11/12)
#legend('topright', legend = c('Disturbed', 'Reference'), lty = c(1,3), col = c(int_col_A, 'gray40'))
axis(side=2,
     at=c( log(.1),log(.2),log(.3),log(.4),log(.5),log(.6),log(.7),log(.8),log(.9),
           log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('', '', '', '', '', '', '', '', '','1', '', '', '', '', '', '', '', '', '', '', '','','','','','','','','','','','','','',''),
     las=0, cex.axis=.8)
text(141, log(800), 'B', font = 2)

# High # 
plot(zoopC$doy, log(zoopC$biomass), yaxt = "n", xlim=c(140, 245), ylim=c(log(.1), log(800)), col.axis = transparent, 
     cex = .75, pch = 17, lty = 1, col = high_col_C, type = 'l', lwd = 2,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(zoopE$doy, log(zoopE$biomass),ylim=c(log(.1), log(800)), xlim=c(140, 245), col.axis = transparent,
     cex = 0.75, pch = 16,  lty = 3, col = 'gray40',type = 'l', lwd = 2, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
#mtext(side = 2, line = 3, "ZP Biomass (ug/L)", cex = 1.25)
mtext(side = 3, line = 0.1, 'High Coupling', cex = 11/12)
legend('bottomleft', legend = c('Disturbed', 'Reference'), lty = c(1,3), col = c(high_col_C, 'gray40'))
axis(side=2,
     at=c( log(.1),log(.2),log(.3),log(.4),log(.5),log(.6),log(.7),log(.8),log(.9),
           log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300),log(400),log(500),log(600),log(700),log(800)), #Where the tick marks should be drawn
     labels = c('', '', '', '', '', '', '', '', '','', '', '', '', '', '', '', '', '', '', '', '','','','','','','','','','','','','','',''),
     las=0, cex.axis=.8)
text(141, log(800), 'C', font = 2)

## Zoobenthos Time Series##============================
daily_miv_density = hort_mivdensity %>%
  #filter(gear == 'HS') %>%
  group_by(pond_id, doy) %>%
  summarize(density = sum(density)) %>%
  ungroup()
daily_miv_density

max(daily_miv_density$density) # 7845.07
min(daily_miv_density$density) # 352.1127

# Separate by pond # 
mivA = daily_miv_density %>% #pulse, int
  filter(pond_id == "A") 

mivB = daily_miv_density %>% #pulse, low
  filter(pond_id == "B") 

mivC = daily_miv_density %>% #pulse, high
  filter(pond_id == "C") 

mivD = daily_miv_density %>% #ref, int
  filter(pond_id == "D") 

mivE = daily_miv_density %>% #ref, high
  filter(pond_id == "E") 

mivF = daily_miv_density %>% #ref, low
  filter(pond_id == "F") 

# Low Coupling # 
plot(mivB$doy, log(mivB$density), yaxt = "n", xlim=c(140, 245), ylim=c(log(300), log(14000)),col.axis = transparent,
     cex = .75, pch = 17, lty = 1, col = low_col_B, type = 'l', lwd = 2,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(mivF$doy, log(mivF$density),ylim=c(log(300), log(14000)), xlim=c(140, 245), col.axis = transparent,
     cex = 0.75, pch = 16,  lty = 3, col = 'gray40',type = 'l', lwd = 2, yaxt = 'n',
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3.2, 
      expression('Macroinvertebrate'), cex = 11/12)
mtext(side = 2, line = 1.8, 
      expression('Density' ~"(#"~L^-1*")"), cex = 11/12)
#legend('topright', legend = c('Disturbed', 'Reference'), lty = c(1,3), col = c(low_col_B, 'gray40'))
axis(side=2,
     at=c(log(300), log(400), log(500),log(600),log(700),log(800),log(900),log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), 
          log(9000), log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('300','', '','','','','','1000','','','','', '','','','','10000', ''),
     las=0, cex.axis=1.2)
text(141, log(14000), 'D', font = 2)

# Intermediate # 
plot(mivA$doy, log(mivA$density), yaxt = "n", xlim=c(140, 245), ylim=c(log(300), log(14000)), col.axis = transparent,
     cex = .75, pch = 17, lty = 1, col = int_col_A, type = 'l', lwd = 2,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(mivD$doy, log(mivD$density),ylim=c(log(300), log(14000)), xlim=c(140, 245), col.axis = transparent,
     cex = 0.75, pch = 16,  lty = 3, col = 'gray40',type = 'l', lwd = 2, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
#legend('topright', legend = c('Disturbed', 'Reference'), lty = c(1,3), col = c(int_col_A, 'gray40'))
axis(side=2,
     at=c(log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000), log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','1000', '','','','','','','','','',''),
     las=0, cex.axis=.8)
text(141, log(14000), 'E', font = 2)

# High # 
plot(mivC$doy, log(mivC$density), yaxt = "n", xlim=c(140, 245), ylim=c(log(300), log(14000)), col.axis = transparent,
     cex = .75, pch = 17, lty = 1, col = high_col_C, type = 'l', lwd = 2,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(mivE$doy, log(mivE$density),ylim=c(log(300), log(14000)), xlim=c(140, 245), col.axis = transparent, 
     cex = 0.75, pch = 16,  lty = 3, col = 'gray40',type = 'l', lwd = 2, yaxt='n',
     xlab = "", ylab = "", cex.axis= 1.2)
#legend('topright', legend = c('Disturbed', 'Reference'), lty = c(1,3), col = c(high_col_C, 'gray40'))
axis(side=2,
     at=c(log(900), log(1000),
          log(2000), log(3000), log(4000), log(5000), log(6000), log(7000), log(8000), log(9000), log(10000), log(20000)), #Where the tick marks should be drawn
     labels = c('','1000', '','','','','','','','','',''),
     las=2, cex.axis=.8)
text(141, log(14000), 'F', font = 2)

# Periphyton Time Series ##==================================
daily_peri_areabiom = hort_periphy %>%
  group_by(pond_id, collect) %>%
  summarize(biomass_area = sum(biomass_area_cm2)) %>%
  ungroup()
daily_peri_areabiom

max(daily_peri_areabiom$biomass_area) # 0.3273
min(daily_peri_areabiom$biomass_area) # 0.0130

# Separate by pond # 
periA = daily_peri_areabiom %>% #pulse, int
  filter(pond_id == "A") 

periB = daily_peri_areabiom %>% #pulse, low
  filter(pond_id == "B") 

periC = daily_peri_areabiom %>% #pulse, high
  filter(pond_id == "C") 

periD = daily_peri_areabiom %>% #ref, int
  filter(pond_id == "D") 

periE = daily_peri_areabiom %>% #ref, high
  filter(pond_id == "E") 

periF = daily_peri_areabiom %>% #ref, low
  filter(pond_id == "F") 

# Low Coupling # 
plot(periB$collect, periB$biomass_area,  xlim=c(140, 245), ylim=c(0, 0.35),  
     cex = .75, pch = 17, lty = 1, col = low_col_B, type = 'l', lwd = 2,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(periF$collect, periF$biomass_area,ylim=c(0, 0.35), xlim=c(140, 245), col.axis = transparent,
     cex = 0.75, pch = 16,  lty = 3, col = 'gray40',type = 'l', lwd = 2,
     xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 3.2, 
      expression('Periphyton Areal'), cex = 11/12)
mtext(side = 2, line = 1.8, 
      expression('Biomass' ~"("*mu*g~cm^-2*")"), cex = 11/12)
text(141, 0.35, 'G', font = 2)
#axis(side = 2, at = c(0, 0.1, 0.2, 0.3, 0.4), cex = 1.2)

#legend('topright', legend = c('Disturbed', 'Reference'), lty = c(1,3), col = c(low_col_B, 'gray40'))


# Intermediate # 
plot(periA$collect, periA$biomass_area,  xlim=c(140, 245), ylim=c(0, 0.35), yaxt='n',
     cex = .75, pch = 17, lty = 1, col = int_col_A, type = 'l', lwd = 2,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(periD$collect, periD$biomass_area,ylim=c(0, 0.35), xlim=c(140, 245), col.axis = transparent,
     cex = 0.75, pch = 16,  lty = 3, col = 'gray40',type = 'l', lwd = 2,
     xlab = "", ylab = "", cex.axis= 1.2)
axis(side = 2, at = c(0, 0.1, 0.2, 0.3, 0.4), cex = 1.2, labels = F)
mtext(side = 1, line = 2, 'Day of Year, 2020')
text(141, 0.35, 'H', font = 2)

# High # 
plot(periC$collect, periC$biomass_area,  xlim=c(140, 245), ylim=c(0, 0.35), yaxt='n', 
     cex = .75, pch = 17, lty = 1, col = high_col_C, type = 'l', lwd = 2,
     xlab = "", ylab = "", cex.axis= 1.2)
par(new = TRUE) #add new smooth to the same plot
plot(periE$collect, periE$biomass_area,ylim=c(0, 0.35), xlim=c(140, 245), col.axis = transparent, 
     cex = 0.75, pch = 16,  lty = 3, col = 'gray40',type = 'l', lwd = 2, 
     xlab = "", ylab = "", cex.axis= 1.2)
#legend('topright', legend = c('Disturbed', 'Reference'), lty = c(1,3), col = c(high_col_C, 'gray40'))
text(141, 0.35, 'I', font = 2)

# Nutrients # 
hort_field = read_csv('hort20_surface_dat.csv')

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
mtext(side = 3, line = 0.1, 'Low Coupling', cex = 11/12)
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
mtext(side = 3, line = 0.1, 'High Coupling', cex = 11/12)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

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
axis(side = 2, at = c(0,0.2, 0.4,0.6,0.8,1.0,1.2), cex.axis = 1, labels = F) 

