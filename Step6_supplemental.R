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
field_B_loess = loess(tp ~ doy, data = fieldB, span = 0.20) # 10% span
field_B_loess

# get smoothed output
field_B_smooth = predict(field_B_loess, se = TRUE)

# loess low reference
field_F_loess = loess(tp ~ doy, data = fieldF, span = 00.20) # 10% span
field_F_loess

# get smoothed output
field_F_smooth = predict(field_F_loess, se = TRUE) 

#======= INTERMEDIATE ========# 
# loess intermediate pulse
field_A_loess = loess(tp ~ doy, data = fieldA, span = 00.20) # 10% span
field_A_loess

# get smoothed output
field_A_smooth = predict(field_A_loess, se = TRUE)

# loess intermediate reference
field_D_loess = loess(tp ~ doy, data = fieldD, span = 00.20) # 10% span
field_D_loess

# get smoothed output
field_D_smooth = predict(field_D_loess, se = TRUE) 

#======= HIGH ========# 
# loess intermediate pulse
field_C_loess = loess(tp ~ doy, data = fieldC, span = 00.20) # 10% span
field_C_loess

# get smoothed output
field_C_smooth = predict(field_C_loess, se = TRUE)

# loess intermediate reference
field_E_loess = loess(tp ~ doy, data = fieldE, span = 00.20) # 10% span
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
polygon(c(143:241, 241,143), c(field_F_smooth$fit - field_F_smooth$se.fit, 
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
polygon(c(143:241, 241,143), c(field_B_smooth$fit - field_B_smooth$se.fit, 
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
polygon(c(143:241, 241,143), c(field_D_smooth$fit - field_D_smooth$se.fit, 
                               rev(field_D_smooth$fit + field_D_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(field_D_smooth$fit, x=fieldD$doy, col=ref_col, lwd = 2)
mtext(side = 3, line = 0.1, 'Intermediate', cex = 11/12)

par(new=T) # add new smooth to same plot 

plot(fieldA$tp, x=fieldA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,90), col = int_col_D, yaxt= 'n', col.axis = transparent)
polygon(c(143:241, 241,143), c(field_A_smooth$fit - field_A_smooth$se.fit, 
                               rev(field_A_smooth$fit + field_A_smooth$se.fit)), 
        col = int_col_D, border = NA)
lines(field_A_smooth$fit, x=fieldA$doy, col=int_col_D, lwd = 2)
text(141, 90, 'B', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)

plot(fieldE$tp, x=fieldE$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,90), col = ref_col, col.axis = transparent)
polygon(c(143:241, 241,143), c(field_E_smooth$fit - field_E_smooth$se.fit, 
                               rev(field_E_smooth$fit + field_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(field_E_smooth$fit, x=fieldE$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(fieldC$tp, x=fieldC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,90), col = high_col_E, yaxt = 'n', col.axis = transparent)
polygon(c(143:241, 241,143), c(field_C_smooth$fit - field_C_smooth$se.fit, 
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
polygon(c(143:241, 241,143), c(field_F_smooth$fit - field_F_smooth$se.fit, 
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
polygon(c(143:241, 241,143), c(field_B_smooth$fit - field_B_smooth$se.fit, 
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
polygon(c(143:241, 241,143), c(field_D_smooth$fit - field_D_smooth$se.fit, 
                               rev(field_D_smooth$fit + field_D_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(field_D_smooth$fit, x=fieldD$doy, col=ref_col, lwd = 2)


par(new=T) # add new smooth to same plot 

plot(fieldA$tn, x=fieldA$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,1.2), col = int_col_D, yaxt= 'n', yaxt = 'n')
polygon(c(143:241, 241,143), c(field_A_smooth$fit - field_A_smooth$se.fit, 
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
polygon(c(143:241, 241,143), c(field_E_smooth$fit - field_E_smooth$se.fit, 
                               rev(field_E_smooth$fit + field_E_smooth$se.fit)), 
        col = ref_col, border = NA)
lines(field_E_smooth$fit, x=fieldE$doy, col=ref_col, lwd = 2)

par(new=T) # add new smooth to same plot 

plot(fieldC$tn, x=fieldC$doy, type = 'p', pch = 20, cex=1.5, xlab ='', ylab = '',
     xlim=c(140, 245), ylim=c(0,1.2), col = high_col_E, yaxt = 'n', col.axis = transparent)
polygon(c(143:241, 241,143), c(field_C_smooth$fit - field_C_smooth$se.fit, 
                               rev(field_C_smooth$fit + field_C_smooth$se.fit)), 
        col = high_col_E, border = NA)
lines(field_C_smooth$fit, x=fieldC$doy, col=high_col_E, lwd = 2)
text(141, 1.2, 'F', font = 2)

#Add in the nutrient pulse dates to the graph
lines(c(176,176), c(-10,700), lty = 3)
lines(c(211,211), c(-10,700), lty = 3)
axis(side = 2, at = c(0,0.2, 0.4,0.6,0.8,1.0,1.2), cex.axis = 1, labels = F) 
