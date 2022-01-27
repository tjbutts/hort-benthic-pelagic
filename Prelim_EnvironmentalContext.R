## Hort Environmental Context ##

# Periphyton #==================
# load data # 
periphy = read_csv('periphy_clean.csv')
periphy

#Colors for data visualization
#Ref: lty = 3, pulse: lty = 1
low_col = "#4AA651" #Pond B, Pond F
int_col ="#2c7fb8" #Pond A, pond D
high_col = "#081d58" #Pond C, Pond E
low_col_trans = rgb(red=74, green=166, blue=81, alpha=85, maxColorValue = 255) #BF
int_col_trans = rgb(44,127,184, alpha = 85, maxColorValue = 255) #AD
high_col_trans = rgb(8,29,88, alpha = 85, maxColorValue = 255) # CE

windows(height=6, width=8)
# striplot # 
stripchart(biomass_area ~ pond_id, data = periphy, 
           pch=16, cex=1.5, 
           frame = FALSE, vertical = FALSE, method="jitter", jitter = 0.3, 
           col=c(int_col, low_col, high_col, int_col_trans, high_col_trans, low_col_trans), 
           xlab = 'Periphyton Areal Biomass (ug/cm^2)')
legend('topright', legend = c('low B-P', 'int B-P', 'high B-P'), col= c(low_col, int_col, high_col), pch = 16,
       cex=1.5)

# Macrophytes #==================
macrophy = read_csv('macrophy_clean.csv') 
macrophy

windows(height=6, width=8)
# stripplot # 
stripchart(biomass ~ pond_id, data = macrophy, 
           pch=16, cex=1.5, 
           frame= FALSE, vertical = FALSE, method='jitter', jitter=0.3, 
           col=c(int_col, low_col, high_col, int_col, high_col, low_col),
           xlab = 'Macrophyte dry biomass (g)')
legend('topright', legend = c('low B-P', 'int B-P', 'high B-P'), col= c(low_col, int_col, high_col), pch = 16,
       cex=1.5)

# Fish body condition - pre/post # ================ 
fish_size = read_csv('fish_length_weight.csv')
fish_size

# stripplot #

fs_pre_lmb = fish_size %>% filter(experiment == 'pre' & spp == 'LMB')
fs_post_lmb = fish_size %>% filter(experiment == 'post' & spp == 'LMB')
fs_pre_blg = fish_size %>% filter(experiment == 'pre' & spp == 'BLG')
fs_post_blg = fish_size %>% filter(experiment == 'post' & spp == 'BLG')
fs_pre_yep = fish_size %>% filter(experiment == 'pre' & spp == 'YEP')
fs_post_yep = fish_size %>% filter(experiment == 'post' & spp == 'YEP')

x <- list('lmb_pre' = fs_pre_lmb$length, 'lmb_post' = fs_post_lmb$length, 
          'blg_pre' = fs_pre_blg$length, 'blg_post' = fs_post_blg$length,
          'yep_pre' = fs_pre_yep$length, 'yep_post' = fs_post_yep$length)

windows(height=6, width=8)
par(mai=c(0.6,1.2,0.6,0.6))
stripchart(x, pch=16, cex=1.5, 
           frame=F, vertical=F, method='jitter', jitter=0.3,
           col=c(high_col_trans, high_col, int_col_trans, int_col, low_col_trans, low_col),
           xlab = 'Fish total length (mm)', 
           xlim= c(50,500),
           las=2)

# Zooplankton biomass time series #=========================
zoop = read_csv('hort_zp_clean_11622.csv')
zoop

# total biomass 
zoop_tot = zoop %>%
  group_by(pond_id, doy, treatment, period) %>%
  summarise(tot_biomass = sum(biomass)) %>%
  ungroup() 
zoop_tot = as.data.frame(zoop_tot) # need to un-tibble-fy this

xa = zoop_tot[zoop_tot$pond_id=='A', 'doy']
ya = zoop_tot[zoop_tot$pond_id=='A', 'tot_biomass']
xb = zoop_tot[zoop_tot$pond_id=='B', 'doy']
yb = zoop_tot[zoop_tot$pond_id=='B', 'tot_biomass']
xc = zoop_tot[zoop_tot$pond_id=='C', 'doy']
yc = zoop_tot[zoop_tot$pond_id=='C', 'tot_biomass']
xd = zoop_tot[zoop_tot$pond_id=='D', 'doy']
yd = zoop_tot[zoop_tot$pond_id=='D', 'tot_biomass']
xe = zoop_tot[zoop_tot$pond_id=='E', 'doy']
ye = zoop_tot[zoop_tot$pond_id=='E', 'tot_biomass']
xf = zoop_tot[zoop_tot$pond_id=='F', 'doy']
yf = zoop_tot[zoop_tot$pond_id=='F', 'tot_biomass']

windows(height=6, width=8)
par(mai=c(0.9,1.2,0.6,0.6))
plot(ya~xa, type='o', col=int_col, lwd=4,cex=2,cex.axis=1.5, ylim=c(0,600), xlim=c(140,245), ylab='',xlab='')
mtext(side=2, 'zooplankton biomass', line=3, cex=2)
mtext(side=1, 'Day of Year, 2020', line=3, cex=2)
lines(yd~xd, type='o', col=int_col_trans, lwd=4)
lines(yb~xb, type='o', col=low_col, lwd=4)
lines(yf~xf, type='o', col =low_col_trans, lwd=4)
lines(yc~xc, type='o', col = high_col, lwd=4)
lines(ye~xe, type='o', col = high_col_trans, lwd=4)
legend('topright', legend=c('low', 'int', 'high'), col=c(low_col, int_col, high_col), pch=20, cex=2)

# Log scale # 
par(mai=c(0.9,1.2,0.6,0.6))
plot(log(ya)~xa, type='l', col=int_col, lwd=4,cex=2,cex.axis=1.5, 
     ylim=c(log(0.1),log(600)), xlim=c(140,245), ylab='',xlab='', yaxt='n')
mtext(side=2, 'zooplankton biomass', line=4, cex=2)
mtext(side=1, 'Day of Year, 2020', line=3, cex=2)
lines(log(yd)~xd, type='l', col=int_col_trans, lwd=4)
lines(log(yb)~xb, type='l', col=low_col, lwd=4)
lines(log(yf)~xf, type='l', col =low_col_trans, lwd=4)
lines(log(yc)~xc, type='l', col = high_col, lwd=4)
lines(log(ye)~xe, type='l', col = high_col_trans, lwd=4)
legend('topright', legend=c('low', 'int', 'high'), col=c(low_col, int_col, high_col), pch=20, cex=1.5)
axis(side=2,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), 
          log(0.9), log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('0.1','','','','','','','','','1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=1, cex.axis=1)


# Macroinvertebrate density time series #================= 
hort_mivdensity 

# total density 
miv_tot = hort_mivdensity %>%
  group_by(pond_id, doy, treatment, period, gear) %>%
  summarise(density = sum(density)) %>%
  ungroup() 
miv_tot 

# Filter by gear 
miv_tot_hs = miv_tot %>% filter(gear == 'HS')
miv_tot_hs = as.data.frame(miv_tot_hs) #undo tibble for plotting
miv_tot_ed = miv_tot %>% filter(gear == 'ED')
miv_tot_ed = as.data.frame(miv_tot_ed) #undo tibble for plotting

windows(height=8, width =8)
# Ekman
xa = miv_tot_ed[miv_tot_ed$pond_id=='A', 'doy'] # No data so far
ya = miv_tot_ed[miv_tot_ed$pond_id=='A', 'density'] # No data so far
xb = miv_tot_ed[miv_tot_ed$pond_id=='B', 'doy']
yb = miv_tot_ed[miv_tot_ed$pond_id=='B', 'density']
xc = miv_tot_ed[miv_tot_ed$pond_id=='C', 'doy']
yc = miv_tot_ed[miv_tot_ed$pond_id=='C', 'density']
xd = miv_tot_ed[miv_tot_ed$pond_id=='D', 'doy'] # No data so far
yd = miv_tot_ed[miv_tot_ed$pond_id=='D', 'density'] # No data so far 
xe = miv_tot_ed[miv_tot_ed$pond_id=='E', 'doy']
ye = miv_tot_ed[miv_tot_ed$pond_id=='E', 'density']
xf = miv_tot_ed[miv_tot_ed$pond_id=='F', 'doy']
yf = miv_tot_ed[miv_tot_ed$pond_id=='F', 'density']


par(mai=c(0.9,1.2,0.6,0.6), mfrow=c(2,1))
max(miv_tot_ed$density) # 41153.85
min(miv_tot_ed$density) # 6730.769
plot(yb~xb, type='o', col=low_col, lwd=4,cex=1.5,cex.axis=1, ylim=c(6000,45000), xlim=c(140,245), ylab='',xlab='')
mtext(side=2, 'macroinvertebrate density (ED)', line=3, cex=1.5)
mtext(side=1, 'Day of Year, 2020', line=3, cex=1.5)
lines(yf~xf, type='o', col =low_col_trans, lwd=4)
lines(yc~xc, type='o', col = high_col, lwd=4)
lines(ye~xe, type='o', col = high_col_trans, lwd=4)
legend('topright', legend=c('low', 'int', 'high'), col=c(low_col, int_col, high_col), pch=20, cex=1.5)

# Hess 
xa = miv_tot_hs[miv_tot_hs$pond_id=='A', 'doy'] # No data so far
ya = miv_tot_hs[miv_tot_hs$pond_id=='A', 'density'] # No data so far
xb = miv_tot_hs[miv_tot_hs$pond_id=='B', 'doy']
yb = miv_tot_hs[miv_tot_hs$pond_id=='B', 'density']
xc = miv_tot_hs[miv_tot_hs$pond_id=='C', 'doy']
yc = miv_tot_hs[miv_tot_hs$pond_id=='C', 'density']
xd = miv_tot_hs[miv_tot_hs$pond_id=='D', 'doy'] # No data so far
yd = miv_tot_hs[miv_tot_hs$pond_id=='D', 'density'] # No data so far 
xe = miv_tot_hs[miv_tot_hs$pond_id=='E', 'doy']
ye = miv_tot_hs[miv_tot_hs$pond_id=='E', 'density']
xf = miv_tot_hs[miv_tot_hs$pond_id=='F', 'doy']
yf = miv_tot_hs[miv_tot_hs$pond_id=='F', 'density']

par(mai=c(0.9,1.2,0.6,0.6))
max(miv_tot_hs$density) # 2690.141
min(miv_tot_hs$density) # 408.4507
plot(yb~xb, type='o', col=low_col, lwd=4,cex=1.5,cex.axis=1, ylim=c(400,3000), xlim=c(140,245), ylab='',xlab='')
mtext(side=2, 'macroinvertebrate density (HS)', line=3, cex=1.5)
mtext(side=1, 'Day of Year, 2020', line=3, cex=1.5)
lines(yf~xf, type='o', col =low_col_trans, lwd=4)
lines(yc~xc, type='o', col = high_col, lwd=4)
lines(ye~xe, type='o', col = high_col_trans, lwd=4)
legend('topright', legend=c('low', 'int', 'high'), col=c(low_col, int_col, high_col), pch=20, cex=1.5)
graphics.off()

# Gastric Lavage Diet Fraction #=================
hort_fish_gaslav 
fish_diet = hort_fish_gaslav %>%
  group_by(pond, fish_id, broad_taxa) %>%
  summarise(abundance = sum(abundance)) %>%
  ungroup()
fish_diet

diet_benthos_lmb = fish_diet %>% filter(broad_taxa == 'benthos' & fish_id == 'lmb') %>%
  unite('fish_diet', fish_id:broad_taxa)
diet_zoop_lmb = fish_diet %>% filter(broad_taxa == 'zoop' & fish_id == 'lmb') %>%
  unite('fish_diet', fish_id:broad_taxa)
diet_benthos_blg = fish_diet %>% filter(broad_taxa == 'benthos' & fish_id == 'blg') %>%
  unite('fish_diet', fish_id:broad_taxa)
diet_zoop_blg = fish_diet %>% filter(broad_taxa == 'zoop' & fish_id == 'blg') %>%
  unite('fish_diet', fish_id:broad_taxa)
diet_benthos_yep = fish_diet %>% filter(broad_taxa == 'benthos' & fish_id == 'yep') %>%
  unite('fish_diet', fish_id:broad_taxa)
diet_zoop_yep = fish_diet %>% filter(broad_taxa == 'zoop' & fish_id == 'yep') %>%
  unite('fish_diet', fish_id:broad_taxa)

diet_consolidate = rbind(diet_benthos_lmb, diet_zoop_lmb, diet_benthos_blg, 
                         diet_zoop_blg, diet_benthos_yep, diet_zoop_yep)
diet_consolidate
windows(height=6, width=8)
par(mai=c(0.9,1.2,0.6,0.6))
stripchart(abundance ~ fish_diet, data = diet_consolidate, col='white', xaxt='n', yaxt='n', 
           xlab='Organism Abundance', cex.axis=2)
axis(1, las=1)
axis(2, at=c(1,2,3,4,5), labels=c('blg_benthos', 'blg_zoop', 'lmb_benthos', 'yep_benthos', 'yep_zoop'), las=2)
high = diet_consolidate$pond == 'C' | diet_consolidate$pond == 'E'
stripchart(abundance ~fish_diet, data = diet_consolidate[high,], col=high_col, add=T, pch=20, cex=3)
int = diet_consolidate$pond == 'A' | diet_consolidate$pond == 'D'
stripchart(abundance ~fish_diet, data = diet_consolidate[int,], col=int_col, add=T, pch=20, cex=3)
low = diet_consolidate$pond == 'B' | diet_consolidate$pond == 'F'
stripchart(abundance ~fish_diet, data = diet_consolidate[low,], col=low_col, add=T, pch=20, cex=3)
legend('topright', legend=c('low', 'int', 'high'), col=c(low_col, int_col, high_col), pch=20, cex=2)

