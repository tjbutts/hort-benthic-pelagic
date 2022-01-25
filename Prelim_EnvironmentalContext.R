## Hort Environmental Context ##

# Periphyton #==================
# load data # 
periphy = read_csv('periphy_clean.csv')
periphy

#Colors for data visualization
#Ref: lty = 3, pulse: lty = 1
low_col = "#7fcdbb" # BF
int_col ="#2c7fb8" # AD
high_col = "#081d58" # CE

# striplot # 
stripchart(biomass_area ~ pond_id, data = periphy, 
           pch=16, cex=1.5, 
           frame = FALSE, vertical = FALSE, method="jitter", jitter = 0.3, 
           col=c(int_col, low_col, high_col, int_col, high_col, low_col), 
           xlab = 'Periphyton Areal Biomass (ug/cm^2)')
legend('topright', legend = c('low B-P', 'int B-P', 'high B-P'), col= c(low_col, int_col, high_col), pch = 16,
       cex=1.5)

# Macrophytes #==================
macrophy = read_csv('macrophy_clean.csv') 
macrophy

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
low_col_trans = rgb(red=127, green=205, blue=187, alpha=75, maxColorValue = 255) #BF
int_col_trans = rgb(44,127,184, alpha = 75, maxColorValue = 255) #AD
high_col_trans = rgb(8,29,88, alpha = 75, maxColorValue = 255) # CE

fs_pre_lmb = fish_size %>% filter(experiment == 'pre' & spp == 'LMB')
fs_post_lmb = fish_size %>% filter(experiment == 'post' & spp == 'LMB')
fs_pre_blg = fish_size %>% filter(experiment == 'pre' & spp == 'BLG')
fs_post_blg = fish_size %>% filter(experiment == 'post' & spp == 'BLG')
fs_pre_yep = fish_size %>% filter(experiment == 'pre' & spp == 'YEP')
fs_post_yep = fish_size %>% filter(experiment == 'post' & spp == 'YEP')

x <- list('lmb_pre' = fs_pre_lmb$length, 'lmb_post' = fs_post_lmb$length, 
          'blg_pre' = fs_pre_blg$length, 'blg_post' = fs_post_blg$length,
          'yep_pre' = fs_pre_yep$length, 'yep_post' = fs_post_yep$length)

par(mai=c(0.6,1.2,0.6,0.6))
stripchart(x, pch=16, cex=1.5, 
           frame=F, vertical=F, method='jitter', jitter=0.3,
           col=c(high_col_trans, high_col, int_col_trans, int_col, low_col_trans, low_col),
           xlab = 'Fish total length (mm)', 
           xlim= c(50,500),
           las=2)

# Zooplankton biomass time series # 
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


plot(ya~xa, type='o', col=int_col, lwd=4,cex=2,cex.axis=1.5, ylim=c(0,900), xlim=c(140,245))
lines(yd~xd, type='o', col=int_col_trans, lwd=4)
lines(yb~xb, type='o', col=low_col, lwd=4)
lines(yf~xf, type='o', col =low_col_trans, lwd=4)
lines(yc~xc, type='o', col = high_col, lwd=4)
lines(ye~xe, type='o', col = high_col_trans, lwd=4)
legend('topright', legend=c('low', 'int', 'high'), col=c(low_col, int_col, high_col), pch=20)

# Macroinvertebrate density time series 