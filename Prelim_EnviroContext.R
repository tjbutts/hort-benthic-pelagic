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


plot(ya~xa, type='l', col=int_col, lwd=4,cex=2,cex.axis=1.5, ylim=c(0,900), xlim=c(140,245))
lines(yd~xd, type='l', col=int_col_trans, lwd=4)
lines(yb~xb, type='l', col=low_col, lwd=4)
lines(yf~xf, type='l', col =low_col_trans, lwd=4)
lines(yc~xc, type='l', col = high_col, lwd=4)
lines(ye~xe, type='l', col = high_col_trans, lwd=4)

plot(zoop_tot[zoop_tot$pond_id=="A", "doy"], 
     zoop_tot[zoop_tot$pond_id=="A", "tot_biomass"], 
     col = 'black', pch = 20, lwd = 6, type = "l",
     ylim = c(0,900), xlim = c(140,245))
mtext(side=2, line=4.5, expression(Zooplankton), cex=1.2)
mtext(side=2, line=3, expression(Biomass~"("*mu*g~L^-1*")"), cex = 1.2)

axis(side=2, 
     at=c(log10(10), log10(20), log10(30), log10(40), log10(50), 
          log10(60), log10(70), log10(80), log10(90), log10(100), 
          log10(200), log10(300), log10(400), log10(500), 
          log10(600), log10(700), log10(800), log10(900), log10(1000), 
          log10(2000), log10(3000), log10(4000), log10(5000), 
          log10(6000), log10(7000), log10(8000), log10(9000), log10(10000)),
     labels = c("10", " ", " ", " ", " ", " ", " ", " ", " ", 
                "100", " ", " ", " ", " ", " ", " ", " ", " ", 
                "1000"," "," "," "," "," "," "," "," ","10000"), 
     las=2, cex.axis = 1.2)

polygon(c(115,123,123,115), c(log10(5000), log10(5000), log10(10000), log10(10000)), col="gray20") # Pre-Fish
lines(c(122.9, 122.9), c(log10(5), log10(5000)), lwd=2, lty=3, col="gray20")
text(119, log10(7500), "Pre Fish", col="white", font=2, cex = 1.2)
lines(c(129.1, 129.1), c(log10(5), log10(6000)), lwd=2, lty=3, col="gray20")
polygon(c(129,168,168,129), c(log10(5000), log10(5000), log10(10000), log10(10000)), col="gray20") # Second Fish
text(148, log10(7500), "Post Fish Addition", col="white", font=2, cex = 1.2)

points(bmb_zoop[bmb_zoop$pond=="F", "doy"], 
       log10(bmb_zoop[bmb_zoop$pond=="F", "total_zoop"]), 
       col=high2, pch=19, lwd=4, type="l")
points(bmb_zoop[bmb_zoop$pond=="C", "doy"], 
       log10(bmb_zoop[bmb_zoop$pond=="C", "total_zoop"]), 
       col=low1, pch=19, lwd=6, type="l")
points(bmb_zoop[bmb_zoop$pond=="E", "doy"], 
       log10(bmb_zoop[bmb_zoop$pond=="E", "total_zoop"]), 
       col=low2, pch=19, lwd=4, type="l")
points(bmb_zoop[bmb_zoop$pond=="A", "doy"], 
       log10(bmb_zoop[bmb_zoop$pond=="A", "total_zoop"]), 
       col=no1, pch=19, lwd=6, type="l")
points(bmb_zoop[bmb_zoop$pond=="D", "doy"], 
       log10(bmb_zoop[bmb_zoop$pond=="D", "total_zoop"]), 
       col=no2, pch=19, lwd=4, type="l")





# Macroinvertebrate density time series 