## Sofia Macroinvertebrate Data ## 

#============================================#
# STEP 1: LOAD IN DATASET
#============================================#
rm(list=ls())
graphics.off()

# Required Libraries for analysis and visualization
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(magrittr)) install.packages('magrittr')
library(magrittr)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate) 
if (!require(readr)) install.packages('readr')
library(readr)
if (!require(vegan)) install.packages('vegan')
library(vegan)
# Visualization
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2) 
if (!require(scales)) install.packages('scales')
library(scales) 

hort_mivdensity = read_csv('hort_mivdensity.csv') # macroinvertebrates 
hort_mivdensity = mutate(hort_mivdensity, coupling = case_when(pond_id == 'A' | pond_id == 'D' ~ 'intermediate',
                                                    pond_id == 'B' | pond_id == 'F' ~ 'low',
                                                    pond_id == 'C' | pond_id == 'E' ~ 'high'))
hort_mivdensity

# Density Calculations #=====================
#Colors for data visualization
#Ref: lty = 3, pulse: lty = 1
low_col = "#4AA651" #Pond B, Pond F
int_col ="#2c7fb8" #Pond A, pond D
high_col = "#081d58" #Pond C, Pond E
low_col_trans = rgb(red=74, green=166, blue=81, alpha=85, maxColorValue = 255) #BF
int_col_trans = rgb(44,127,184, alpha = 85, maxColorValue = 255) #AD
high_col_trans = rgb(8,29,88, alpha = 85, maxColorValue = 255) # CE

# total density by larger grouping/pond/doy
miv_tot_group = hort_mivdensity %>%
  group_by(pond_id, doy, treatment, period, common_name, gear) %>%
  summarise(density = sum(density)) %>%
  ungroup() %>% 
  drop_na() %>%
  filter(common_name != 'caddisfly_case' | common_name != 'tadpole')
miv_tot_group

# total density by pond/doy 
miv_tot = hort_mivdensity %>%
  filter(common_name != 'caddisfly_case' | common_name != 'tadpole') %>%
  group_by(pond_id, doy, treatment, period) %>%
  summarise(density = sum(density)) %>%
  ungroup() %>% 
  drop_na() %>% 
  as.data.frame()
miv_tot

# Total by pond/doy 
xa = miv_tot[miv_tot$pond_id=='A', 'doy'] 
ya = miv_tot[miv_tot$pond_id=='A', 'density'] 
xb = miv_tot[miv_tot$pond_id=='B', 'doy']
yb = miv_tot[miv_tot$pond_id=='B', 'density']
xc = miv_tot[miv_tot$pond_id=='C', 'doy']
yc = miv_tot[miv_tot$pond_id=='C', 'density']
xd = miv_tot[miv_tot$pond_id=='D', 'doy'] 
yd = miv_tot[miv_tot$pond_id=='D', 'density']  
xe = miv_tot[miv_tot$pond_id=='E', 'doy']
ye = miv_tot[miv_tot$pond_id=='E', 'density']
xf = miv_tot[miv_tot$pond_id=='F', 'doy']
yf = miv_tot[miv_tot$pond_id=='F', 'density']

windows(height=4, width =6)
max(miv_tot$density) # 9224.138
min(miv_tot$density) # 301.7241
plot(yb~xb, type='o', col=low_col, lwd=4,cex=1.5,cex.axis=1, ylim=c(0,10000), xlim=c(140,245), ylab='',xlab='')
mtext(side=2, 'macroinvertebrate density', line=3, cex=1.5)
mtext(side=1, 'Day of Year, 2020', line=3, cex=1.5)
lines(yf~xf, type='o', col =low_col_trans, lwd=4)
lines(yc~xc, type='o', col = high_col, lwd=4)
lines(ye~xe, type='o', col = high_col_trans, lwd=4)
lines(ya~xa, type='o', col = int_col, lwd=4)
lines(yd~xd, type='o', col = int_col_trans, lwd=4)
legend('topright', legend=c('low', 'int', 'high'), col=c(low_col, int_col, high_col), pch=20, cex=1.5)

# total density by pond/doy/gear
miv_tot_g = hort_mivdensity %>%
  filter(common_name != 'caddisfly_case' | common_name != 'tadpole') %>%
  group_by(pond_id, doy, treatment, period, gear) %>%
  summarise(density = sum(density)) %>%
  ungroup() %>% 
  drop_na() %>% 
  as.data.frame()
miv_tot_g

# Filter by gear 
miv_tot_hs = miv_tot_g %>% filter(gear == 'HS')
miv_tot_hs = as.data.frame(miv_tot_hs) #undo tibble for plotting
miv_tot_ed = miv_tot_g %>% filter(gear == 'ED')
miv_tot_ed = as.data.frame(miv_tot_ed) #undo tibble for plotting

# Ekman
xa = miv_tot_ed[miv_tot_ed$pond_id=='A', 'doy'] 
ya = miv_tot_ed[miv_tot_ed$pond_id=='A', 'density'] 
xb = miv_tot_ed[miv_tot_ed$pond_id=='B', 'doy']
yb = miv_tot_ed[miv_tot_ed$pond_id=='B', 'density']
xc = miv_tot_ed[miv_tot_ed$pond_id=='C', 'doy']
yc = miv_tot_ed[miv_tot_ed$pond_id=='C', 'density']
xd = miv_tot_ed[miv_tot_ed$pond_id=='D', 'doy'] 
yd = miv_tot_ed[miv_tot_ed$pond_id=='D', 'density']  
xe = miv_tot_ed[miv_tot_ed$pond_id=='E', 'doy']
ye = miv_tot_ed[miv_tot_ed$pond_id=='E', 'density']
xf = miv_tot_ed[miv_tot_ed$pond_id=='F', 'doy']
yf = miv_tot_ed[miv_tot_ed$pond_id=='F', 'density']

max(miv_tot_ed$density) # 9224.138
min(miv_tot_ed$density) # 301.7241
plot(yb~xb, type='o', col=low_col, lwd=4,cex=1.5,cex.axis=1, ylim=c(0,10000), xlim=c(140,245), ylab='',xlab='')
mtext(side=2, 'macroinvertebrate density (ED)', line=3, cex=1.5)
mtext(side=1, 'Day of Year, 2020', line=3, cex=1.5)
lines(yf~xf, type='o', col =low_col_trans, lwd=4)
lines(yc~xc, type='o', col = high_col, lwd=4)
lines(ye~xe, type='o', col = high_col_trans, lwd=4)
lines(ya~xa, type='o', col = int_col, lwd=4)
lines(yd~xd, type='o', col = int_col_trans, lwd=4)
legend('topright', legend=c('low', 'int', 'high'), col=c(low_col, int_col, high_col), pch=20, cex=1.5)

# Hess 
xa = miv_tot_hs[miv_tot_hs$pond_id=='A', 'doy'] 
ya = miv_tot_hs[miv_tot_hs$pond_id=='A', 'density'] 
xb = miv_tot_hs[miv_tot_hs$pond_id=='B', 'doy']
yb = miv_tot_hs[miv_tot_hs$pond_id=='B', 'density']
xc = miv_tot_hs[miv_tot_hs$pond_id=='C', 'doy']
yc = miv_tot_hs[miv_tot_hs$pond_id=='C', 'density']
xd = miv_tot_hs[miv_tot_hs$pond_id=='D', 'doy'] 
yd = miv_tot_hs[miv_tot_hs$pond_id=='D', 'density'] 
xe = miv_tot_hs[miv_tot_hs$pond_id=='E', 'doy']
ye = miv_tot_hs[miv_tot_hs$pond_id=='E', 'density']
xf = miv_tot_hs[miv_tot_hs$pond_id=='F', 'doy']
yf = miv_tot_hs[miv_tot_hs$pond_id=='F', 'density']

par(mai=c(0.9,1.2,0.6,0.6))
max(miv_tot_hs$density) # 7774.648
min(miv_tot_hs$density) # 408.4507
plot(yb~xb, type='o', col=low_col, lwd=4,cex=1.5,cex.axis=1, ylim=c(0,10000), xlim=c(140,245), ylab='',xlab='')
mtext(side=2, 'macroinvertebrate density (HS)', line=3, cex=1.5)
mtext(side=1, 'Day of Year, 2020', line=3, cex=1.5)
lines(yf~xf, type='o', col =low_col_trans, lwd=4)
lines(yc~xc, type='o', col = high_col, lwd=4)
lines(ye~xe, type='o', col = high_col_trans, lwd=4)
lines(ya~xa, type='o', col = int_col, lwd=4)
lines(yd~xd, type='o', col = int_col_trans, lwd=4)
legend('topright', legend=c('low', 'int', 'high'), col=c(low_col, int_col, high_col), pch=20, cex=1.5)

# Ordination of MIV data - common_name #=====================
hort_mivdensity 
# total density grouped by doy/pond_id/common_name
miv_tot_comm = hort_mivdensity %>%
  group_by(pond_id, doy, coupling, common_name) %>%
  summarise(density = sum(density)) %>%
  ungroup() %>% 
  drop_na()
miv_tot_comm

miv_comm_wide = miv_tot_comm %>%
  pivot_wider(names_from = 'common_name', 
              values_from = 'density') %>% 
  select(!c(caddisfly_case, tadpole))
miv_comm_wide[is.na(miv_comm_wide)] = 0
miv_comm_wide 

# Run an nmds (make shape doy and color treatment) # 
# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
miv_ord_m = as.matrix(miv_comm_wide[,4:17])

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
miv_hell = decostand(miv_ord_m, method = 'hellinger')

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a 
# poor representation
miv_nmds = metaMDS(miv_hell, k=2,trymax=100, distance='bray') 
miv_nmds # Stress is 0.11, decent
stressplot(miv_nmds) # Pretty low scatter, good fit  
plot(miv_nmds) # Check it out

ordiplot(miv_nmds, type='n')
orditorp(miv_nmds, display='species') 

# Extract nMDS scores (x and y coordinates, for better plotting)
data.scores = as.data.frame(scores(miv_nmds))

# Add columns to the data frame from original data 
data.scores$pond_id = miv_comm_wide$pond_id # pull pond ID identifier
data.scores$doy = as.factor(miv_comm_wide$doy) # pull doy identifier
data.scores$coupling = miv_comm_wide$coupling # pull treatment identifier 
head(data.scores)

# Plot nMDS can use base R or ggplot, using ggplot here for now # 
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
xx = ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size = 4, aes(colour=coupling)) + # plot points 
  geom_text(x=-0.5,y=0.38, label='Stress = 0.111', size=8) + # place stress value
  theme(axis.text.y = element_text(colour = "black", 
                                   size = 12, 
                                   face = "bold"), # y-axis text
        axis.text.x = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 12), # x-axis text
        legend.text = element_text(size = 12, 
                                   face ="bold", 
                                   colour ="black"), # legend text (doesn't appear in figure)
        legend.position = "none", 
        axis.title.y = element_text(face = "bold", size = 14), # Remove any legend, will build below
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), # axis title 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), # legend title (doesn't appear in figure)
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), # Blank background
        legend.key=element_blank()) + 
  labs(x='NMDS1', shape='BMB Density', y='NMDS2') +
  scale_colour_manual(values = c(low_col, int_col, high_col)) # color gradient representing early to late day of year
windows(height=7,width=6) # Dimensions for Ecosphere figure
xx # plot the figure 

# Ordination by lowest taxa ID #========================
# total density grouped by doy/pond_id/taxa
miv_tot_comm = hort_mivdensity %>%
  group_by(pond_id, doy, coupling, taxa) %>%
  summarise(density = sum(density)) %>%
  ungroup() %>% 
  drop_na()
miv_tot_comm

miv_comm_wide = miv_tot_comm %>%
  pivot_wider(names_from = 'taxa', 
              values_from = 'density') %>% 
  select(!c(trichoptera_case, tadpole))
miv_comm_wide[is.na(miv_comm_wide)] = 0
miv_comm_wide 

# Run an nmds (make shape doy and color treatment) # 
# Run nMDS with full community data # 
# Convert data into a matrix for the nMDS # 
miv_ord_m = as.matrix(miv_comm_wide[,4:17])

# Perform a data transformation to give low weights to 'species' with low biomass and many zeros # 
miv_hell = decostand(miv_ord_m, method = 'hellinger')

# Run nmds # ===================================
# Generally, stress < 0.05 provides an excellent representation in reduced 
# dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a 
# poor representation
miv_nmds = metaMDS(miv_hell, k=2,trymax=100, distance='bray') 
miv_nmds # Stress is 0.11, decent
stressplot(miv_nmds) # Pretty low scatter, good fit  
plot(miv_nmds) # Check it out

ordiplot(miv_nmds, type='n')
orditorp(miv_nmds, display='species') 

# Extract nMDS scores (x and y coordinates, for better plotting)
data.scores = as.data.frame(scores(miv_nmds))

# Add columns to the data frame from original data 
data.scores$pond_id = miv_comm_wide$pond_id # pull pond ID identifier
data.scores$doy = as.factor(miv_comm_wide$doy) # pull doy identifier
data.scores$coupling = miv_comm_wide$coupling # pull treatment identifier 
head(data.scores)

# Plot nMDS can use base R or ggplot, using ggplot here for now # 
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
xx = ggplot(data.scores, aes(x=NMDS1, y=NMDS2)) +
  geom_point(size = 4, aes(colour=coupling)) + # plot points 
  geom_text(x=-0.5,y=0.25, label='Stress = 0.111', size=8) + # place stress value
  theme(axis.text.y = element_text(colour = "black", 
                                   size = 12, 
                                   face = "bold"), # y-axis text
        axis.text.x = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 12), # x-axis text
        legend.text = element_text(size = 12, 
                                   face ="bold", 
                                   colour ="black"), # legend text (doesn't appear in figure)
        legend.position = "none", 
        axis.title.y = element_text(face = "bold", size = 14), # Remove any legend, will build below
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), # axis title 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), # legend title (doesn't appear in figure)
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), # Blank background
        legend.key=element_blank()) + 
  labs(x='NMDS1', shape='BMB Density', y='NMDS2') +
  scale_colour_manual(values = c(low_col, int_col, high_col)) # color gradient representing early to late day of year
windows(height=7,width=6) # Dimensions for Ecosphere figure
xx # plot the figure 

