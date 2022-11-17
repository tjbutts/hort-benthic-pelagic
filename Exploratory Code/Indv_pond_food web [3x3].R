# Food Webs in Individual Hort Farm Ponds # 

# Required Libraries for analysis and visualization
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(magrittr)) install.packages('magrittr')
library(magrittr)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate) 
if (!require(readr)) install.packages('readr')
library(readr)
# Visualization
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2) 
if (!require(scales)) install.packages('scales')
library(scales) 
if (!require(gridExtra)) install.packages('gridExtra')
library(gridExtra)

# Trophic Levels #=====================
## Primary Producers ##

# Periphyton # 
## Already an areal estimate ## 
hort_periphy # ug/cm^2 get to m^2 
peri = hort_periphy %>% 
  rename(peri_areal = biomass_area_m2) %>%
  select(pond_id, collect, peri_areal) # (ug/m^2)
peri

## Look at in a time series ## 
#set up plot - use collect date as the DOY # 

# Chlorophyll-a # 
## Take the ug/L estimate and convert to ug/m^2 ## 
## Convert ug/L to ug/m^3 then multiply by sample depth to get m^2 ## 
## Data are gapfilled data between 10-30 cm sample depth, use 0.20 m for multiplier ## 
chl = hort_field %>%
  select(pond_id, doy, chla) %>%
  mutate(chl_areal = (1000*chla*0.20)) # ug/m^2
chl

# Macrophytes # 
## convert the g/m^2 to mg/m^2 ## 
macrophy = hort_macrophy %>% 
  select(pond_id, doy, areal_biomass) %>% 
  rename(macrophy_areal_g = areal_biomass) %>% # g/m^2 
  mutate(macrophy_areal_mg = macrophy_areal_g*1000) # mg/m^2
macrophy # areal biomass is g/m^2
macrophy[macrophy == 0] <- NA

## Primary Consumers ## 

# Zooplankton # 
## Take the ug/L estimate and convert to ug/m^2 ## 
## Convert ug/L to ug/m^3 then multiply by tow depth to get m^2 
zp = hort_zoop %>%
  group_by(pond_id, doy, treatment) %>% 
  summarise(tot = sum(biomass)) %>% 
  ungroup() %>%
  select(pond_id,treatment, doy, tot) %>%
  mutate(tot_areal = (1000*tot*1))
zp  

# Macroinvertebrates # 
## Already an areal estimate - probably need a second axis since MIVs are density estimates 
miv = hort_mivdensity %>% 
  group_by(pond_id, treatment, doy, gear) %>%
  summarise(tot_dens = sum(density)) %>%
  ungroup() %>% 
  select(pond_id, treatment, doy, gear, tot_dens)
miv 

# 3x3 Array #==========================

# plotting # 
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

# Pond F #=============================
peri_F = peri %>% filter(pond_id == 'F')
chl_F = chl %>% filter(pond_id == 'F')
macrophy_F = macrophy %>% filter(pond_id == 'F')
zp_F = zp %>% filter(pond_id == 'F')
miv_F = miv %>% filter(pond_id == 'F')

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
peri_F2 = peri_F %>% rename(doy = collect) %>% select(pond_id, doy, peri_areal)
chl_F2 = chl_F %>% select(pond_id, doy, chl_areal)
macrophy_F2 = macrophy_F %>% select(pond_id, doy, macrophy_areal_mg)

join_F1 = left_join(chl_F2, peri_F2, by=c('pond_id', 'doy'))
join_F1

join_F2 = left_join(join_F1, macrophy_F2, by=c('pond_id', 'doy'))
join_F2

# Combine primary producers into a data frame # 
pond_F_pp = join_F2 %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass') %>%
  mutate(taxa = factor(taxa, c('chl_areal', 'peri_areal', 'macrophy_areal_mg')))
pond_F_pp


#Plotting 
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

pond_F_pp_algae = pond_F_pp %>% filter(taxa == 'chl_areal')
median(pond_F_pp_algae$areal_biomass) #507.54
p1 = ggplot(pond_F_pp_algae, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=low_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Algae (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p1

pond_F_pp_peri = pond_F_pp %>% filter(taxa == 'peri_areal')
median(pond_F_pp_peri$areal_biomass, na.rm = T) #1069.89
p2 = ggplot(pond_F_pp_peri, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=low_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Periphyton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p2

pond_F_pp_mac = pond_F_pp %>% filter(taxa == 'macrophy_areal_mg')
median(pond_F_pp_mac$areal_biomass, na.rm = T) #67250
p3 = ggplot(pond_F_pp_mac, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'dry biomass (mg m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=low_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macrophytes (mg/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p3

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
zp_F2 = zp_F %>% rename(zp_areal_ugm2 = tot_areal) %>% select(pond_id, doy, zp_areal_ugm2)
miv_F2 = miv_F %>% rename(miv_areal_nm2 = tot_dens) %>% select(pond_id, doy, miv_areal_nm2)

# Gather each data set then rbind the two together to account for different DOY collections 
zp_gather_F = zp_F2 %>% 
  pivot_longer(cols = zp_areal_ugm2, names_to = 'taxa', values_to = 'areal_concentration')
miv_gather_F = miv_F2 %>%
  pivot_longer(cols= miv_areal_nm2, names_to = 'taxa', values_to = 'areal_concentration')

# rbind the two separate data frames 
join_pc_F = rbind(zp_gather_F, miv_gather_F) 
pond_F_pc = join_pc_F %>% mutate(taxa = factor(taxa, c('zp_areal_ugm2', 'miv_areal_nm2')))
#min = 110 
#max = 542767 

pond_F_pc_zp = pond_F_pc %>% filter(taxa == 'zp_areal_ugm2')
median(pond_F_pc_zp$areal_concentration, na.rm = T) #18013.68
p4 = ggplot(pond_F_pc_zp, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Zooplankton areal biomass (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=low_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Zooplankton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p4

pond_F_pc_miv = pond_F_pc %>% filter(taxa == 'miv_areal_nm2')
median(pond_F_pc_miv$areal_concentration, na.rm = T) #2500
p5 = ggplot(pond_F_pc_miv, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'macroinvertebrte density (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=low_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macroinvertebrates (#/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p5

windows(height=4,width=12)
grid.arrange(p1,p2,p3, ncol=3, nrow=1)

windows(height=4, width=8)
grid.arrange(p4,p5, ncol=2, nrow=1)

median(pond_F_pp_algae$areal_biomass) #507.54
median(pond_F_pp_peri$areal_biomass, na.rm = T) #1069.89
median(pond_F_pp_mac$areal_biomass, na.rm = T) #67250
median(pond_F_pc_zp$areal_concentration, na.rm = T) #18013.68
median(pond_F_pc_miv$areal_concentration, na.rm = T) #2500

# Pond B #=============================
peri_B = peri %>% filter(pond_id == 'B')
chl_B = chl %>% filter(pond_id == 'B')
macrophy_B = macrophy %>% filter(pond_id == 'B')
zp_B = zp %>% filter(pond_id == 'B')
miv_B = miv %>% filter(pond_id == 'B')

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
peri_B2 = peri_B %>% rename(doy = collect) %>% select(pond_id, doy, peri_areal)
chl_B2 = chl_B %>% select(pond_id, doy, chl_areal)
macrophy_B2 = macrophy_B %>% select(pond_id, doy, macrophy_areal_mg)

join_B1 = left_join(chl_B2, peri_B2, by=c('pond_id', 'doy'))
join_B1

join_B2 = left_join(join_B1, macrophy_B2, by=c('pond_id', 'doy'))
join_B2

# Combine primary producers into a data frame # 
pond_B_pp = join_B2 %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass') %>%
  mutate(taxa = factor(taxa, c('chl_areal', 'peri_areal', 'macrophy_areal_mg')))
pond_B_pp

# Plotting # 
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

pond_B_pp_algae = pond_B_pp %>% filter(taxa == 'chl_areal')
p1 = ggplot(pond_B_pp_algae, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=low_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Algae (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p1

pond_B_pp_peri = pond_B_pp %>% filter(taxa == 'peri_areal')
p2 = ggplot(pond_B_pp_peri, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=low_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Periphyton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p2

pond_B_pp_mac = pond_B_pp %>% filter(taxa == 'macrophy_areal_mg')
p3 = ggplot(pond_B_pp_mac, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'dry biomass (mg m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=low_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macrophytes (mg/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p3

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
zp_B2 = zp_B %>% rename(zp_areal_ugm2 = tot_areal) %>% select(pond_id, doy, zp_areal_ugm2)
miv_B2 = miv_B %>% rename(miv_areal_nm2 = tot_dens) %>% select(pond_id, doy, miv_areal_nm2)

# Gather each data set then rbind the two together to account for different DOY collections 
zp_gather_B = zp_B2 %>% 
  pivot_longer(cols = zp_areal_ugm2, names_to = 'taxa', values_to = 'areal_concentration')
miv_gather_B = miv_B2 %>%
  pivot_longer(cols= miv_areal_nm2, names_to = 'taxa', values_to = 'areal_concentration')

# rbind the two separate data frames 
join_pc_B = rbind(zp_gather_B, miv_gather_B) 
pond_B_pc = join_pc_B %>% mutate(taxa = factor(taxa, c('zp_areal_ugm2', 'miv_areal_nm2')))
#min = 110 
#max = 542767 

pond_B_pc_zp = pond_B_pc %>% filter(taxa == 'zp_areal_ugm2')
p4 = ggplot(pond_B_pc_zp, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Zooplankton areal biomass (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=low_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Zooplankton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p4

pond_B_pc_miv = pond_B_pc %>% filter(taxa == 'miv_areal_nm2')
p5 = ggplot(pond_B_pc_miv, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Macroinvertebrte density (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=low_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macroinvertebrates (#/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p5

windows(height=4,width=12)
grid.arrange(p1,p2,p3, ncol=3, nrow=1)

windows(height=4, width=8)
grid.arrange(p4,p5, ncol=2, nrow=1)

median(pond_B_pp_algae$areal_biomass, na.rm = T) #654.133
median(pond_B_pp_peri$areal_biomass, na.rm = T) #824.36
median(pond_B_pp_mac$areal_biomass, na.rm = T) #64250
median(pond_B_pc_zp$areal_concentration, na.rm = T) #2836.27
median(pond_B_pc_miv$areal_concentration, na.rm = T) #3017.24

# Pond C #=============================
peri_C = peri %>% filter(pond_id == 'C')
chl_C = chl %>% filter(pond_id == 'C')
macrophy_C = macrophy %>% filter(pond_id == 'C')
zp_C = zp %>% filter(pond_id == 'C')
miv_C = miv %>% filter(pond_id == 'C')

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
peri_C2 = peri_C %>% rename(doy = collect) %>% select(pond_id, doy, peri_areal)
chl_C2 = chl_C %>% select(pond_id, doy, chl_areal)
macrophy_C2 = macrophy_C %>% select(pond_id, doy, macrophy_areal_mg)

join_C1 = left_join(chl_C2, peri_C2, by=c('pond_id', 'doy'))
join_C1

join_C2 = left_join(join_C1, macrophy_C2, by=c('pond_id', 'doy'))
join_C2

# Combine primary producers into a data frame # 
pond_C_pp = join_C2 %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass') %>%
  mutate(taxa = factor(taxa, c('chl_areal', 'peri_areal', 'macrophy_areal_mg')))
pond_C_pp

# Plotting # 
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

pond_C_pp_algae = pond_C_pp %>% filter(taxa == 'chl_areal')
p1 = ggplot(pond_C_pp_algae, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=high_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Algae (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p1

pond_C_pp_peri = pond_C_pp %>% filter(taxa == 'peri_areal')
p2 = ggplot(pond_C_pp_peri, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=high_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Periphyton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p2

pond_C_pp_mac = pond_C_pp %>% filter(taxa == 'macrophy_areal_mg')
p3 = ggplot(pond_C_pp_mac, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'dry biomass (mg m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=high_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macrophytes (mg/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p3

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
zp_C2 = zp_C %>% rename(zp_areal_ugm2 = tot_areal) %>% select(pond_id, doy, zp_areal_ugm2)
miv_C2 = miv_C %>% rename(miv_areal_nm2 = tot_dens) %>% select(pond_id, doy, miv_areal_nm2)

# Gather each data set then rbind the two together to account for different DOY collections 
zp_gather_C = zp_C2 %>% 
  pivot_longer(cols = zp_areal_ugm2, names_to = 'taxa', values_to = 'areal_concentration')
miv_gather_C = miv_C2 %>%
  pivot_longer(cols= miv_areal_nm2, names_to = 'taxa', values_to = 'areal_concentration')

# rbind the two separate data frames 
join_pc_C = rbind(zp_gather_C, miv_gather_C) 
pond_C_pc = join_pc_C %>% mutate(taxa = factor(taxa, c('zp_areal_ugm2', 'miv_areal_nm2')))
#min = 110 
#max = 542767 

pond_C_pc_zp = pond_C_pc %>% filter(taxa == 'zp_areal_ugm2')
p4 = ggplot(pond_C_pc_zp, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Zooplankton areal biomass (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=high_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Zooplankton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p4

pond_C_pc_miv = pond_C_pc %>% filter(taxa == 'miv_areal_nm2')
p5 = ggplot(pond_C_pc_miv, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Macroinvertebrte density (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=high_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macroinvertebrates (#/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p5

windows(height=4,width=12)
grid.arrange(p1,p2,p3, ncol=3, nrow=1)

windows(height=4, width=8)
grid.arrange(p4,p5, ncol=2, nrow=1)

median(pond_C_pp_algae$areal_biomass, na.rm = T) #349.08
median(pond_C_pp_peri$areal_biomass, na.rm = T) #1061.83
median(pond_C_pp_mac$areal_biomass, na.rm = T) #76250
median(pond_C_pc_zp$areal_concentration, na.rm = T) #30686.48
median(pond_C_pc_miv$areal_concentration, na.rm = T) #5086.21

# Pond E #=============================
peri_E = peri %>% filter(pond_id == 'E')
chl_E = chl %>% filter(pond_id == 'E')
macrophy_E = macrophy %>% filter(pond_id == 'E')
zp_E = zp %>% filter(pond_id == 'E')
miv_E = miv %>% filter(pond_id == 'E')

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
peri_E2 = peri_E %>% rename(doy = collect) %>% select(pond_id, doy, peri_areal)
chl_E2 = chl_E %>% select(pond_id, doy, chl_areal)
macrophy_E2 = macrophy_E %>% select(pond_id, doy, macrophy_areal_mg)

join_E1 = left_join(chl_E2, peri_E2, by=c('pond_id', 'doy'))
join_E1

join_E2 = left_join(join_E1, macrophy_E2, by=c('pond_id', 'doy'))
join_E2

# Combine primary producers into a data frame # 
pond_E_pp = join_E2 %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass') %>%
  mutate(taxa = factor(taxa, c('chl_areal', 'peri_areal', 'macrophy_areal_mg')))
pond_E_pp

# Plotting # 
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

pond_E_pp_algae = pond_E_pp %>% filter(taxa == 'chl_areal')
p1 = ggplot(pond_E_pp_algae, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=high_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Algae (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p1

pond_E_pp_peri = pond_E_pp %>% filter(taxa == 'peri_areal')
p2 = ggplot(pond_E_pp_peri, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=high_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Periphyton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p2

pond_E_pp_mac = pond_E_pp %>% filter(taxa == 'macrophy_areal_mg')
p3 = ggplot(pond_E_pp_mac, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'dry biomass (mg m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=high_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macrophytes (mg/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p3

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
zp_E2 = zp_E %>% rename(zp_areal_ugm2 = tot_areal) %>% select(pond_id, doy, zp_areal_ugm2)
miv_E2 = miv_E %>% rename(miv_areal_nm2 = tot_dens) %>% select(pond_id, doy, miv_areal_nm2)

# Gather each data set then rbind the two together to account for different DOY collections 
zp_gather_E = zp_E2 %>% 
  pivot_longer(cols = zp_areal_ugm2, names_to = 'taxa', values_to = 'areal_concentration')
miv_gather_E = miv_E2 %>%
  pivot_longer(cols= miv_areal_nm2, names_to = 'taxa', values_to = 'areal_concentration')

# rbind the two separate data frames 
join_pc_E = rbind(zp_gather_E, miv_gather_E) 
pond_E_pc = join_pc_E %>% mutate(taxa = factor(taxa, c('zp_areal_ugm2', 'miv_areal_nm2')))
#min = 110 
#max = 542767 

pond_E_pc_zp = pond_E_pc %>% filter(taxa == 'zp_areal_ugm2')
p4 = ggplot(pond_E_pc_zp, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Zooplankton areal biomass (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=high_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Zooplankton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p4

pond_E_pc_miv = pond_E_pc %>% filter(taxa == 'miv_areal_nm2')
p5 = ggplot(pond_E_pc_miv, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Macroinvertebrte density (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=high_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macroinvertebrates (#/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p5

windows(height=4,width=12)
grid.arrange(p1,p2,p3, ncol=3, nrow=1)

windows(height=4, width=8)
grid.arrange(p4,p5, ncol=2, nrow=1)

median(pond_E_pp_algae$areal_biomass, na.rm = T) #491.59
median(pond_E_pp_peri$areal_biomass, na.rm = T) #1280.70
median(pond_E_pp_mac$areal_biomass, na.rm = T) #94875
median(pond_E_pc_zp$areal_concentration, na.rm = T) #11054.02
median(pond_E_pc_miv$areal_concentration, na.rm = T) #1950.70

# Pond D #=============================
peri_D = peri %>% filter(pond_id == 'D')
chl_D = chl %>% filter(pond_id == 'D')
macrophy_D = macrophy %>% filter(pond_id == 'D')
zp_D = zp %>% filter(pond_id == 'D')
miv_D = miv %>% filter(pond_id == 'D')

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
peri_D2 = peri_D %>% rename(doy = collect) %>% select(pond_id, doy, peri_areal)
chl_D2 = chl_D %>% select(pond_id, doy, chl_areal)
macrophy_D2 = macrophy_D %>% select(pond_id, doy, macrophy_areal_mg)

join_D1 = left_join(chl_D2, peri_D2, by=c('pond_id', 'doy'))
join_D1

join_D2 = left_join(join_D1, macrophy_D2, by=c('pond_id', 'doy'))
join_D2

# Combine primary producers into a data frame # 
pond_D_pp = join_D2 %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass') %>%
  mutate(taxa = factor(taxa, c('chl_areal', 'peri_areal', 'macrophy_areal_mg')))
pond_D_pp

# Plotting # 
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

pond_D_pp_algae = pond_D_pp %>% filter(taxa == 'chl_areal')
p1 = ggplot(pond_D_pp_algae, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=int_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Algae (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p1

pond_D_pp_peri = pond_D_pp %>% filter(taxa == 'peri_areal')
p2 = ggplot(pond_D_pp_peri, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=int_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Periphyton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p2

pond_D_pp_mac = pond_D_pp %>% filter(taxa == 'macrophy_areal_mg')
p3 = ggplot(pond_D_pp_mac, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'dry biomass (mg m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=int_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macrophytes (mg/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p3

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
zp_D2 = zp_D %>% rename(zp_areal_ugm2 = tot_areal) %>% select(pond_id, doy, zp_areal_ugm2)
miv_D2 = miv_D %>% rename(miv_areal_nm2 = tot_dens) %>% select(pond_id, doy, miv_areal_nm2)

# Gather each data set then rbind the two together to account for different DOY collections 
zp_gather_D = zp_D2 %>% 
  pivot_longer(cols = zp_areal_ugm2, names_to = 'taxa', values_to = 'areal_concentration')
miv_gather_D = miv_D2 %>%
  pivot_longer(cols= miv_areal_nm2, names_to = 'taxa', values_to = 'areal_concentration')

# rbind the two separate data frames 
join_pc_D = rbind(zp_gather_D, miv_gather_D) 
pond_D_pc = join_pc_D %>% mutate(taxa = factor(taxa, c('zp_areal_ugm2', 'miv_areal_nm2')))
#min = 110 
#max = 542767 

pond_D_pc_zp = pond_D_pc %>% filter(taxa == 'zp_areal_ugm2')
p4 = ggplot(pond_D_pc_zp, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Zooplankton areal biomass (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=int_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Zooplankton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p4

pond_D_pc_miv = pond_D_pc %>% filter(taxa == 'miv_areal_nm2')
p5 = ggplot(pond_D_pc_miv, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Macroinvertebrte density (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=int_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macroinvertebrates (#/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p5

windows(height=4,width=12)
grid.arrange(p1,p2,p3, ncol=3, nrow=1)

windows(height=4, width=8)
grid.arrange(p4,p5, ncol=2, nrow=1)

median(pond_D_pp_algae$areal_biomass, na.rm = T) #407.3
median(pond_D_pp_peri$areal_biomass, na.rm = T) #2244.29
median(pond_D_pp_mac$areal_biomass, na.rm = T) #127375
median(pond_D_pc_zp$areal_concentration, na.rm = T) #42013.29
median(pond_D_pc_miv$areal_concentration, na.rm = T) #4439.66

# Pond A #=============================
peri_A = peri %>% filter(pond_id == 'A')
chl_A = chl %>% filter(pond_id == 'A')
macrophy_A = macrophy %>% filter(pond_id == 'A')
zp_A = zp %>% filter(pond_id == 'A')
miv_A = miv %>% filter(pond_id == 'A')

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
peri_A2 = peri_A %>% rename(doy = collect) %>% select(pond_id, doy, peri_areal)
chl_A2 = chl_A %>% select(pond_id, doy, chl_areal)
macrophy_A2 = macrophy_A %>% select(pond_id, doy, macrophy_areal_mg)

join_A1 = left_join(chl_A2, peri_A2, by=c('pond_id', 'doy'))
join_A1

join_A2 = left_join(join_A1, macrophy_A2, by=c('pond_id', 'doy'))
join_A2

# Combine primary producers into a data frame # 
pond_A_pp = join_A2 %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass') %>%
  mutate(taxa = factor(taxa, c('chl_areal', 'peri_areal', 'macrophy_areal_mg')))
pond_A_pp

# Plotting # 
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

pond_A_pp_algae = pond_A_pp %>% filter(taxa == 'chl_areal')
p1 = ggplot(pond_A_pp_algae, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=int_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_Aiscrete(labels=addline_format(c('Algae (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p1

pond_A_pp_peri = pond_A_pp %>% filter(taxa == 'peri_areal')
p2 = ggplot(pond_A_pp_peri, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=int_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Periphyton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p2

pond_A_pp_mac = pond_A_pp %>% filter(taxa == 'macrophy_areal_mg')
p3 = ggplot(pond_A_pp_mac, aes(x=taxa, y=areal_biomass)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'dry biomass (mg m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=int_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macrophytes (mg/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p3

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
zp_A2 = zp_A %>% rename(zp_areal_ugm2 = tot_areal) %>% select(pond_id, doy, zp_areal_ugm2)
miv_A2 = miv_A %>% rename(miv_areal_nm2 = tot_dens) %>% select(pond_id, doy, miv_areal_nm2)

# Gather each data set then rbind the two together to account for different DOY collections 
zp_gather_A = zp_A2 %>% 
  pivot_longer(cols = zp_areal_ugm2, names_to = 'taxa', values_to = 'areal_concentration')
miv_gather_A = miv_A2 %>%
  pivot_longer(cols= miv_areal_nm2, names_to = 'taxa', values_to = 'areal_concentration')

# rbind the two separate data frames 
join_pc_A = rbind(zp_gather_A, miv_gather_A) 
pond_A_pc = join_pc_A %>% mutate(taxa = factor(taxa, c('zp_areal_ugm2', 'miv_areal_nm2')))
#min = 110 
#max = 542767 

pond_A_pc_zp = pond_A_pc %>% filter(taxa == 'zp_areal_ugm2')
p4 = ggplot(pond_A_pc_zp, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Zooplankton areal biomass (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=int_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Zooplankton (ug/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p4

pond_A_pc_miv = pond_A_pc %>% filter(taxa == 'miv_areal_nm2')
p5 = ggplot(pond_A_pc_miv, aes(x=taxa, y=areal_concentration)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 550000), name = 'Macroinvertebrte density (ug m-2)') +
  geom_point(position = position_jitter(0.1), cex=2) + 
  stat_summary(fun=median, geom='point', shape=19,size=8, color=int_col) +
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Macroinvertebrates (#/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p5

windows(height=4,width=12)
grid.arrange(p1,p2,p3, ncol=3, nrow=1)

windows(height=4, width=8)
grid.arrange(p4,p5, ncol=2, nrow=1)

median(pond_A_pp_algae$areal_biomass, na.rm = T) #1487.98
median(pond_A_pp_peri$areal_biomass, na.rm = T) #1722.26
median(pond_A_pp_mac$areal_biomass, na.rm = T) #73250
median(pond_A_pc_zp$areal_concentration, na.rm = T) #52505.36
median(pond_A_pc_miv$areal_concentration, na.rm = T) #1915.493
