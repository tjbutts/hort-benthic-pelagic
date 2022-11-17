## 3x3 Array of food web figure ## 

## Primary Producers ##==============================
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

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
peri2 = peri %>% rename(doy = collect) %>% select(pond_id, doy, peri_areal)
chl2 = chl %>% select(pond_id, doy, chl_areal)
macrophy2 = macrophy %>% select(pond_id, doy, macrophy_areal_mg)

join1 = left_join(chl2, peri2, by=c('pond_id', 'doy'))
join1

join2 = left_join(join1, macrophy2, by=c('pond_id', 'doy'))
join2

low_bp = join2 %>%
  filter(pond_id == 'B' | pond_id == 'F') %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass') %>%
  mutate(treatment = case_when(
    pond_id == 'B' ~ 'pulsed', 
    pond_id == 'F' ~ 'reference'
  )) %>%
  mutate(taxa = factor(taxa, c('chl_areal', 'peri_areal', 'macrophy_areal_mg')))
low_bp

int_bp = join2 %>%
  filter(pond_id == 'A' | pond_id == 'D')%>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass' ) %>%
  mutate(treatment = case_when(
    pond_id == 'A' ~ 'pulsed', 
    pond_id == 'D' ~ 'reference'
  ))  %>%
  mutate(taxa = factor(taxa, c('chl_areal', 'peri_areal', 'macrophy_areal_mg')))
int_bp

high_bp = join2 %>% 
  filter(pond_id == 'C' | pond_id == 'E') %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass' ) %>%
  mutate(treatment = case_when(
    pond_id == 'C' ~ 'pulsed', 
    pond_id == 'E' ~ 'reference'
  ))  %>%
  mutate(taxa = factor(taxa, c('chl_areal', 'peri_areal', 'macrophy_areal_mg')))
high_bp

#Plot Figures #

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

# Primary Producers - dual y-axis #=========================
windows(height=3, width=10)
library(scales)
library(gridExtra)

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

p1 = ggplot(low_bp, aes(x=taxa, y=areal_biomass, fill=treatment, alpha=treatment, shape=taxa)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)',
                sec.axis = sec_axis(trans=~./1000, name='Macrophyte Areal Biomass (mg m-2)')) +
  scale_shape_manual(values = c(21,22,23)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, low_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  annotation_logticks(sides = 'r') + 
  scale_x_discrete(labels=addline_format(c('Algae (ug/m2)', 'Periphyton (ug/m2)', 'Macrophytes (mg/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

# Int coupling figure #
p2 = ggplot(int_bp, aes(x=taxa, y=areal_biomass, fill=treatment, alpha=treatment, shape=taxa)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)',
                sec.axis = sec_axis(trans=~./1000, name='Macrophyte Areal Biomass (mg m-2)')) +
  scale_shape_manual(values = c(21,22,23)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(int_col, int_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  annotation_logticks(sides = 'r') + 
  scale_x_discrete(labels=addline_format(c('Algae (ug/m2)', 'Periphyton (ug/m2)', 'Macrophytes (mg/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

# High coupling figure #
p3 = ggplot(high_bp, aes(x=taxa, y=areal_biomass, fill=treatment, alpha=treatment, shape=taxa)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)',
                sec.axis = sec_axis(trans=~./1000, name='Macrophyte Areal Biomass (mg m-2)')) +
  scale_shape_manual(values = c(21,22,23)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(high_col, high_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  annotation_logticks(sides = 'r') + 
  scale_x_discrete(labels=addline_format(c('Algae (ug/m2)', 'Periphyton (ug/m2)', 'Macrophytes (mg/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

grid.arrange(p1,p2,p3, ncol=3, nrow=1)

# Primary Producers - y-axis break #=========================
# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
peri3 = peri %>% rename(doy = collect) %>% mutate(peri_areal_mg = peri_areal/1000) %>% select(pond_id, doy, peri_areal_mg)
chl3 = chl %>% mutate(chl_areal_mg = chl_areal/1000) %>% select(pond_id, doy, chl_areal_mg)
macrophy2 = macrophy %>% select(pond_id, doy, macrophy_areal_mg)

join3 = left_join(chl3, peri3, by=c('pond_id', 'doy'))
join3

join4 = left_join(join1, macrophy2, by=c('pond_id', 'doy'))
join4

low_bp2 = join4 %>%
  filter(pond_id == 'B' | pond_id == 'F') %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal_mg, peri_areal_mg), names_to='taxa', values_to='areal_biomass') %>%
  mutate(treatment = case_when(
    pond_id == 'B' ~ 'pulsed', 
    pond_id == 'F' ~ 'reference'
  )) %>%
  mutate(taxa = factor(taxa, c('chl_areal_mg', 'peri_areal_mg', 'macrophy_areal_mg')))
low_bp2

int_bp2 = join4 %>%
  filter(pond_id == 'A' | pond_id == 'D')%>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal_mg, peri_areal_mg), names_to='taxa', values_to='areal_biomass' ) %>%
  mutate(treatment = case_when(
    pond_id == 'A' ~ 'pulsed', 
    pond_id == 'D' ~ 'reference'
  ))  %>%
  mutate(taxa = factor(taxa, c('chl_areal_mg', 'peri_areal_mg', 'macrophy_areal_mg')))
int_bp2

high_bp2 = join4 %>% 
  filter(pond_id == 'C' | pond_id == 'E') %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal_mg, peri_areal_mg), names_to='taxa', values_to='areal_biomass' ) %>%
  mutate(treatment = case_when(
    pond_id == 'C' ~ 'pulsed', 
    pond_id == 'E' ~ 'reference'
  ))  %>%
  mutate(taxa = factor(taxa, c('chl_areal_mg', 'peri_areal_mg', 'macrophy_areal_mg')))
high_bp2


windows(height=4, width=12)
library(scales)
library(gridExtra)
library(ggbreak) # Xu et al. 2021, Frontiers in Genetics

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

p1.1 = ggplot(low_bp2, aes(x=taxa, y=areal_biomass, fill=treatment, alpha=treatment, shape=taxa)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(0.1, 550000), name = 'Areal Biomass (mg/m2)') +
  scale_y_break(breaks = c(10, 1100)) +
  scale_shape_manual(values = c(21,22,23)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, low_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Algae (chl-a)', 'Periphyton (chl-a)', 'Macrophytes (dry)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

# Int coupling figure #
p2.1 = ggplot(int_bp2, aes(x=taxa, y=areal_biomass, fill=treatment, alpha=treatment, shape=taxa)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(0.1, 550000), name = 'Areal Biomass (mg/m2)') +
  scale_y_break(breaks = c(10, 1100)) +
  scale_shape_manual(values = c(21,22,23)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(int_col, int_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Algae (chl-a)', 'Periphyton (chl-a)', 'Macrophytes (dry)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

# High coupling figure #
p3.1 = ggplot(high_bp2, aes(x=taxa, y=areal_biomass, fill=treatment, alpha=treatment, shape=taxa)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(0.1, 550000), name = 'Areal Biomass (mg/m2)') +
  scale_y_break(breaks = c(10, 1100)) +
  scale_shape_manual(values = c(21,22,23)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(high_col, high_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  scale_x_discrete(labels=addline_format(c('Algae (chl-a)', 'Periphyton (chl-a)', 'Macrophytes (dry)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

grid.arrange(print(p1.1), print(p2.1),print(p3.1), ncol=3, nrow=1)

## Primary Consumers ## =============================

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
  select(pond_id, treatment, doy, gear, tot_dens) %>%
  filter(gear == 'ED') # Stick with Ponar Dredge for now 
miv 

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
zp2 = zp %>% rename(zp_areal_ugm2 = tot_areal) %>% select(pond_id, doy, zp_areal_ugm2)
miv2 = miv %>% rename(miv_areal_nm2 = tot_dens) %>% select(pond_id, doy, miv_areal_nm2)

# Gather each data set then rbind the two together to account for different DOY collections 
zp_gather = zp2 %>% 
  pivot_longer(cols = zp_areal_ugm2, names_to = 'taxa', values_to = 'areal_concentration')
miv_gather = miv2 %>%
  pivot_longer(cols= miv_areal_nm2, names_to = 'taxa', values_to = 'areal_concentration')

# rbind the two separate data frames 
join_pc = rbind(zp_gather, miv_gather)
min(join_pc$areal_concentration) #110 
max(join_pc$areal_concentration) # 542767 

low_bp_pc = join_pc %>%
  filter(pond_id == 'B' | pond_id == 'F') %>% 
  mutate(treatment = case_when(
    pond_id == 'B' ~ 'pulsed', 
    pond_id == 'F' ~ 'reference'
  )) %>%
  mutate(taxa = factor(taxa, c('zp_areal_ugm2', 'miv_areal_nm2')))
low_bp_pc

int_bp_pc = join_pc %>%
  filter(pond_id == 'A' | pond_id == 'D') %>% 
  mutate(treatment = case_when(
    pond_id == 'A' ~ 'pulsed', 
    pond_id == 'D' ~ 'reference'
  )) %>%
  mutate(taxa = factor(taxa, c('zp_areal_ugm2', 'miv_areal_nm2')))
int_bp_pc

high_bp_pc = join_pc %>%
  filter(pond_id == 'C' | pond_id == 'E') %>% 
  mutate(treatment = case_when(
    pond_id == 'C' ~ 'pulsed', 
    pond_id == 'E' ~ 'reference'
  )) %>%
  mutate(taxa = factor(taxa, c('zp_areal_ugm2', 'miv_areal_nm2')))
high_bp_pc

#Plot Figures #

library(scales)
library(gridExtra)

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

p4 = ggplot(low_bp_pc, aes(x=taxa, y=areal_concentration, fill=treatment, alpha=treatment, shape=taxa)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000), name = 'Zooplankton Areal Biomass (ug/m2)',
                sec.axis = dup_axis(name='Benthos Areal Density (#/m2)'))  +
  scale_shape_manual(values = c(21,22)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, low_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  annotation_logticks(sides = 'r') + 
  scale_x_discrete(labels=addline_format(c('Zooplankton (ug/m2)', 'Benthos (#/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

p5 = ggplot(int_bp_pc, aes(x=taxa, y=areal_concentration, fill=treatment, alpha=treatment, shape=taxa)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000), name = 'Zooplankton Areal Biomass (ug/m2)',
                sec.axis = dup_axis(name='Benthos Areal Density (#/m2)'))  +
  scale_shape_manual(values = c(21,22)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(int_col, int_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  annotation_logticks(sides = 'r') + 
  scale_x_discrete(labels=addline_format(c('Zooplankton (ug/m2)', 'Benthos (#/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

p6 = ggplot(high_bp_pc, aes(x=taxa, y=areal_concentration, fill=treatment, alpha=treatment, shape=taxa)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000), name = 'Zooplankton Areal Biomass (ug/m2)',
                sec.axis = dup_axis(name='Benthos Areal Density (#/m2)'))  +
  scale_shape_manual(values = c(21,22)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(high_col, high_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  annotation_logticks(sides = 'r') + 
  scale_x_discrete(labels=addline_format(c('Zooplankton (ug/m2)', 'Benthos (#/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

windows(height=8, width=12)
grid.arrange(p4,p5,p6,print(p1.1),print(p2.1),print(p3.1), ncol=3, nrow=2)
