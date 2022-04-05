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

windows(height=3, width=10)
library(scales)
library(gridExtra)

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

p1 = ggplot(pond_F_pp, aes(x=taxa, y=areal_biomass, fill=pond_id, shape=taxa)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000), name = 'Chlorophyll-a areal concentraion (ug m-2)',
                sec.axis = sec_axis(trans=~./1000, name='Macrophyte Areal Biomass (mg m-2)')) +
  scale_shape_manual(values = c(21,22,23)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, low_col, low_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() +
  annotation_logticks(sides = 'l') + 
  annotation_logticks(sides = 'r') + 
  scale_x_discrete(labels=addline_format(c('Algae (ug/m2)', 'Periphyton (ug/m2)', 'Macrophytes (mg/m2)'))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p1

#Stripchart Version# 
stripchart(Temp~Month,
           data=airquality,
           main="Different strip chart for each month",
           xlab="Months",
           ylab="Temperature",
           col="brown3",
           group.names=c("May","June","July","August","September"),
           vertical=TRUE,
           pch=16)