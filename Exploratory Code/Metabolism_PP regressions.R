# Hort Metabolism 

hort_metabolism_clean

## Separate by grouping - total biomass, cladocerans, copepods, rotifers ## 
gpp = hort_metabolism_clean %>% 
  select(pond_id, doy, GPP)
gpp
max(gpp$GPP) #22
min(gpp$GPP) #0

## GPP by pond## ===========================

# Because we will be plotting by pond, make some separate data frames to make life easier
gppA = gpp %>% #pulse, int
  filter(pond_id == "A") 

gppB = gpp %>% #pulse, low
  filter(pond_id == "B") 

gppC = gpp %>% #pulse, high
  filter(pond_id == "C") 

gppD = gpp %>% #ref, int
  filter(pond_id == "D") 

gppE = gpp %>% #ref, high
  filter(pond_id == "E") 

gppF = gpp %>% #ref, low
  filter(pond_id == "F") 

## Sources of PP ## 
hort_periphy = read_csv('periphy_clean.csv') # periphyton
hort_periphy %>% 
  select(pond_id, collect, biomass_area_cm2) %>%
  rename(doy = collect)

hort_macrophy = read_csv('macrophy_clean.csv') # macrophytes 
hort_macrophy %>% 
  select(pond_id, doy, areal_biomass)

chl = read_csv('hort20_surface_dat.csv') %>% # Algae 
  select(pond_id, doy, chla)
chl
chlA = chl %>% #pulse, int
  filter(pond_id == "A") 

chlB = chl %>% #pulse, low
  filter(pond_id == "B") 

chlC = chl %>% #pulse, high
  filter(pond_id == "C") 

chlD = chl %>% #ref, int
  filter(pond_id == "D") 

chlE = chl %>% #ref, high
  filter(pond_id == "E") 

chlF = chl %>% #ref, low
  filter(pond_id == "F") 

# GPP Regressions # 
gppA
chlA

pp_A = left_join(gppA, chlA, by = c('pond_id', 'doy'))
pp_A

pp_B = left_join(gppB, chlB, by = c('pond_id', 'doy'))
pp_B

pp_C = left_join(gppC, chlC, by = c('pond_id', 'doy'))
pp_C

pp_D = left_join(gppD, chlD, by = c('pond_id', 'doy'))
pp_D

pp_E = left_join(gppE, chlE, by = c('pond_id', 'doy'))
pp_E

pp_F = left_join(gppF, chlF, by = c('pond_id', 'doy'))
pp_F

library('ggpubr')
#Pond A
ggscatter(pp_A, x='chla', y='GPP', 
          add = 'reg.line', conf.int = T,
          cor.coef = T, cor.method = 'pearson', 
          xlab = 'Chlorophyll-a Biomass', ylab = 'GPP')

#Pond B
ggscatter(pp_B, x='chla', y='GPP', 
          add = 'reg.line', conf.int = T,
          cor.coef = T, cor.method = 'pearson', 
          xlab = 'Chlorophyll-a Biomass', ylab = 'GPP')

#Pond C
ggscatter(pp_C, x='chla', y='GPP', 
          add = 'reg.line', conf.int = T,
          cor.coef = T, cor.method = 'pearson', 
          xlab = 'Chlorophyll-a Biomass', ylab = 'GPP')

#Pond D 
ggscatter(pp_D, x='chla', y='GPP', 
          add = 'reg.line', conf.int = T,
          cor.coef = T, cor.method = 'pearson', 
          xlab = 'Chlorophyll-a Biomass', ylab = 'GPP')

#Pond E 
ggscatter(pp_E, x='chla', y='GPP', 
          add = 'reg.line', conf.int = T,
          cor.coef = T, cor.method = 'pearson', 
          xlab = 'Chlorophyll-a Biomass', ylab = 'GPP')

#Pond F
ggscatter(pp_F, x='chla', y='GPP', 
          add = 'reg.line', conf.int = T,
          cor.coef = T, cor.method = 'pearson', 
          xlab = 'Chlorophyll-a Biomass', ylab = 'GPP')
