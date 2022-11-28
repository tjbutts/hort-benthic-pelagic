## Step2.5_Environmental Variables ##

## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022

#============================================#
# STEP 2.5: Average Environmental Variables 
#============================================#

## Data ## 
hort_field
hort_carbon
hort_macrophy

## Average +- sd of ##===========
##TP ##========== 
tp = hort_field %>%
  select(pond_id, doy, tp, tp_flag) %>% # No tp flags so will discount 
  filter(tp != 'NA')
tp

n = tp %>% filter(pond_id == 'B') 
n

tp_avg = tp %>%
  group_by(pond_id) %>%
  summarize(tp_avg = mean(tp), 
            tp_sd = sd(tp)) %>%
  ungroup()

tp_avg 

##TN ##=============
tn = hort_field %>%
  select(pond_id, doy, tn, tn_flag) %>% # No tn flags so will discount 
  filter(tn != 'NA')
tn

n = tp %>% filter(pond_id == 'A') 
n

tn_avg = tn %>%
  group_by(pond_id) %>%
  summarize(tn_avg = mean(tn), 
            tn_sd = sd(tn)) %>%
  ungroup()

tn_avg

##SRP ##=============
srp = hort_field %>%
  select(pond_id, doy, srp, srp_flag) %>% # Flags indicate value replaced with long-term labortory minimum reporting value  
  filter(srp != 'NA')
srp

n = srp %>% filter(pond_id == 'B') 
n

srp_avg = srp %>%
  group_by(pond_id) %>%
  summarize(srp_avg = mean(srp), 
            srp_sd = sd(srp)) %>%
  ungroup()

srp_avg

##NOX ##=============
nox = hort_field %>%
  select(pond_id, doy, nox, nox_flag) %>% # Flags indicate value replaced with long-term labortory minimum reporting value  
  filter(nox != 'NA')
nox

n = nox %>% filter(pond_id == 'A') 
n

nox_avg = nox %>%
  group_by(pond_id) %>%
  summarize(nox_avg = mean(nox), 
            nox_sd = sd(nox)) %>%
  ungroup()

nox_avg

##nhx ##=============
nhx = hort_field %>%
  select(pond_id, doy, nhx, nhx_flag) %>% # Flags indicate value replaced with long-term labortory minimum reporting value  
  filter(nhx != 'NA')
nhx

n = nhx %>% filter(pond_id == 'A') 
n

nhx_avg = nhx %>%
  group_by(pond_id) %>%
  summarize(nhx_avg = mean(nhx), 
            nhx_sd = sd(nhx)) %>%
  ungroup()

nhx_avg 

##DOC_mgL ##=============
DOC_mgL = hort_carbon %>%
  select(pond_id, doy, DOC_mgL) %>% 
  filter(DOC_mgL != 'NA')
DOC_mgL

n = DOC_mgL %>% filter(pond_id == 'A') 
n

DOC_mgL_avg = DOC_mgL %>%
  group_by(pond_id) %>%
  summarize(DOC_mgL_avg = mean(DOC_mgL), 
            DOC_mgL_sd = sd(DOC_mgL)) %>%
  ungroup()

DOC_mgL_avg 

## Macrophyte dry biomass ## 

mac = hort_macrophy %>% 
  select(pond_id, doy, areal_biomass) %>% 
  filter(areal_biomass != 'NA') %>% 
  filter(doy == 171 | doy == 213) %>% # day of years where all ponds were sampled 
  group_by(pond_id, doy) %>%
  summarize(mean = mean(areal_biomass), 
            sd = sd(areal_biomass)) %>%
  ungroup()
mac

mac2 = mac %>%
  group_by(pond_id) %>%
  summarize(mean = mean(mean), 
            sd = sd(sd)) %>%
  ungroup()
mac2  
