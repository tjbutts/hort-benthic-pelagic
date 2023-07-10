## Step2.5_Environmental Variables ##

## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022

#============================================#
# STEP 2.5: Average Environmental Variables 
#============================================#

## Data ## 
hort_field

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


