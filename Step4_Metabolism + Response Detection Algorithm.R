## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022

#============================================#
# STEP 4: Ecosystem Metabolism 
#           & Response Detection Algorithm 
#============================================#

# Read in the relevant data and packages
# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

metab

# Assess distribution of missing DOYs between ponds

# GPP # 
doy = as.data.frame(c(145:240)) %>% rename(doy = 'c(145:240)')
dat_y = select(metab, pond_id, doy, GPP) 
# Can use GPP, R, or NEP as the flag removes all data from a DOY if GPP or R is erroneous

dat = as_tibble(merge(gpp_y, doy, by = 'doy', all=TRUE))
dat

windows(width=6, height = 6)
stripchart(doy ~ pond_id, data = dat)
mtext('Metabolism Data', side=3, line=1)

## Separate by Metabolism Parameters ## 
# NEP GAMs #============================
nep = metab %>% 
  select(pond_id, doy, NEP)
nep 

# Because we will be plotting by pond, make some separate data frames to make life easier
nepA = nep %>% #pulse, int
  filter(pond_id == "A") 

nepB = nep %>% #pulse, low
  filter(pond_id == "B") 

nepC = nep %>% #pulse, high
  filter(pond_id == "C") 

nepD = nep %>% #ref, int
  filter(pond_id == "D") 

nepE = nep %>% #ref, high
  filter(pond_id == "E") 

nepF = nep %>% #ref, low
  filter(pond_id == "F") 


# GAM - Intermediate Pulse 
nep_A_gam <- gam(NEP~ s(doy, k = 75),data = nepA, method = 'REML')
summary(nep_A_gam) # Smoothing term significant 
gam.check(nep_A_gam)

# GAM - Intermediate Reference 
nep_D_gam <- gam(NEP~ s(doy, k = 75),data = nepD, method = 'REML')
summary(nep_D_gam) # Smoothing term significant 
gam.check(nep_D_gam)

# GAM - High Pulse 
nep_C_gam <- gam(NEP~ s(doy, k = 75),data = nepC, method = 'REML')
summary(nep_C_gam) # Smoothing term significant 
gam.check(nep_C_gam)

# GAM - High Reference 
nep_E_gam <- gam(NEP~ s(doy, k = 75),data = nepE, method = 'REML')
summary(nep_E_gam) # Smoothing term significant 
gam.check(nep_E_gam)

# GAM - Low Pulse 
nep_B_gam <- gam(NEP~ s(doy, k = 75),data = nepB, method = 'REML')
summary(nep_B_gam) # Smoothing term significant 
gam.check(nep_B_gam)

# GAM - Low Reference 
nep_F_gam <- gam(NEP~ s(doy, k = 75),data = nepF, method = 'REML')
summary(nep_F_gam) # Smoothing term significant 
gam.check(nep_F_gam)

# R GAMs #============================
r = metab %>% 
  select(pond_id, doy, R)
r

# Because we will be plotting by pond, make some separate data frames to make life easier
rA = r %>% #pulse, int
  filter(pond_id == "A") 

rB = r %>% #pulse, low
  filter(pond_id == "B") 

rC = r %>% #pulse, high
  filter(pond_id == "C") 

rD = r %>% #ref, int
  filter(pond_id == "D") 

rE = r %>% #ref, high
  filter(pond_id == "E") 

rF = r %>% #ref, low
  filter(pond_id == "F") 


# GAM - Intermediate Pulse 
r_A_gam <- gam(R~ s(doy, k = 75),data = rA, method = 'REML')
summary(r_A_gam) # Smoothing term significant 
gam.check(r_A_gam)

# GAM - Intermediate Reference 
r_D_gam <- gam(R~ s(doy, k = 75),data = rD, method = 'REML')
summary(r_D_gam) # Smoothing term significant 
gam.check(r_D_gam)

# GAM - High Pulse 
r_C_gam <- gam(R~ s(doy, k = 75),data = rC, method = 'REML')
summary(r_C_gam) # Smoothing term significant 
gam.check(r_C_gam)

# GAM - High Reference 
r_E_gam <- gam(R~ s(doy, k = 90),data = rE, method = 'REML')
summary(r_E_gam) # Smoothing term significant 
gam.check(r_E_gam) # k is significant, does not meet assumption 

# GAM - Low Pulse 
r_B_gam <- gam(R~ s(doy, k = 75),data = rB, method = 'REML')
summary(r_B_gam) # Smoothing term significant 
gam.check(r_B_gam)

# GAM - Low Reference 
r_F_gam <- gam(R~ s(doy, k = 75),data = rF, method = 'REML')
summary(r_F_gam) # Smoothing term significant 
gam.check(r_F_gam)
