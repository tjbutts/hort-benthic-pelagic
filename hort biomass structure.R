# Food web Structure analysis - part 2 # 

# Mean & Median Trophic Level Biomasses over whole experiment #=================
# 1st Trophic Level # 
## Macrophytes 
macrophy_summary_pond= hort_macrophy %>% # g/m^2 
  select(pond_id, doy, areal_biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(pond_id) %>%
  summarize(mean = mean(areal_biomass, na.rm=T),
            median = median(areal_biomass, na.rm=T),
            sd = sd(areal_biomass, na.rm=T)) %>%
  ungroup()
macrophy_summary_pond 

macrophy_summary_coupling = hort_macrophy %>% # g/m^2 
  select(pond_id, doy, areal_biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(coupling) %>%
  summarize(mean = mean(areal_biomass, na.rm=T),
            median = median(areal_biomass, na.rm=T),
            sd = sd(areal_biomass, na.rm=T)) %>%
  ungroup()
macrophy_summary_coupling

## Periphyton
hort_periphy
periphy_summary_pond= hort_periphy %>% # ug/cm^2
  select(pond_id, collect, biomass_area_m2) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(pond_id) %>%
  summarize(mean = mean(biomass_area_m2, na.rm=T),
            median = median(biomass_area_m2, na.rm=T),
            sd = sd(biomass_area_m2, na.rm=T)) %>%
  ungroup()
periphy_summary_pond 

periphy_summary_coupling = hort_periphy %>% # g/m^2 
  select(pond_id, collect, biomass_area_m2) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(coupling) %>%
  summarize(mean = mean(biomass_area_m2, na.rm=T),
            median = median(biomass_area_m2, na.rm=T),
            sd = sd(biomass_area_m2, na.rm=T)) %>%
  ungroup()
periphy_summary_coupling

## Algae
hort_field
chl_summary_pond= hort_field %>% # ug/L
  select(pond_id, doy, chla) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(pond_id) %>%
  summarize(mean = mean(chla, na.rm=T),
            median = median(chla, na.rm=T),
            sd = sd(chla, na.rm=T)) %>%
  ungroup()
chl_summary_pond 

chl_summary_coupling = hort_field %>% # ug/L 
  select(pond_id, doy, chla) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(coupling) %>%
  summarize(mean = mean(chla, na.rm=T),
            median = median(chla, na.rm=T),
            sd = sd(chla, na.rm=T)) %>%
  ungroup()
chl_summary_coupling 

# 2nd Trophic Level # 
hort_zoop 
zoop_summary_pond = hort_zoop %>% #ug/L 
  select(pond_id, doy, group, taxon, biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>% 
  group_by(pond_id, doy) %>%
  summarise(tot = sum(biomass)) %>%
  ungroup() %>%
  group_by(pond_id) %>%
  summarise(mean = mean (tot, na.rm=T),
            median = median(tot, na.rm=T),
            sd = sd(tot, na.rm=T)) %>%
  ungroup() 
zoop_summary_pond

zoop_summary_coupling = hort_zoop %>% #ug/L 
  select(pond_id, doy, group, taxon, biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>% 
  group_by(pond_id, doy, coupling) %>%
  summarise(tot = sum(biomass)) %>%
  ungroup() %>%
  group_by(coupling) %>%
  summarise(mean = mean (tot, na.rm=T),
            median = median(tot, na.rm=T),
            sd = sd(tot, na.rm=T)) %>%
  ungroup() 
zoop_summary_coupling

# Macroinvertebrates 

# Plotting # 
# By Pond # 
macrophy_summary_coupling = as.data.frame(macrophy_summary_coupling)
periphy_summary_pond
zoop_summary_pond

boxplot(median~coupling, data=macrophy_summary_coupling)

# Mean & Median Trophic Level Biomasses pre-first pulse ##=============================
# 1st Trophic Level # 
## Macrophytes 
macrophy_summary_pond2= hort_macrophy %>% # g/m^2 
  select(pond_id, doy, areal_biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>% 
  filter(doy < 176) %>%
  group_by(pond_id) %>%
  summarize(mean = mean(areal_biomass, na.rm=T),
            median = median(areal_biomass, na.rm=T),
            sd = sd(areal_biomass, na.rm=T)) %>%
  ungroup()
macrophy_summary_pond2 

macrophy_summary_coupling2 = hort_macrophy %>% # g/m^2 
  filter(doy < 176) %>%
  select(pond_id, doy, areal_biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(coupling) %>%
  summarize(mean = mean(areal_biomass, na.rm=T),
            median = median(areal_biomass, na.rm=T),
            sd = sd(areal_biomass, na.rm=T)) %>%
  ungroup()
macrophy_summary_coupling2

## Periphyton
hort_periphy
periphy_summary_pond2= hort_periphy %>% # ug/cm^2
  select(pond_id, collect, biomass_area_cm2) %>%
  filter(collect < 176) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(pond_id) %>%
  summarize(mean = mean(biomass_area_cm2, na.rm=T),
            median = median(biomass_area_cm2, na.rm=T),
            sd = sd(biomass_area_cm2, na.rm=T)) %>%
  ungroup()
periphy_summary_pond2 

periphy_summary_coupling2 = hort_periphy %>% # ug/cm^2
  filter(collect < 176) %>%
  select(pond_id, collect, biomass_area_cm2) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(coupling) %>%
  summarize(mean = mean(biomass_area_cm2, na.rm=T),
            median = median(biomass_area_cm2, na.rm=T),
            sd = sd(biomass_area_cm2, na.rm=T)) %>%
  ungroup()
periphy_summary_coupling2

## Algae
hort_field
chl_summary_pond2= hort_field %>% # ug/L
  filter(doy < 176) %>%
  select(pond_id, doy, chla) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(pond_id) %>%
  summarize(mean = mean(chla, na.rm=T),
            median = median(chla, na.rm=T),
            sd = sd(chla, na.rm=T)) %>%
  ungroup()
chl_summary_pond2 

chl_summary_coupling2 = hort_field %>% # ug/L 
  filter(doy < 176) %>%
  select(pond_id, doy, chla) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(coupling) %>%
  summarize(mean = mean(chla, na.rm=T),
            median = median(chla, na.rm=T),
            sd = sd(chla, na.rm=T)) %>%
  ungroup()
chl_summary_coupling2 

# 2nd Trophic Level # 
hort_zoop 
zoop_summary_pond2 = hort_zoop %>% #ug/L 
  filter(doy < 176) %>%
  select(pond_id, doy, group, taxon, biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>% 
  group_by(pond_id, doy) %>%
  summarise(tot = sum(biomass)) %>%
  ungroup() %>%
  group_by(pond_id) %>%
  summarise(mean = mean (tot, na.rm=T),
            median = median(tot, na.rm=T),
            sd = sd(tot, na.rm=T)) %>%
  ungroup() 
zoop_summary_pond2

zoop_summary_coupling2 = hort_zoop %>% #ug/L 
  filter(doy < 176) %>%
  select(pond_id, doy, group, taxon, biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>% 
  group_by(pond_id, doy, coupling) %>%
  summarise(tot = sum(biomass)) %>%
  ungroup() %>%
  group_by(coupling) %>%
  summarise(mean = mean (tot, na.rm=T),
            median = median(tot, na.rm=T),
            sd = sd(tot, na.rm=T)) %>%
  ungroup() 
zoop_summary_coupling2

# Macroinvertebrates 

# Mean & Median Trophic Level Biomasses post-first, pre-second pulse #===========================
# 1st Trophic Level # 
## Macrophytes 
macrophy_summary_pond3= hort_macrophy %>% # g/m^2 
  select(pond_id, doy, areal_biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>% 
  filter(doy > 176 & doy < 215) %>% # went to 215 here as macrophytes were sampled post second pulse 
  group_by(pond_id) %>%
  summarize(mean = mean(areal_biomass, na.rm=T),
            median = median(areal_biomass, na.rm=T),
            sd = sd(areal_biomass, na.rm=T)) %>%
  ungroup()
macrophy_summary_pond3 

macrophy_summary_coupling3 = hort_macrophy %>% # g/m^2 
  filter(doy < 176 & doy < 215) %>% # went to 215 here as macrophytes were sampled post second pulse 
  select(pond_id, doy, areal_biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(coupling) %>%
  summarize(mean = mean(areal_biomass, na.rm=T),
            median = median(areal_biomass, na.rm=T),
            sd = sd(areal_biomass, na.rm=T)) %>%
  ungroup()
macrophy_summary_coupling3

## Periphyton
hort_periphy
periphy_summary_pond3= hort_periphy %>% # ug/cm^2
  select(pond_id, collect, biomass_area_cm2) %>%
  filter(collect > 176 & collect < 211) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(pond_id) %>%
  summarize(mean = mean(biomass_area_cm2, na.rm=T),
            median = median(biomass_area_cm2, na.rm=T),
            sd = sd(biomass_area_cm2, na.rm=T)) %>%
  ungroup()
periphy_summary_pond3 

periphy_summary_coupling3 = hort_periphy %>% # ug/cm^2
  filter(collect > 176 & collect < 211) %>%
  select(pond_id, collect, biomass_area_cm2) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(coupling) %>%
  summarize(mean = mean(biomass_area_cm2, na.rm=T),
            median = median(biomass_area_cm2, na.rm=T),
            sd = sd(biomass_area_cm2, na.rm=T)) %>%
  ungroup()
periphy_summary_coupling3

## Algae
hort_field
chl_summary_pond3= hort_field %>% # ug/L
  filter(doy > 176 & doy < 211) %>%
  select(pond_id, doy, chla) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(pond_id) %>%
  summarize(mean = mean(chla, na.rm=T),
            median = median(chla, na.rm=T),
            sd = sd(chla, na.rm=T)) %>%
  ungroup()
chl_summary_pond3 

chl_summary_coupling3 = hort_field %>% # ug/L 
  filter(doy > 176 & doy < 211) %>%
  select(pond_id, doy, chla) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(coupling) %>%
  summarize(mean = mean(chla, na.rm=T),
            median = median(chla, na.rm=T),
            sd = sd(chla, na.rm=T)) %>%
  ungroup()
chl_summary_coupling3 

# 2nd Trophic Level # 
hort_zoop 
zoop_summary_pond3 = hort_zoop %>% #ug/L 
  filter(doy > 176 & doy < 211) %>%
  select(pond_id, doy, group, taxon, biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>% 
  group_by(pond_id, doy) %>%
  summarise(tot = sum(biomass)) %>%
  ungroup() %>%
  group_by(pond_id) %>%
  summarise(mean = mean (tot, na.rm=T),
            median = median(tot, na.rm=T),
            sd = sd(tot, na.rm=T)) %>%
  ungroup() 
zoop_summary_pond3

zoop_summary_coupling3 = hort_zoop %>% #ug/L 
  filter(doy > 176 & doy < 211) %>%
  select(pond_id, doy, group, taxon, biomass) %>%
  mutate(coupling = case_when(
    pond_id == 'B' | pond_id == 'F' ~ 'low', 
    pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
    pond_id == 'C' | pond_id == 'E' ~ 'high')) %>% 
  group_by(pond_id, doy, coupling) %>%
  summarise(tot = sum(biomass)) %>%
  ungroup() %>%
  group_by(coupling) %>%
  summarise(mean = mean (tot, na.rm=T),
            median = median(tot, na.rm=T),
            sd = sd(tot, na.rm=T)) %>%
  ungroup() 
zoop_summary_coupling3

# Macroinvertebrates 