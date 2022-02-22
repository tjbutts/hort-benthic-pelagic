## 3x3 Array of food web figure ## 

# Periphyton # 
## Already an areal estimate ## 
hort_periphy # ug/cm^2 get to m^2 
peri = hort_periphy %>% 
  mutate(peri_areal = biomass_area*10000) %>% # cm^2 to m^2, multiply by 10000 
  mutate(peri_areal_mg = peri_areal/1000)
peri

## Look at in a time series ## 
#set up plot - use collect date as the DOY # 

# Chlorophyll-a # 
## Take the ug/L estimate and convert to ug/m^2 ## 
## Convert ug/L to ug/m^3 then multiply by sample depth to get m^2 ## 
## Data are gapfilled data between 10-30 cm sample depth, use 0.20 m for multiplier ## 
chl = hort_field %>%
  select(pond_id, doy, chla) %>%
  mutate(chl_areal = (1000*chla*0.20)) %>%
  mutate(chl_areal_mg = chl_areal/1000)
chl

# Macrophytes # 
## convert the g/m^2 to mg/m^2 ## 
macrophy = hort_macrophy %>% 
  select(pond_id, doy, areal_biomass) %>%
  mutate(macrophy_areal_mg = areal_biomass*1000) 
macrophy[macrophy == 0] <- 0.001
macrophy

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
peri2 = peri %>% rename(doy = collect) %>% select(pond_id, doy, peri_areal_mg)
chl2 = chl %>% select(pond_id, doy, chl_areal_mg)
macrophy2 = macrophy %>% select(pond_id, doy, macrophy_areal_mg)

join1 = left_join(chl2, peri2, by=c('pond_id', 'doy'))
join1

join2 = left_join(join1, macrophy2, by=c('pond_id', 'doy'))
join2

low_bp = join2 %>%
  filter(pond_id == 'B' | pond_id == 'F') %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal_mg, peri_areal_mg), names_to='taxa', values_to='areal_biomass') %>%
  mutate(treatment = case_when(
    pond_id == 'B' ~ 'pulsed', 
    pond_id == 'F' ~ 'reference'
  ))
low_bp

int_bp = join2 %>%
  filter(pond_id == 'A' | pond_id == 'D')%>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal_mg, peri_areal_mg), names_to='taxa', values_to='areal_biomass' ) %>%
  mutate(treatment = case_when(
    pond_id == 'A' ~ 'pulsed', 
    pond_id == 'D' ~ 'reference'
  ))
int_bp

high_bp = join2 %>% 
  filter(pond_id == 'C' | pond_id == 'E') %>% 
  pivot_longer(cols = c(macrophy_areal_mg, chl_areal_mg, peri_areal_mg), names_to='taxa', values_to='areal_biomass' ) %>%
  mutate(treatment = case_when(
    pond_id == 'C' ~ 'pulsed', 
    pond_id == 'E' ~ 'reference'
  ))
high_bp

#Plot Figures #====================

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

# Low coupling figure # 
windows(height=3, width=10)
library(scales)
library(gridExtra)

p1 = ggplot(low_bp, aes(x=treatment, y=areal_biomass, fill=taxa, alpha=treatment)) + 
  #scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
   #             labels = trans_format('log10', math_format(10^.x)),
    #            limits = c(0, 550000)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, int_col, high_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(pch=21, position = position_jitterdodge(jitter.width = 0.1)) + 
  theme_bw() + 
  #annotation_logticks(sides = 'l') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p1
# Int coupling figure #
p2 = ggplot(int_bp, aes(x=treatment, y=areal_biomass, fill=taxa, alpha=treatment)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, int_col, high_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(pch=21, position = position_jitterdodge(jitter.width = 0.1)) + 
  theme_bw() + 
  annotation_logticks(sides = 'l') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# High coupling figure #
p3 = ggplot(high_bp, aes(x=treatment, y=areal_biomass, fill=taxa, alpha=treatment)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, int_col, high_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(pch=21, position = position_jitterdodge(jitter.width = 0.1)) + 
  theme_bw() + 
  annotation_logticks(sides = 'l') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = 'none')

grid.arrange(p1,p2,p3, ncol=3, nrow=1)

