## Step2_Create Food Web Array ## 

# Create an array with 3 columns and 5 rows 
  # Zoobenthos, Zooplankton, Periphyton, Macrophytes, Nutrients 

# Create boxplots for each row (one for pulsed, one for reference)

# First Generate a 3x1 Plot for each dataset 
library(ggplot2)
library(scales)
library(gridExtra)

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

## Zooplankton Biomass ##======================
hort_zoop 

daily_zp_biomass = hort_zoop %>% 
  group_by(pond_id, doy) %>%
  summarize(biomass = sum(biomass)) %>%
  ungroup()
daily_zp_biomass

low_zoop = daily_zp_biomass %>%
  filter(pond_id == 'B' | pond_id == 'F') %>% 
  mutate(treatment = case_when(
    pond_id == 'B' ~ 'pulsed', 
    pond_id == 'F' ~ 'reference'))
low_zoop

p1 = ggplot(low_zoop, aes(x=treatment, y=biomass, fill = treatment, alpha = treatment)) +
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
               labels = trans_format('log10', math_format(10^.x)),
               limits = c(1, 800), name = 'Zooplankton Biomass (ug/L)') +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, low_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() + 
  annotation_logticks(sides = 'l') + 
  annotation_logticks(sides = 'r') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p1

int_zoop = daily_zp_biomass %>%
  filter(pond_id == 'A' | pond_id == 'D') %>% 
  mutate(treatment = case_when(
    pond_id == 'A' ~ 'pulsed', 
    pond_id == 'D' ~ 'reference'))
int_zoop

p2 = ggplot(int_zoop, aes(x=treatment, y=biomass, fill = treatment, alpha = treatment)) +
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 800), name = 'Zooplankton Biomass (ug/L)') +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(int_col, int_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() + 
  annotation_logticks(sides = 'l') + 
  annotation_logticks(sides = 'r') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p2

high_zoop = daily_zp_biomass %>%
  filter(pond_id == 'C' | pond_id == 'E') %>% 
  mutate(treatment = case_when(
    pond_id == 'C' ~ 'pulsed', 
    pond_id == 'E' ~ 'reference'))
high_zoop

p3 = ggplot(high_zoop, aes(x=treatment, y=biomass, fill = treatment, alpha = treatment)) +
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(1, 800), name = 'Zooplankton Biomass (ug/L)') +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(high_col, high_col)) + 
  scale_alpha_manual(values=c(1,0.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3)) + 
  theme_bw() + 
  annotation_logticks(sides = 'l') + 
  annotation_logticks(sides = 'r') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')
p3

grid.arrange(p1, p2, p3, ncol=3, nrow=1)
   
## Zoobenthos Density ##========================
hort_mivdensity

## Periphyton Biomass##======================
hort_periphy

## Macrophytes Dry Biomass ##====================
hort_macrophy

## Nutrients Concentrations (P & N) ##=====================
hort_field