## Convert Chloropohyll to areal estimate ## 
# Pond Width = 12.2 m 
# Pond Length = 45.7 m 
# Mean Depth = 1.2 m 

# VOLUME = 669048 (from pulse addition calculations) 
# SURFACE AREA = 12.2*45.7 = 557.54 m^2

# Periphyton # 
## Already an areal estimate ## 
hort_periphy # ug/cm^2 get to m^2 
peri = hort_periphy %>% 
  mutate(peri_areal = biomass_area*10000) # cm^2 to m^2, multiply by 10000
peri

## Look at in a time series ## 
#set up plot - use collect date as the DOY # 

# Chlorophyll-a # 
## Take the ug/L estimate and convert to ug/m^2 ## 
## Convert ug/L to ug/m^3 then multiply by sample depth to get m^2 ## 
## Data are gapfilled data between 10-30 cm sample depth, use 0.20 m for multiplier ## 
chl = hort_field %>%
  select(pond_id, doy, chla) %>%
  mutate(chl_areal = (1000*chla*0.20))
chl

# Zooplankton Areal # 
## Take the ug/L estimate and convert to ug/m^2 ## 
## Convert ug/L to ug/m^3 then multiply by tow depth to get m^2 
zp = hort_zoop %>%
  group_by(pond_id, doy, treatment) %>% 
  summarise(tot = sum(biomass)) %>%
  select(pond_id,treatment, doy, tot) %>%
  mutate(tot_areal = (1000*tot*1))
zp  

# Generate a boxplot per pond #=============================
# install.packages("ggplot2")
library(ggplot2)

# Data
set.seed(8)
y <- rnorm(200)
group <- sample(LETTERS[1:3], size = 200,
                replace = TRUE)
df <- data.frame(y, group)

# Box plot by group with jitter (example)
df
ggplot(df, aes(x = group, y = y, colour = group)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width=0.1)

Dates30s <- data.frame(seq(as.Date("2011-01-01"), to = as.Date("2040-12-31"), by = "day"))
colnames(Dates30s) <- "date"
FakeData <- data.frame(A = runif(10958, min = 0.3, max = 1.5), 
                       B = runif(10958, min = 1.2, max = 2), 
                       C = runif(10958, min = 0.6, max = 1.8))

### Calculate Year, Month then Water year (WY) and Season
myData <- data.frame(Dates30s, FakeData) %>% 
  mutate(Year = year(date),
         MonthNr = month(date),
         Month = month(date, label = TRUE, abbr = TRUE)) %>% 
  mutate(WY = case_when(MonthNr > 9 ~ Year + 1,
                        TRUE      ~ Year)) %>% 
  mutate(Season = case_when(MonthNr %in%  9:11  ~ "Fall",
                            MonthNr %in%  c(12, 1, 2) ~ "Winter",
                            MonthNr %in%  3:5   ~ "Spring",
                            TRUE ~ "Summer")) %>% 
  select(-date, -MonthNr, -Year) %>% 
  as_tibble()
myData

### Seasonal Avg by WY
SeasonalAvg <- myData %>%  
  select(-Month) %>% 
  group_by(WY, Season) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(key = "State", value = "MFI", -WY, -Season)
SeasonalAvg

s1 <- ggplot(SeasonalAvg, aes(x = Season, y = MFI, color = State)) +
  geom_boxplot(width=0.3, position = position_dodge(width=0.7)) +
  geom_point(position = position_jitterdodge(seed = 0.5))
s1

# Join all taxa by doy and pond_id, filter out by treatment into separate data frames 
peri2 = peri %>% rename(doy = collect) %>% select(pond_id, doy, peri_areal)
chl2 = chl %>% select(pond_id, doy, chl_areal)
zp2 = zp %>% select(pond_id, doy, tot_areal)

join1 = left_join(chl2, peri2, by=c('pond_id', 'doy'))
join1

join2 = left_join(join1, zp2, by=c('pond_id', 'doy'))
join2

low_bp = join2 %>%
  filter(pond_id == 'B' | pond_id == 'F') %>% 
  pivot_longer(cols = c(tot_areal, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass') %>%
  mutate(treatment = case_when(
    pond_id == 'B' ~ 'pulsed', 
    pond_id == 'F' ~ 'reference'
  ))
low_bp

int_bp = join2 %>%
  filter(pond_id == 'A' | pond_id == 'D')%>% 
  pivot_longer(cols = c(tot_areal, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass' ) %>%
  mutate(treatment = case_when(
    pond_id == 'A' ~ 'pulsed', 
    pond_id == 'D' ~ 'reference'
  ))
int_bp

high_bp = join2 %>% 
  filter(pond_id == 'C' | pond_id == 'E') %>% 
  pivot_longer(cols = c(tot_areal, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass' ) %>%
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
  theme(legend.position='none')

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

## Make a separate figure per 'taxa' ## 
join2
pulsed = join2 %>%
  pivot_longer(cols = c(tot_areal, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass') %>%
  mutate(treatment = case_when(
    pond_id == 'B' ~ 'pulsed', 
    pond_id == 'F' ~ 'reference', 
    pond_id == 'A' ~ 'pulsed', 
    pond_id == 'D' ~ 'reference', 
    pond_id == 'C' ~ 'pulsed', 
    pond_id == 'E' ~ 'reference')) %>%
  mutate(type = case_when(
    pond_id == 'B' ~ 'low', 
    pond_id == 'F' ~ 'low', 
    pond_id == 'A' ~ 'int', 
    pond_id == 'D' ~ 'int', 
    pond_id == 'C' ~ 'high', 
    pond_id == 'E' ~ 'high')) %>%
  filter(treatment == 'pulsed')
pulsed

reference = join2 %>%
  pivot_longer(cols = c(tot_areal, chl_areal, peri_areal), names_to='taxa', values_to='areal_biomass') %>%
  mutate(treatment = case_when(
    pond_id == 'B' ~ 'pulsed', 
    pond_id == 'F' ~ 'reference', 
    pond_id == 'A' ~ 'pulsed', 
    pond_id == 'D' ~ 'reference', 
    pond_id == 'C' ~ 'pulsed', 
    pond_id == 'E' ~ 'reference')) %>%
  mutate(type = case_when(
    pond_id == 'B' ~ 'low', 
    pond_id == 'F' ~ 'low', 
    pond_id == 'A' ~ 'int', 
    pond_id == 'D' ~ 'int', 
    pond_id == 'C' ~ 'high', 
    pond_id == 'E' ~ 'high')) %>%
  filter(treatment == 'reference')
reference

chlp = pulsed %>% 
  filter(taxa == 'chl_areal') %>%
  mutate(type = as.factor(type)) %>%
  mutate(val = case_when(
    type == 'low' ~ 1, 
    type == 'int' ~ 2, 
    type == 'high' ~ 3
  )) %>%
  mutate(type = fct_reorder(type, val))

perip = pulsed %>%
  filter(taxa=='peri_areal') %>%
  mutate(type = as.factor(type)) %>%
  mutate(val = case_when(
    type == 'low' ~ 1, 
    type == 'int' ~ 2, 
    type == 'high' ~ 3
  )) %>%
  mutate(type = fct_reorder(type, val))

totp = pulsed %>%
  filter(taxa == 'tot_areal') %>%
  mutate(type = as.factor(type)) %>%
  mutate(val = case_when(
    type == 'low' ~ 1, 
    type == 'int' ~ 2, 
    type == 'high' ~ 3
  )) %>%
  mutate(type = fct_reorder(type, val))

chlr = reference %>%
  filter(taxa == 'chl_areal') %>%
  mutate(type = as.factor(type)) %>%
  mutate(val = case_when(
    type == 'low' ~ 1, 
    type == 'int' ~ 2, 
    type == 'high' ~ 3
  )) %>%
  mutate(type = fct_reorder(type, val))

perir = reference %>%
  filter(taxa == 'peri_areal') %>%
  mutate(type = as.factor(type)) %>%
  mutate(val = case_when(
    type == 'low' ~ 1, 
    type == 'int' ~ 2, 
    type == 'high' ~ 3
  )) %>%
  mutate(type = fct_reorder(type, val))

totr = reference %>%
  filter(taxa == 'tot_areal') %>%
  mutate(type = as.factor(type)) %>%
  mutate(val = case_when(
    type == 'low' ~ 1, 
    type == 'int' ~ 2, 
    type == 'high' ~ 3
  )) %>%
  mutate(type = fct_reorder(type, val))

# Pulsed figures # 
# Low coupling figure # 
windows(height=5, width=10)
library(scales)
library(gridExtra)

p4 = ggplot(chlp, aes(x=type, y=areal_biomass, fill=type)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, int_col, high_col)) + 
  geom_point(pch=21, position = position_jitterdodge(jitter.width = 0.4)) + 
  theme_bw() + 
  annotation_logticks(sides = 'l') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

# Int coupling figure #
p5 = ggplot(perip, aes(x=type, y=areal_biomass, fill=type)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, int_col, high_col)) + 
  geom_point(pch=21, position = position_jitterdodge(jitter.width = 0.4)) + 
  theme_bw() + 
  annotation_logticks(sides = 'l') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

# High coupling figure #
p6 = ggplot(totp, aes(x=type, y=areal_biomass, fill=type)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, int_col, high_col)) + 
  geom_point(pch=21, position = position_jitterdodge(jitter.width = 0.4)) + 
  theme_bw() + 
  annotation_logticks(sides = 'l') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

# Reference Figures 
p7 = ggplot(chlr, aes(x=type, y=areal_biomass, fill=type)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, int_col, high_col)) + 
  geom_point(pch=21, position = position_jitterdodge(jitter.width = 0.4)) + 
  theme_bw() + 
  annotation_logticks(sides = 'l') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

# Int coupling figure #
p8= ggplot(perir, aes(x=type, y=areal_biomass, fill=type)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, int_col, high_col)) + 
  geom_point(pch=21, position = position_jitterdodge(jitter.width = 0.4)) + 
  theme_bw() + 
  annotation_logticks(sides = 'l') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

# High coupling figure #
p9 = ggplot(totr, aes(x=type, y=areal_biomass, fill=type)) + 
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), 
                labels = trans_format('log10', math_format(10^.x)),
                limits = c(100, 550000)) +
  geom_boxplot(outlier.size=0) +
  scale_fill_manual(values=c(low_col, int_col, high_col)) + 
  geom_point(pch=21, position = position_jitterdodge(jitter.width = 0.4)) + 
  theme_bw() + 
  annotation_logticks(sides = 'l') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position='none')

grid.arrange(p4,p5,p6,p7,p8,p9, ncol=3, nrow=2)
