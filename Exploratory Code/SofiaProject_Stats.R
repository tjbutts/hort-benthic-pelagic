######################################
## ANOVA on Hort Macroinvertebrates ##
######################################

# Load packages # 
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
install.packages('AICcmodavg')
library(AICcmodavg)

dat = hort_mivdensity %>% 
  filter(common_name != 'caddisfly_case' & common_name != 'tadpole') %>% 
  mutate(fw = case_when(pond_id == 'A' | pond_id == 'D' ~ 'intermediate', 
                        pond_id == 'B' | pond_id == 'F' ~ 'low', 
                        pond_id == 'C' | pond_id == 'E' ~ 'high')) %>%
  group_by(pond_id, fw, doy) %>%
  summarise(tot_density = sum(density)) %>% 
  ungroup() %>%
  mutate(fw = as.factor(fw), 
         pond_id = as.factor(pond_id),
         doy = as.factor(doy), 
         tot_density = as.numeric(tot_density)) %>%
  as.data.frame()
dat

# one way Anova - Density by food web configuration 
one.way = aov(log(tot_density)~fw, data=dat)
summary(one.way)
two.way = aov(log(tot_density)~fw + doy, data=dat)
summary(two.way)
interact = aov(log(tot_density)~fw*doy, data=dat)
summary(interact)
block = aov(log(tot_density)~fw+doy+pond_id, data=dat)
summary(block)

model.set = list(one.way, two.way, interact, block)
model.names = c('one.way', 'two.way', 'interact', 'block')
aictab(model.set, modnames = model.names)

par(mfrow=c(2,2))
plot(block)

tukey.block = TukeyHSD(block)
tukey.block

graphics.off()
tukey.plot.aov = aov(log(tot_density) ~ fw+doy+pond_id, data = dat)
tukey.plot = TukeyHSD(tukey.plot.aov)
plot(tukey.plot, las=1)

block.plot <- ggplot(dat, aes(x = pond_id, y = tot_density, group = fw)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
block.plot


######################################
##       Median Density of MIVs     ##
###################################### 
hort_mivdensity
