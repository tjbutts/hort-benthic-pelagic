# For Ellen's Macrophytes -- did Daphnia biomass differ between ponds F&B
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(ggridges)) install.packages('ggridges')
library(ggridges)
if (!require(gridExtra)) install.packages('gridExtra')
library(gridExtra)

#Data sets
zoops = read.csv("hort_zp_clean_11622.csv")
surface = read.csv("hort20_surface_dat.csv")

#==========================================
#Summer Mean Biomass for various taxonomic groupings
pondF = zoops %>%
  filter(pond_id == "F") %>%
  group_by(group) %>%
  summarize(summer_biomass = mean(biomass, na.rm = TRUE)) %>%
  ungroup()

pondB = zoops %>%
  filter(pond_id == "B") %>%
  group_by(group) %>%
  summarize(summer_biomass = mean(biomass, na.rm = TRUE)) %>%
  ungroup()


windows(height = 6, width = 4)
par(mfrow = c(2,1), mai = c(0.5,0.5,0.1,0.1), omi = c(1,0.4,0.1,0.1))
barplot(pondF$summer_biomass, names = pondB$group, las = 2, 
        col ="#4575b4", ylim = c(0,50))
mtext(side = 2, line = 2.5, "Biomass (ug/L)")
text(12, 10, "Pond F", font = 4)
barplot(pondB$summer_biomass, names = pondB$group, 
        las = 2, col = "#f46d43", ylim = c(0,50))
mtext(side = 2, line = 2.5, "Biomass (ug/L)")
text(12, 10, "Pond B", font = 4)

#==================================
# Nutrients
nutsB = surface %>%
  filter(pond_id == "B") %>%
  filter(!(is.na(tp))) %>%
  select(pond_id, doy, period, tp, srp, tn, nox, nhx)

nutsF = surface %>%
  filter(pond_id == "F") %>%
  filter(!(is.na(tp))) %>%
  select(pond_id, doy, period, tp, srp, tn, nox, nhx)

nuts_mac = rbind(nutsB, nutsF)


col_B = rgb(244,109,67, max = 255, alpha = 150)
col_F = rgb(69,117,180, max = 255, alpha = 150)


tp_ridge <-
ggplot(nuts_mac, aes(x = tp, y = pond_id, fill = pond_id)) + 
  geom_density_ridges2(scale = 2, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(col_B, col_F)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = 'Total P (ug/L)') +
  xlab(label = '') +
  ylab(label = '')

srp_ridge <-
ggplot(nuts_mac, aes(x = srp, y = pond_id, fill = pond_id)) + 
  geom_density_ridges2(scale = 2, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(col_B, col_F)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = 'Soluble P (ug/L)') +
  xlab(label = '') +
  ylab(label = '')

tn_ridge <-
ggplot(nuts_mac, aes(x = tn, y = pond_id, fill = pond_id)) + 
  geom_density_ridges2(scale = 2, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(col_B, col_F)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = 'Total N (mg/L)') +
  xlab(label = '') +
  ylab(label = '')

nox_ridge <- 
  ggplot(nuts_mac, aes(x = nox, y = pond_id, fill = pond_id)) + 
  geom_density_ridges2(scale = 2, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(col_B, col_F)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = 'Nitrate (mg/L)') +
  xlab(label = '') +
  ylab(label = '')

windows(6,6)
grid.arrange(tp_ridge, srp_ridge, tn_ridge, nox_ridge, nrow = 2)


#==================================
# Chlorophyll
plot(surface[surface$pond_id=="B", "doy"], 
     surface[surface$pond_id=="B", "chla"], 
     type = "l", col = col_B, lwd = 4, ylim = c(0,12),
     xlab = "Day of Year, 2020", ylab = "Chlorophyll (0-0.25 m)")
points(surface[surface$pond_id=="F", "doy"], 
       surface[surface$pond_id=="F", "chla"], 
       type = "l", col = col_F, lwd = 4)
legend("topright", legend = c("Pond B", "Pond F"), 
       pch = 15, col = c(col_B, col_F), pt.cex = 2)
