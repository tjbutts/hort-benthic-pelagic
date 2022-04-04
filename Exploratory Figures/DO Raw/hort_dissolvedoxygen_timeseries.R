# Hort Farm Dissolved Oxygen Data # 

#============================================#
# STEP 1: LOAD IN DATASETS 
#============================================#
rm(list=ls())
graphics.off()

# Required Libraries for analysis and visualization
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(magrittr)) install.packages('magrittr')
library(magrittr)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate) 
if (!require(readr)) install.packages('readr')
library(readr)
# Visualization
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2) 
if (!require(scales)) install.packages('scales')
library(scales) 
if (!require(forecast)) install.packages('forecast')
library(forecast) 

# Set working directory to hort-benthic-pelagic folder within Tyler Hort Resilience 

## Daily Average ## 
hort_dosat = read_csv('hort20_dosat_dat.csv')
hort_dosat # Daily average DOSAT data 

## Full Data ## 
full_hort_do = read_csv('2020_dologger_dat.csv')
full_hort_do

# Pond A #==================
a = full_hort_do %>% 
  filter(pond == 'A') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>%
  mutate(doyfrac = decimal_date(datetime))
a = a %>% mutate(rn = seq.int(nrow(a))) %>% as.data.frame()
a

a2 = full_hort_do %>% 
  filter(pond == 'A') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>%
  mutate(doyfrac = decimal_date(datetime)) %>% 
  filter(doy==185) # change to zero in on specific date 
a2

#windows(height=12, width=20)
#par(mfrow=c(3,2))
windows(height=4, width=10)
plot(dosat~doyfrac, data=a, type='l')
mtext(side=3, text = 'Pond A', line=1.5, font=2)
abline(h=100, lwd=2) # 100% saturation line
# Add disturbances 
lines(c(2020.478, 2020.478), c(-10,20000), lty = 3)
lines(c(2020.574, 2020.574), c(-10,20000), lty = 3)
lines(c(2020.607, 2020.607), c(-10,20000), lty = 2, lwd = 2)
col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')
rect(2020.503,-4,2020.526,300, col=col, border=NA)

# Pond B #=============
b = full_hort_do %>% 
  filter(pond == 'B') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>% 
  mutate(doyfrac = decimal_date(datetime)) %>%
  mutate(doyfrac2 = (doyfrac - 2020)*365)
b
b = b %>% mutate(rn = seq.int(nrow(b))) %>% as.data.frame()
b

b2 = full_hort_do %>% 
  filter(pond == 'B') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>%
  mutate(doyfrac = decimal_date(doy)) %>% 
  filter(doy==185) # change to zero in on specific date 
b2

plot(dosat~doyfrac2, data=b, type='o', xaxt='n', lwd=2, xlim=c(190,240))
axis(side=1, at=b$doy)
mtext(side=3, text = 'Pond B', line=1.5, font=2)
abline(h=100, lwd=2) # 100% saturation line
# Add disturbances 
lines(c(2020.478, 2020.478), c(-10,20000), lty = 3)
lines(c(2020.574, 2020.574), c(-10,20000), lty = 3)
lines(c(2020.607, 2020.607), c(-10,20000), lty = 2, lwd = 2)
col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')
rect(2020.503,-4,2020.526,300, col=col, border=NA)

# Pond C #=============
c = full_hort_do %>% 
  filter(pond == 'C') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>%
  mutate(doyfrac = decimal_date(datetime))
c = c %>% mutate(rn = seq.int(nrow(c))) %>% as.data.frame()
c

c2 = full_hort_do %>% 
  filter(pond == 'C') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>%
  mutate(doyfrac = decimal_date(datetime)) %>% 
  filter(doy==185) # change to zero in on specific date 
c2

plot(dosat~doyfrac, data=c, type='o', lwd=2)
mtext(side=3, text = 'Pond C', line=1.5, font=2)
abline(h=100, lwd=2) # 100% saturation line
# add disturbances 
lines(c(2020.478, 2020.478), c(-10,20000), lty = 3)
lines(c(2020.574, 2020.574), c(-10,20000), lty = 3)
lines(c(2020.607, 2020.607), c(-10,20000), lty = 2, lwd = 2)
col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')
rect(2020.503,-4,2020.526,300, col=col, border=NA)

# Pond D #=============
d = full_hort_do %>% 
  filter(pond == 'D') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>%
  mutate(doyfrac = decimal_date(datetime))
d = d %>% mutate(rn = seq.int(nrow(d))) %>% as.data.frame()
d

d2 = full_hort_do %>% 
  filter(pond == 'D') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>%
  mutate(doyfrac = decimal_date(datetime)) %>% 
  filter(doy==185) # change to zero in on specific date 
d2

plot(dosat~doyfrac, data=d, type='o')
mtext(side=3, text = 'Pond D', line=1.5, font=2)
abline(h=100, lwd=2) # 100% saturation line
# add disturbances 
lines(c(2020.478, 2020.478), c(-10,20000), lty = 3)
lines(c(2020.574, 2020.574), c(-10,20000), lty = 3)
lines(c(2020.607, 2020.607), c(-10,20000), lty = 2, lwd = 2)
col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')
rect(2020.503,-4,2020.526,300, col=col, border=NA)

# Pond E #=============
e = full_hort_do %>% 
  filter(pond == 'E') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>%
  mutate(doyfrac = decimal_date(datetime))
e = e %>% mutate(rn = seq.int(nrow(e))) %>% as.data.frame()
e

e2 = full_hort_do %>% 
  filter(pond == 'E') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>%
  mutate(doyfrac = decimal_date(datetime)) %>% 
  filter(doy==185) # change to zero in on specific date 
e2

plot(dosat~doyfrac, data=e, type='o')
mtext(side=3, text = 'Pond E', line=1.5, font=2)
abline(h=100, lwd=2) # 100% saturation line
# add disturbances 
lines(c(2020.478, 2020.478), c(-10,20000), lty = 3)
lines(c(2020.574, 2020.574), c(-10,20000), lty = 3)
lines(c(2020.607, 2020.607), c(-10,20000), lty = 2, lwd = 2)
col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')
rect(2020.503,-4,2020.526,300, col=col, border=NA)

# Pond F #=============
f = full_hort_do %>% 
  filter(pond == 'F') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>%
  mutate(doyfrac = decimal_date(datetime))
f = f %>% mutate(rn = seq.int(nrow(f))) %>% as.data.frame()
f

f2 = full_hort_do %>% 
  filter(pond == 'F') %>% 
  mutate(datetime = mdy_hm(datetime),
         date = mdy(date)) %>%
  mutate(doyfrac = decimal_date(datetime)) %>% 
  filter(doy==185) # change to zero in on specific date 
f2

plot(dosat~doyfrac, data=f, type='o')
mtext(side=3, text = 'Pond F', line=1.5, font=2)
abline(h=100, lwd=2) # 100% saturation line
# add disturbances 
lines(c(2020.478, 2020.478), c(-10,20000), lty = 3)
lines(c(2020.574, 2020.574), c(-10,20000), lty = 3)
lines(c(2020.607, 2020.607), c(-10,20000), lty = 2, lwd = 2)
col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')
rect(2020.503,-4,2020.526,300, col=col, border=NA)
