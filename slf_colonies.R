# This script runs a staggered diff-in-diff estimating the effect of SLF on forest cover

# Making sure requisite libraries are installed

list.of.packages <- c('modelsummary', 'tidycensus', 'stargazer', 'sandwich', 'leaflet', 'lydemapr',
                      'ggplot2', 'tigris', 'lmtest', 'dplyr', 'DRDID', 'did', 'sf')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]

if (length(new.packages) > 0) {
  
  install.packages(new.packages)
  
}

# Loading libraries

library(modelsummary)
library(tidycensus)
library(stargazer)
library(sandwich)
library(lydemapr)
library(leaflet)
library(ggplot2)
library(tigris)
library(lmtest)
library(dplyr)
library(DRDID)
library(did)
library(sf)

# Project directory

direc <- 'D:/SLF/'

# Getting SLKF data from lydemapr

lyde <- lydemapr::lyde

# Filter for high established colonies only

lyde <- lyde %>% filter(lyde_established == TRUE)

# Geolocating lyde observations with tigris

options(tigris_use_cache = TRUE)
co <- counties()
co$STATEFP <- as.integer(co$STATEFP)
co <- co %>% filter(STATEFP <= 56)
co <- co %>% filter(!STATEFP %in% c(2, 15))

lyde <- st_as_sf(lyde, coords = c('longitude', 'latitude'))
lyde <- lyde %>% st_set_crs(st_crs(co))

inside <- st_within(lyde, co)

fips <- c()

for (i in 1:ceiling(nrow(lyde)/10000)) {
  
  print(i)
  tmp <- c()
  
  if (i < ceiling(nrow(lyde)/10000)) {
    
    for (j in (1+(i-1)*10000):(i*10000)) {
      
      tmp <- c(tmp, co$GEOID[inside[[j]][1]])
      
    }
    
  } else {
    
    for (j in (1+(i-1)*10000):nrow(lyde)) {
      
      tmp <- c(tmp, co$GEOID[inside[[j]][1]])
      
    }
    
  }
  
  fips <- c(fips, tmp)
  
}

lyde$FIPS <- fips

rm(fips)
rm(inside)

# Converting lyde into a panel

fips <- c()
year <- c()
treated <- c()

for (y in unique(lyde$bio_year)) {
  
  print(y)
  tmp <- lyde %>% filter(bio_year == y)
  
  for (f in unique(lyde$FIPS)) {
    
    tmp2 <- tmp %>% filter(FIPS == f)
    fips <- c(fips, f)
    year <- c(year, y)
    treated <- c(treated, as.integer(nrow(tmp2) > 0))
    
  }
  
}

slf <- as.data.frame(cbind(fips, year, treated))
colnames(slf) <- c('County', 'Year', 'SLF')
slf$SLF <- as.integer(slf$SLF)
slf <- slf %>% filter(!is.na(County))

# Bringing in data from the ACS

acs.pop <- as.data.frame(NULL)

for (y in 2010:2023) {
  
  tmp <- get_acs(geography = 'county', year = y, variables = 'DP05_0001')
  tmp$Year <- rep(y, nrow(tmp))
  acs.pop <- rbind(acs.pop, tmp)
  
}

acs.data <- acs.pop[,c(1,4,6)]
colnames(acs.data)[2] <- 'Population'
rm(acs.pop)

v10 <- c('DP03_0008', 'DP03_0008P', 'DP03_0009', 'DP03_0009P', 'DP03_0062', 'DP03_0119',
         'DP03_0119P', 'DP05_0025', 'DP05_0025P', 'DP02_0067', 'DP02_0067P', 'DP05_0032',
         'DP05_0032P', 'DP05_0033', 'DP05_0033P', 'DP05_0066', 'DP05_0066P', 'DP04_0001',
         'DP03_0067', 'DP04_0100')

v15 <- c('DP03_0008', 'DP03_0008P', 'DP03_0009', 'DP03_0009P', 'DP03_0062', 'DP03_0119',
         'DP03_0119P', 'DP05_0025', 'DP05_0025P', 'DP02_0067', 'DP02_0067P', 'DP05_0032',
         'DP05_0032P', 'DP05_0033', 'DP05_0033P', 'DP05_0066', 'DP05_0066P', 'DP04_0001',
         'DP03_0067', 'DP04_0101')

v17 <- c('DP03_0008', 'DP03_0008P', 'DP03_0009', 'DP03_0009P', 'DP03_0062', 'DP03_0119',
         'DP03_0119P', 'DP05_0029', 'DP05_0029P', 'DP02_0067', 'DP02_0067P', 'DP05_0037',
         'DP05_0037P', 'DP05_0038', 'DP05_0038P', 'DP05_0071', 'DP05_0071P', 'DP04_0001',
         'DP03_0067', 'DP04_0101')

v18 <- c('DP03_0008', 'DP03_0008P', 'DP03_0009', 'DP03_0009P', 'DP03_0062', 'DP03_0119',
         'DP03_0119P', 'DP05_0029', 'DP05_0029P', 'DP02_0067', 'DP02_0067P', 'DP05_0037',
         'DP05_0037P', 'DP05_0038', 'DP05_0038P', 'DP05_0071', 'DP05_0071P', 'DP04_0001',
         'DP03_0067', 'DP04_0101')

v19 <- c('DP03_0008', 'DP03_0008P', 'DP03_0009', 'DP03_0009P', 'DP03_0062', 'DP03_0119',
         'DP03_0119P', 'DP05_0024', 'DP05_0024P', 'DP02_0068', 'DP02_0068P', 'DP05_0037',
         'DP05_0037P', 'DP05_0038', 'DP05_0038P', 'DP05_0071', 'DP05_0071P', 'DP04_0001',
         'DP03_0067', 'DP04_0101')

v22 <- c('DP03_0008', 'DP03_0008P', 'DP03_0009', 'DP03_0009P', 'DP03_0062', 'DP03_0119',
         'DP03_0119P', 'DP05_0024', 'DP05_0024P', 'DP02_0068', 'DP02_0068P', 'DP05_0037',
         'DP05_0037P', 'DP05_0038', 'DP05_0038P', 'DP05_0073', 'DP05_0073P', 'DP04_0001',
         'DP03_0067', 'DP04_0101')

v23 <- c('DP03_0008', 'DP03_0008P', 'DP03_0009', 'DP03_0009P', 'DP03_0062', 'DP03_0119',
         'DP03_0119P', 'DP05_0024', 'DP05_0024P', 'DP02_0068', 'DP02_0068P', 'DP05_0037',
         'DP05_0037P', 'DP05_0038', 'DP05_0038P', 'DP05_0076', 'DP05_0076P', 'DP04_0001',
         'DP03_0067', 'DP04_0101')

acs.x <- as.data.frame(NULL)

for (y in 2010:2014) {
  
  tmp <- get_acs(geography = 'county', year = y, variables = v10)
  tmp$Year <- rep(y, nrow(tmp))
  
  v <- c()
  
  for (i in 1:nrow(tmp)) {
    
    print(i)
    v <- c(v, which(v10 == tmp$variable[i]))
    
  }
  
  tmp$V <- v
  acs.x <- rbind(acs.x, tmp)
  
}

for (y in 2015:2016) {
  
  tmp <- get_acs(geography = 'county', year = y, variables = v15)
  tmp$Year <- rep(y, nrow(tmp))
  
  v <- c()
  
  for (i in 1:nrow(tmp)) {
    
    print(i)
    v <- c(v, which(v15 == tmp$variable[i]))
    
  }
  
  tmp$V <- v
  acs.x <- rbind(acs.x, tmp)
  
}

tmp <- get_acs(geography = 'county', year = 2017, variables = v17)
tmp$Year <- rep(2017, nrow(tmp))

v <- c()

for (i in 1:nrow(tmp)) {
  
  print(i)
  v <- c(v, which(v17 == tmp$variable[i]))
  
}

tmp$V <- v
acs.x <- rbind(acs.x, tmp)

tmp <- get_acs(geography = 'county', year = 2018, variables = v18)
tmp$Year <- rep(2018, nrow(tmp))

v <- c()

for (i in 1:nrow(tmp)) {
  
  print(i)
  v <- c(v, which(v18 == tmp$variable[i]))
  
}

tmp$V <- v
acs.x <- rbind(acs.x, tmp)

for (y in 2019:2021) {
  
  tmp <- get_acs(geography = 'county', year = y, variables = v19)
  tmp$Year <- rep(y, nrow(tmp))
  
  v <- c()
  
  for (i in 1:nrow(tmp)) {
    
    print(i)
    v <- c(v, which(v19 == tmp$variable[i]))
    
  }
  
  tmp$V <- v
  acs.x <- rbind(acs.x, tmp)
  
}

tmp <- get_acs(geography = 'county', year = 2022, variables = v22)
tmp$Year <- rep(2022, nrow(tmp))

v <- c()

for (i in 1:nrow(tmp)) {
  
  print(i)
  v <- c(v, which(v22 == tmp$variable[i]))
  
}

tmp$V <- v
acs.x <- rbind(acs.x, tmp)

tmp <- get_acs(geography = 'county', year = 2023, variables = v23)
tmp$Year <- rep(2023, nrow(tmp))

v <- c()

for (i in 1:nrow(tmp)) {
  
  print(i)
  v <- c(v, which(v23 == tmp$variable[i]))
  
}

tmp$V <- v
acs.x <- rbind(acs.x, tmp)

jobs <- c('DP03_0033', 'DP03_0034', 'DP03_0035', 'DP03_0036', 'DP03_0037', 'DP03_0038',
          'DP03_0039', 'DP03_0040', 'DP03_0041', 'DP03_0042', 'DP03_0043', 'DP03_0044', 'DP03_0045')

for (y in 2010:2023) {
  
  tmp <- get_acs(geography = 'county', year = y, variables = jobs)
  tmp$Year <- rep(y, nrow(tmp))
  
  v <- c()
  
  for (i in 1:nrow(tmp)) {
    
    print(i)
    v <- c(v, which(jobs == tmp$variable[i]) + length(v23))
    
  }
  
  tmp$V <- v
  acs.x <- rbind(acs.x, tmp)
  
}

acs.lfp <- c()
acs.lfp2 <- c()
acs.unemp <- c()
acs.unemp2 <- c()
acs.hhinc <- c()
acs.pov <- c()
acs.pov2 <- c()
acs.old <- c()
acs.old2 <- c()
acs.bs <- c()
acs.bs2 <- c()
acs.white <- c()
acs.white2 <- c()
acs.black <- c()
acs.black2 <- c()
acs.hisp <- c()
acs.hisp2 <- c()
acs.hunits <- c()
acs.ss <- c()
acs.mort <- c()

acs.j1 <- c()
acs.j2 <- c()
acs.j3 <- c()
acs.j4 <- c()
acs.j5 <- c()
acs.j6 <- c()
acs.j7 <- c()
acs.j8 <- c()
acs.j9 <- c()
acs.j10 <- c()
acs.j11 <- c()
acs.j12 <- c()
acs.j13 <- c()

for (i in 1:nrow(acs.data)) {
  
  print(i)
  tmp <- acs.x %>% filter(GEOID == acs.data$GEOID[i]) %>% filter(Year == acs.data$Year[i])
  
  acs.lfp <- c(acs.lfp, tmp[which(tmp$V == 1),]$estimate[1])
  acs.lfp2 <- c(acs.lfp2, tmp[which(tmp$V == 2),]$estimate[1])
  acs.unemp <- c(acs.unemp, tmp[which(tmp$V == 3),]$estimate[1])
  acs.unemp2 <- c(acs.unemp2, tmp[which(tmp$V == 4),]$estimate[1])
  acs.hhinc <- c(acs.hhinc, tmp[which(tmp$V == 5),]$estimate[1])
  acs.pov <- c(acs.pov, tmp[which(tmp$V == 6),]$estimate[1])
  acs.pov2 <- c(acs.pov2, tmp[which(tmp$V == 7),]$estimate[1])
  acs.old <- c(acs.old, tmp[which(tmp$V == 8),]$estimate[1])
  acs.old2 <- c(acs.old2, tmp[which(tmp$V == 9),]$estimate[1])
  acs.bs <- c(acs.bs, tmp[which(tmp$V == 10),]$estimate[1])
  acs.bs2 <- c(acs.bs2, tmp[which(tmp$V == 11),]$estimate[1])
  acs.white <- c(acs.white, tmp[which(tmp$V == 12),]$estimate[1])
  acs.white2 <- c(acs.white2, tmp[which(tmp$V == 13),]$estimate[1])
  acs.black <- c(acs.black, tmp[which(tmp$V == 14),]$estimate[1])
  acs.black2 <- c(acs.black2, tmp[which(tmp$V == 15),]$estimate[1])
  acs.hisp <- c(acs.hisp, tmp[which(tmp$V == 16),]$estimate[1])
  acs.hisp2 <- c(acs.hisp2, tmp[which(tmp$V == 17),]$estimate[1])
  acs.hunits <- c(acs.hunits, tmp[which(tmp$V == 18),]$estimate[1])
  acs.ss <- c(acs.ss, tmp[which(tmp$V == 19),]$estimate[1])
  acs.mort <- c(acs.mort, tmp[which(tmp$V == 20),]$estimate[1])
  
  acs.j1 <- c(acs.j1, tmp[which(tmp$V == 21),]$estimate[1])
  acs.j2 <- c(acs.j2, tmp[which(tmp$V == 22),]$estimate[1])
  acs.j3 <- c(acs.j3, tmp[which(tmp$V == 23),]$estimate[1])
  acs.j4 <- c(acs.j4, tmp[which(tmp$V == 24),]$estimate[1])
  acs.j5 <- c(acs.j5, tmp[which(tmp$V == 25),]$estimate[1])
  acs.j6 <- c(acs.j6, tmp[which(tmp$V == 26),]$estimate[1])
  acs.j7 <- c(acs.j7, tmp[which(tmp$V == 27),]$estimate[1])
  acs.j8 <- c(acs.j8, tmp[which(tmp$V == 28),]$estimate[1])
  acs.j9 <- c(acs.j9, tmp[which(tmp$V == 29),]$estimate[1])
  acs.j10 <- c(acs.j10, tmp[which(tmp$V == 30),]$estimate[1])
  acs.j11 <- c(acs.j11, tmp[which(tmp$V == 31),]$estimate[1])
  acs.j12 <- c(acs.j12, tmp[which(tmp$V == 32),]$estimate[1])
  acs.j13 <- c(acs.j13, tmp[which(tmp$V == 33),]$estimate[1])
  
}

acs.data$Population <- log(acs.data$Population)
acs.data$LFP <- log(acs.lfp+1)
acs.data$Unemployment_Rate <- acs.unemp2 / 100
acs.data$HH_Income <- log(acs.hhinc+1)
acs.data$Poverty_Rate <- acs.pov2 / 100
acs.data$Elderly <- log(acs.old+1)
acs.data$College_Pct <- acs.bs2 / 100
acs.data$White <- log(acs.white+1)
acs.data$White_Pct <- acs.white2 / 100
acs.data$Black <- log(acs.black+1)
acs.data$Black_Pct <- acs.black2 / 100
acs.data$Hispanic <- log(acs.hisp+1)
acs.data$Hispanic_Pct <- acs.hisp2 / 100
acs.data$Housing_Units <- log(acs.hunits+1)
acs.data$Social_Security <- log(acs.ss+1)
acs.data$Mortgage <- log(acs.mort+1)

acs.data$J1 <- log(acs.j1+1)
acs.data$J2 <- log(acs.j2+1)
acs.data$J3 <- log(acs.j3+1)
acs.data$J4 <- log(acs.j4+1)
acs.data$J5 <- log(acs.j5+1)
acs.data$J6 <- log(acs.j6+1)
acs.data$J7 <- log(acs.j7+1)
acs.data$J8 <- log(acs.j8+1)
acs.data$J9 <- log(acs.j9+1)
acs.data$J10 <- log(acs.j10+1)
acs.data$J11 <- log(acs.j11+1)
acs.data$J12 <- log(acs.j12+1)
acs.data$J13 <- log(acs.j13+1)

rm(acs.x)

# Subsetting for the contiguous US

acs.data$State <- substr(acs.data$GEOID, 1, 2)
acs.data <- acs.data %>% filter(! State %in% c('02', '15', '72'))
acs.data$County <- as.integer(acs.data$GEOID)

# Joining dataframes

acs.data$County <- as.integer(acs.data$County)
acs.data$Year <- as.integer(acs.data$Year)
slf$County <- as.integer(slf$County)
slf$Year <- as.integer(slf$Year)
data <- left_join(acs.data, slf, by = c('County', 'Year'))
data$SLF[which(is.na(data$SLF))] <- 0

# Reading in the income data from the IRS

irs.co <- c()
irs.inc <- c()
irs.farms <- c()
irs.years <- c()

for (y in 2010:2022) {
  
  print(y)
  
  yr <- y - 2000
  tmp <- read.csv(paste0(direc, 'data/IRS/', yr, 'incyallagi.csv'))
  tmp$FIPS <- 1000*tmp$STATEFIPS + tmp$COUNTYFIPS
  
  for (c in unique(data$County)) {
    
    tmpx <- tmp %>% filter(FIPS == c)
    
    irs.co <- c(irs.co, c)
    irs.inc <- c(irs.inc, sum(tmpx$A00200) / sum(tmpx$N00200))
    irs.farms <- c(irs.farms, sum(tmpx$SCHF))
    irs.years <- c(irs.years, y)
    
  }
  
}

irs <- as.data.frame(cbind(irs.co, irs.inc, irs.farms, irs.years))
colnames(irs) <- c('County', 'Income', 'Farms', 'Year')

# Merge income with the rest of the data

irs$County <- as.integer(irs$County)
irs$Year <- as.integer(irs$Year)
data <- left_join(data, irs, by = c('County', 'Year'))

# Adding treatment timing info for the sdid model

treat <- c()
post <- c()

for (i in 1:nrow(data)) {
  
  print(i)
  tmp <- data %>% filter(County == data$County[i])
  
  if (max(tmp$SLF == 1)) {
    
    treat <- c(treat, 1)
    
    if (data$Year[i] >= tmp$Year[min(which(tmp$SLF == 1))]) {
      
      post <- c(post, 1)
      
    } else {
      
      post <- c(post, 0)
      
    }
    
  } else {
    
    treat <- c(treat, 0)
    post <- c(post, 0)
    
  }
  
}

data$Treated <- treat
data$Post <- post

# Creating variables for the callaway and sant'anna sdid model

data$Year <- as.integer(data$Year)
data$Time <- data$Year - 2009

data.xxx <- c()

for (i in 1:nrow(data)) {
  
  print(i)
  tmp <- data %>% filter(County == data$County[i])
  
  if (max(tmp$Treated) == 0) {
    
    data.xxx <- c(data.xxx, 0)
    
  } else {
    
    data.xxx <- c(data.xxx, min(which(tmp$Post == 1)))
    
  }
  
}

data$Treat_Time <- data.xxx

# Make ids numeric for att_gt

uniq <- sort(unique(data$County))

id.vals <- c()

for (i in 1:nrow(data)) {
  
  print(i)
  id.vals <- c(id.vals, which(uniq == data$County[i]))
  
}

data$ID <- id.vals

# Logging income (which was given in thousands of USD) and farms

data$Income <- log(data$Income*1000)
data$Farms <- log(data$Farms+1)

# Reading the CBP wage data

cbp.co <- c()
cbp.yr <- c()
cbp.emp <- c()
cbp.wag <- c()
cbp.est <- c()

for (y in 2010:2011) {
  
  print(y)
  tmp <- read.csv(paste0(direc, 'data/CBP/cbp', y-2000, 'co.txt'))
  tmp <- tmp[which(tmp$naics == '------'),]
  
  tmp.co <- c()
  
  for (i in 1:nrow(tmp)) {
    
    tmp.co <- c(tmp.co, tmp$fipstate[i]*1000 + tmp$fipscty[i])
  }
  
  tmp$County <- tmp.co
  
  for (c in unique(data$County)) {
    
    tmpx <- tmp %>% filter(County == c)
    
    cbp.co <- c(cbp.co, c)
    cbp.yr <- c(cbp.yr, y)
    cbp.emp <- c(cbp.emp, max(tmpx$emp, na.rm = T))
    cbp.wag <- c(cbp.wag, max(tmpx$ap, na.rm = T) / max(tmpx$emp, na.rm = T))
    cbp.est <- c(cbp.est, max(tmpx$est, na.rm = T))
    
  }
  
}

for (y in 2012:2023) {
  
  print(y)
  tmp <- read.csv(paste0(direc, 'data/CBP/CBP', y, '.CB', y-2000, '00CBP-Data.csv'))
  tmp <- tmp[which(tmp[,3] == '00'),]
  
  tmp.co <- c()
  
  for (i in 1:nrow(tmp)) {
    
    tmp.co <- c(tmp.co, as.integer(strsplit(tmp$GEO_ID[i], 'US')[[1]][2]))
  }
  
  tmp$County <- tmp.co
  
  for (c in unique(data$County)) {
    
    tmpx <- tmp %>% filter(County == c)
    tmpx$EMP <- as.integer(tmpx$EMP)
    tmpx$PAYANN <- as.integer(tmpx$PAYANN)
    tmpx$ESTAB <- as.integer(tmpx$ESTAB)
    
    cbp.co <- c(cbp.co, c)
    cbp.yr <- c(cbp.yr, y)
    cbp.emp <- c(cbp.emp, max(tmpx$EMP, na.rm = T))
    cbp.wag <- c(cbp.wag, max(tmpx$PAYANN, na.rm = T) / max(tmpx$EMP, na.rm = T))
    cbp.est <- c(cbp.est, max(tmpx$ESTAB, na.rm = T))
    
  }
  
}

cbp <- as.data.frame(cbind(cbp.co, cbp.yr, cbp.emp, cbp.wag, cbp.est))
colnames(cbp) <- c('County', 'Year', 'Employees', 'Wages', 'Establishments')

# Merge in the cbp data

data <- left_join(data, cbp, by = c('County', 'Year'))

# Reading in the commuting zone / labor market data

coms <- read.csv(paste0(direc, 'data/ERS/preliminary-2020-commuting-zones.csv'))

# Adding commuting zone identifiers to the data set

coms$County <- coms$FIPStxt
data <- left_join(data, coms, by = c('County'))

# Final data prep

data$Treat_Time[which(is.na(data$Treat_Time))] <- 0
data$Wages <- log(data$Wages*1000)
data$Wages[!is.finite(data$Wages)] <- NA
data$Wages[which(data$Wages == 0)] <- NA
data$Employees <- log(data$Employees+1)
data$Establishments <- log(data$Establishments+1)

# Adding Zillow data

zillow <- read.csv(paste0(direc, 'data/ZHVI/County_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv'))

fips <- c()

for (i in 1:nrow(zillow)) {
  
  print(i)
  
  s <- as.character(zillow$StateCodeFIPS[i])
  c <- as.character(zillow$MunicipalCodeFIPS[i])
  
  while (nchar(s) < 2) {
    
    s <- paste0('0', s)
    
  }
  
  while (nchar(c) < 3) {
    
    c <- paste0('0', c)
    
  }
  
  fips <- c(fips, paste0(s,c))
  
}

zillow$FIPS <- fips
zillow$FIPS2 <- as.integer(zillow$FIPS)

data$FIPS <- as.integer(data$GEOID)

prices <- c()
log.prices <- c()

for (i in 1:nrow(data)) {
  
  print(i)
  
  tmp <- zillow %>% filter(FIPS2 == data$FIPS[i])
  vals <- c()
  
  for (j in 1:12) {
    
    ifelse(j < 10, m <- paste0('0', j), m <- j)
    
    if (j %in% c(1, 3, 5, 7, 8, 10, 12)) {
      
      d <- 31
      
    } else if (j %in% c(4, 6, 9, 11)) {
      
      d <- 30
      
    } else if (data$Year[i] %in% c('2012', '2016', '2020')) {
      
      d <- 29
      
    } else {
      
      d <- 28
      
    }
    
    nombre <- paste0('X', data$Year[i], '.', m, '.', d)
    vals <- c(vals, tmp[1,which(colnames(tmp) == nombre)])
    
  }
  
  prices <- c(prices, mean(vals, na.rm = TRUE))
  log.prices <- c(log.prices, log(mean(vals, na.rm = TRUE)))
  
}

data$Price <- prices
data$ZHVI <- log.prices

# Demeaning outcomes by state-year (to account for state-by-year fixed effects)

data$SY <- paste0(data$State.x, data$Year)

Income2 <- c()
Wages2 <- c()
HH_Income2 <- c()
Unemployment_Rate2 <- c()
LFP2 <- c()
Employees2 <- c()
Population2 <- c()
Establishments2 <- c()
Farms2 <- c()
Social_Security2 <- c()
Mortgage2 <- c()
Price2 <- c()
ZHVI2 <- c()
J12 <- c()
J22 <- c()
J32 <- c()
J42 <- c()
J52 <- c()
J62 <- c()
J72 <- c()
J82 <- c()
J92 <- c()
J102 <- c()
J112 <- c()
J122 <- c()
J132 <- c()

for (i in 1:nrow(data)) {
  
  print(i)
  tmp <- data %>% filter(SY == data$SY[i])
  
  Income2 <- c(Income2, data$Income[i] - mean(tmp$Income, na.rm = T))
  Wages2 <- c(Wages2, data$Wages[i] - mean(tmp$Wages, na.rm = T))
  HH_Income2 <- c(HH_Income2, data$HH_Income[i] - mean(tmp$HH_Income, na.rm = T))
  Unemployment_Rate2 <- c(Unemployment_Rate2, data$Unemployment_Rate[i] - mean(tmp$Unemployment_Rate, na.rm = T))
  LFP2 <- c(LFP2, data$LFP[i] - mean(tmp$LFP, na.rm = T))
  Employees2 <- c(Employees2, data$Employees[i] - mean(tmp$Employees, na.rm = T))
  Population2 <- c(Population2, data$Population[i] - mean(tmp$Population, na.rm = T))
  Establishments2 <- c(Establishments2, data$Establishments[i] - mean(tmp$Establishments, na.rm = T))
  Farms2 <- c(Farms2, data$Farms[i] - mean(tmp$Farms, na.rm = T))
  Social_Security2 <- c(Social_Security2, data$Social_Security[i] - mean(tmp$Social_Security, na.rm = T))
  Mortgage2 <- c(Mortgage2, data$Mortgage[i] - mean(tmp$Mortgage, na.rm = T))
  Price2 <- c(Price2, data$Price[i] - mean(tmp$Price, na.rm = T))
  ZHVI2 <- c(ZHVI2, data$ZHVI[i] - mean(tmp$ZHVI, na.rm = T))
  J12 <- c(J12, data$J1[i] - mean(tmp$J1, na.rm = T))
  J22 <- c(J22, data$J2[i] - mean(tmp$J2, na.rm = T))
  J32 <- c(J32, data$J3[i] - mean(tmp$J3, na.rm = T))
  J42 <- c(J42, data$J4[i] - mean(tmp$J4, na.rm = T))
  J52 <- c(J52, data$J5[i] - mean(tmp$J5, na.rm = T))
  J62 <- c(J62, data$J6[i] - mean(tmp$J6, na.rm = T))
  J72 <- c(J72, data$J7[i] - mean(tmp$J7, na.rm = T))
  J82 <- c(J82, data$J8[i] - mean(tmp$J8, na.rm = T))
  J92 <- c(J92, data$J9[i] - mean(tmp$J9, na.rm = T))
  J102 <- c(J102, data$J10[i] - mean(tmp$J10, na.rm = T))
  J112 <- c(J112, data$J11[i] - mean(tmp$J11, na.rm = T))
  J122 <- c(J122, data$J12[i] - mean(tmp$J12, na.rm = T))
  J132 <- c(J132, data$J13[i] - mean(tmp$J13, na.rm = T))
  
}

data$Income2 <- Income2
data$Wages2 <- Wages2
data$HH_Income2 <- HH_Income2
data$Unemployment_Rate2 <- Unemployment_Rate2
data$LFP2 <- LFP2
data$Employees2 <- Employees2
data$Population2 <- Population2
data$Establishments2 <- Establishments2
data$Farms2 <- Farms2
data$Social_Security2 <- Social_Security2
data$Mortgage2 <- Mortgage2
data$Price2 <- Price2
data$ZHVI2 <- ZHVI2
data$J12 <- J12
data$J22 <- J22
data$J32 <- J32
data$J42 <- J42
data$J52 <- J52
data$J62 <- J62
data$J72 <- J72
data$J82 <- J82
data$J92 <- J92
data$J102 <- J102
data$J112 <- J112
data$J122 <- J122
data$J132 <- J132

# Remove units treated in first period (or prior)

reg.data <- data %>% filter(Treat_Time != 1)
data22 <- data %>% filter(Year < 2023)
irs.data <- data22 %>% filter(Treat_Time != 1)

# Running the staggered diff-in-diff models

sdid1 <- att_gt(yname = 'Income2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = irs.data)
sdid2 <- att_gt(yname = 'Income2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = irs.data)

sdid3 <- att_gt(yname = 'Wages2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid4 <- att_gt(yname = 'Wages2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid5 <- att_gt(yname = 'HH_Income2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid6 <- att_gt(yname = 'HH_Income2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid7 <- att_gt(yname = 'Unemployment_Rate2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid8 <- att_gt(yname = 'Unemployment_Rate2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid9 <- att_gt(yname = 'LFP2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid10 <- att_gt(yname = 'LFP2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid11 <- att_gt(yname = 'Employees2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid12 <- att_gt(yname = 'Employees2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid13 <- att_gt(yname = 'Population2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid14 <- att_gt(yname = 'Population2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid15 <- att_gt(yname = 'Establishments2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid16 <- att_gt(yname = 'Establishments2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid17 <- att_gt(yname = 'Farms2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = irs.data)
sdid18 <- att_gt(yname = 'Farms2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = irs.data)

sdid19 <- att_gt(yname = 'J12', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid20 <- att_gt(yname = 'J12', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid21 <- att_gt(yname = 'J22', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid22 <- att_gt(yname = 'J22', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid23 <- att_gt(yname = 'J32', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid24 <- att_gt(yname = 'J32', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid25 <- att_gt(yname = 'J42', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid26 <- att_gt(yname = 'J42', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid27 <- att_gt(yname = 'J52', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid28 <- att_gt(yname = 'J52', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid29 <- att_gt(yname = 'J62', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid30 <- att_gt(yname = 'J62', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid31 <- att_gt(yname = 'J72', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid32 <- att_gt(yname = 'J72', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid33 <- att_gt(yname = 'J82', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid34 <- att_gt(yname = 'J82', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid35 <- att_gt(yname = 'J92', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid36 <- att_gt(yname = 'J92', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid37 <- att_gt(yname = 'J102', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid38 <- att_gt(yname = 'J102', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid39 <- att_gt(yname = 'J112', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid40 <- att_gt(yname = 'J112', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid41 <- att_gt(yname = 'J122', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid42 <- att_gt(yname = 'J122', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid43 <- att_gt(yname = 'J132', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid44 <- att_gt(yname = 'J132', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid45 <- att_gt(yname = 'ZHVI2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data)
sdid46 <- att_gt(yname = 'ZHVI2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data)

sdid1_cs <- aggte(sdid1, type = 'dynamic', na.rm = TRUE)
sdid2_cs <- aggte(sdid2, type = 'dynamic', na.rm = TRUE)
sdid3_cs <- aggte(sdid3, type = 'dynamic', na.rm = TRUE)
sdid4_cs <- aggte(sdid4, type = 'dynamic', na.rm = TRUE)
sdid5_cs <- aggte(sdid5, type = 'dynamic', na.rm = TRUE)
sdid6_cs <- aggte(sdid6, type = 'dynamic', na.rm = TRUE)
sdid7_cs <- aggte(sdid7, type = 'dynamic', na.rm = TRUE)
sdid8_cs <- aggte(sdid8, type = 'dynamic', na.rm = TRUE)
sdid9_cs <- aggte(sdid9, type = 'dynamic', na.rm = TRUE)
sdid10_cs <- aggte(sdid10, type = 'dynamic', na.rm = TRUE)
sdid11_cs <- aggte(sdid11, type = 'dynamic', na.rm = TRUE)
sdid12_cs <- aggte(sdid12, type = 'dynamic', na.rm = TRUE)
sdid13_cs <- aggte(sdid13, type = 'dynamic', na.rm = TRUE)
sdid14_cs <- aggte(sdid14, type = 'dynamic', na.rm = TRUE)
sdid15_cs <- aggte(sdid15, type = 'dynamic', na.rm = TRUE)
sdid16_cs <- aggte(sdid16, type = 'dynamic', na.rm = TRUE)
sdid17_cs <- aggte(sdid17, type = 'dynamic', na.rm = TRUE)
sdid18_cs <- aggte(sdid18, type = 'dynamic', na.rm = TRUE)
sdid19_cs <- aggte(sdid19, type = 'dynamic', na.rm = TRUE)
sdid20_cs <- aggte(sdid20, type = 'dynamic', na.rm = TRUE)
sdid21_cs <- aggte(sdid21, type = 'dynamic', na.rm = TRUE)
sdid22_cs <- aggte(sdid22, type = 'dynamic', na.rm = TRUE)
sdid23_cs <- aggte(sdid23, type = 'dynamic', na.rm = TRUE)
sdid24_cs <- aggte(sdid24, type = 'dynamic', na.rm = TRUE)
sdid25_cs <- aggte(sdid25, type = 'dynamic', na.rm = TRUE)
sdid26_cs <- aggte(sdid26, type = 'dynamic', na.rm = TRUE)
sdid27_cs <- aggte(sdid27, type = 'dynamic', na.rm = TRUE)
sdid28_cs <- aggte(sdid28, type = 'dynamic', na.rm = TRUE)
sdid29_cs <- aggte(sdid29, type = 'dynamic', na.rm = TRUE)
sdid30_cs <- aggte(sdid30, type = 'dynamic', na.rm = TRUE)
sdid31_cs <- aggte(sdid31, type = 'dynamic', na.rm = TRUE)
sdid32_cs <- aggte(sdid32, type = 'dynamic', na.rm = TRUE)
sdid33_cs <- aggte(sdid33, type = 'dynamic', na.rm = TRUE)
sdid34_cs <- aggte(sdid34, type = 'dynamic', na.rm = TRUE)
sdid35_cs <- aggte(sdid35, type = 'dynamic', na.rm = TRUE)
sdid36_cs <- aggte(sdid36, type = 'dynamic', na.rm = TRUE)
sdid37_cs <- aggte(sdid37, type = 'dynamic', na.rm = TRUE)
sdid38_cs <- aggte(sdid38, type = 'dynamic', na.rm = TRUE)
sdid39_cs <- aggte(sdid39, type = 'dynamic', na.rm = TRUE)
sdid40_cs <- aggte(sdid40, type = 'dynamic', na.rm = TRUE)
sdid41_cs <- aggte(sdid41, type = 'dynamic', na.rm = TRUE)
sdid42_cs <- aggte(sdid42, type = 'dynamic', na.rm = TRUE)
sdid43_cs <- aggte(sdid43, type = 'dynamic', na.rm = TRUE)
sdid44_cs <- aggte(sdid44, type = 'dynamic', na.rm = TRUE)
sdid45_cs <- aggte(sdid45, type = 'dynamic', na.rm = TRUE)
sdid46_cs <- aggte(sdid46, type = 'dynamic', na.rm = TRUE)

# Viewing results

summary(sdid1_cs)
summary(sdid2_cs)
summary(sdid3_cs)
summary(sdid4_cs)
summary(sdid5_cs)
summary(sdid6_cs)
summary(sdid7_cs)
summary(sdid8_cs)
summary(sdid9_cs)
summary(sdid10_cs)
summary(sdid11_cs)
summary(sdid12_cs)
summary(sdid13_cs)
summary(sdid14_cs)
summary(sdid15_cs)
summary(sdid16_cs)
summary(sdid17_cs)
summary(sdid18_cs)
summary(sdid19_cs)
summary(sdid20_cs)
summary(sdid21_cs)
summary(sdid22_cs)
summary(sdid23_cs)
summary(sdid24_cs)
summary(sdid25_cs)
summary(sdid26_cs)
summary(sdid27_cs)
summary(sdid28_cs)
summary(sdid29_cs)
summary(sdid30_cs)
summary(sdid31_cs)
summary(sdid32_cs)
summary(sdid33_cs)
summary(sdid34_cs)
summary(sdid35_cs)
summary(sdid36_cs)
summary(sdid37_cs)
summary(sdid38_cs)
summary(sdid39_cs)
summary(sdid40_cs)
summary(sdid41_cs)
summary(sdid42_cs)
summary(sdid43_cs)
summary(sdid44_cs)
summary(sdid45_cs)
summary(sdid46_cs)

ggdid(sdid1_cs, title = 'Average Effect by Length of Exposure to Spotted Lanternfly Colonies\n\n- Average Wages (IRS) -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sdid3_cs, title = 'Average Effect by Length of Exposure to Spotted Lanternfly Colonies\n\n- Average Wages (CBP) -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sdid5_cs, title = 'Average Effect by Length of Exposure to Spotted Lanternfly Colonies\n\n- Median Household Income (ACS) -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sdid7_cs, title = 'Average Effect by Length of Exposure to Spotted Lanternfly Colonies\n\n- Unemployment Rate (ACS) -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sdid21_cs, title = 'Average Effect by Length of Exposure to Spotted Lanternfly Colonies\n\n- Jobs in Construction (ACS) -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sdid39_cs, title = 'Average Effect by Length of Exposure to Spotted Lanternfly Colonies\n\n- Jobs in Urban Amenities (ACS) -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(sdid45_cs, title = 'Average Effect by Length of Exposure to Spotted Lanternfly Colonies\n\n- House Price Index (ZHVI) -') + theme(plot.title = element_text(hjust = 0.5))

# Creating a leaflet showing counties treated by 2023

d23 <- data %>% filter(Year == 2023)
d23 <- d23[,c(1,37, 40)]
plot.dat <- left_join(co, d23, by = c('GEOID'))
plot.dat$Treat_Time <- ifelse(plot.dat$Treat_Time == 0, 0, plot.dat$Treat_Time + 2009)
plot.dat$County <- as.integer(plot.dat$GEOID)

ct.t <- c()
ct.tt <- c()

for (i in 1:nrow(plot.dat)) {
  
  if (plot.dat$County[i] %in% c(9001, 9003, 9005, 9007, 9009, 9011, 9013, 9015)) {
    
    tmp <- slf %>% filter(County == plot.dat$County[i])
    
    if (nrow(tmp) > 0) {
      
      ct.t <- c(ct.t, max(tmp$SLF, na.rm = T))
      
    } else {
      
      ct.t <- c(ct.t, 0) 
      
    }
    
    if (max(tmp$SLF, na.rm = T) == 1) {
      
      tmp <- tmp %>% filter(SLF == 1)
      ct.tt <- c(ct.tt, min(tmp$Year))
      
    } else {
      
      ct.tt <- c(ct.tt, 0)
      
    }
    
  } else {
    
    ct.t <- c(ct.t, plot.dat$Treated[i])
    ct.tt <- c(ct.tt, plot.dat$Treat_Time[i])
    
  }
  
}

plot.dat$Treated <- ct.t
plot.dat$Treat_Time <- ct.tt

pal <- colorFactor(palette = c('white', 'yellow', '#008000', 'cyan', 'blue', 'purple', 'pink', 'red', 'orange', 'brown', 'black'), domain = c(0, 2014:2023))

leaflet(plot.dat$geometry) %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = plot.dat$Treated, color = 'black', fillColor = pal(plot.dat$Treat_Time)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% addLegend('bottomright', colors = c('white', 'yellow', '#008000', 'cyan', 'blue', 'purple', 'pink', 'red', 'orange', 'brown', 'black'), labels = c('No Established Colonies', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023'), title = 'Year of Colony Establishment', opacity = 1.0)

# Creating a figure for all counties where SLF has been spotted

lyde2 <- lydemapr::lyde
lyde2 <- st_as_sf(lyde2, coords = c('longitude', 'latitude'))
lyde2 <- lyde2 %>% st_set_crs(st_crs(co))

inside <- st_within(lyde2, co)

fips <- c()

for (i in 1:ceiling(nrow(lyde2)/10000)) {
  
  print(i)
  tmp <- c()
  
  if (i < ceiling(nrow(lyde2)/10000)) {
    
    for (j in (1+(i-1)*10000):(i*10000)) {
      
      tmp <- c(tmp, co$GEOID[inside[[j]][1]])
      
    }
    
  } else {
    
    for (j in (1+(i-1)*10000):nrow(lyde2)) {
      
      tmp <- c(tmp, co$GEOID[inside[[j]][1]])
      
    }
    
  }
  
  fips <- c(fips, tmp)
  
}

lyde2$FIPS <- fips

rm(fips)
rm(inside)

spots <- c()

for (i in 1:nrow(co)) {
  
  print(i)
  
  if (co$GEOID[i] %in% lyde2$FIPS) {
    
    spots <- c(spots, 1)
    
  } else {
    
    spots <- c(spots, 0)
    
  }
  
}

co$Treated <- spots

pal2 <- colorFactor(palette = c('white', 'red4'), domain = c(0, 1))

leaflet(co$geometry) %>% addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = co$Treated, color = 'black', fillColor = pal2(co$Treated)) %>% addProviderTiles(providers$CartoDB.Positron)

# Matching counties on pre-treatment outcomes with never-treated units

treat.df <- data %>% filter(Treated == 1)
con.df <- data %>% filter(Treated == 0)

treated.units <- unique(treat.df$ID)
control.units <- unique(con.df$ID)

treat.keep <- c()
con.keep <- c()

for (i in treated.units) {
  
  print(i)
  
  tmp <- treat.df %>% filter(ID == i)
  tmp <- tmp %>% filter(Time < tmp$Treat_Time[1])
  tmp2 <- con.df %>% filter(Time < tmp$Treat_Time[1])
  
  con_ids <- unique(tmp2$ID)
  con_pop <- c()
  con_inc <- c()
  con_une <- c()
  con_col <- c()
  
  for (j in con_ids) {
    
    tmp3 <- tmp2 %>% filter(ID == j)
    con_pop <- c(con_pop, mean(tmp3$Population))
    con_inc <- c(con_inc, mean(tmp3$HH_Income))
    con_une <- c(con_une, mean(tmp3$Unemployment_Rate))
    con_col <- c(con_col, mean(tmp3$College_Pct))
    
  }
  
  ref_pop <- mean(tmp$Population)
  ref_inc <- mean(tmp$HH_Income)
  ref_une <- mean(tmp$Unemployment_Rate)
  ref_col <- mean(tmp$College_Pct)
  
  good_pop <- con_ids[which(abs(con_pop - ref_pop) < (.05 * ref_pop))]
  good_inc <- con_ids[which(abs(con_inc - ref_inc) < (.05 * ref_inc))]
  good_une <- con_ids[which(abs(con_une - ref_une) < 0.5)]
  good_col <- con_ids[which(abs(con_col - ref_col) < .05)]
  
  keep_cons <- good_pop[which(good_pop %in% good_pop)]
  keep_cons <- keep_cons[which(keep_cons %in% good_inc)]
  keep_cons <- keep_cons[which(keep_cons %in% good_une)]
  keep_cons <- keep_cons[which(keep_cons %in% good_col)]
  
  con.keep <- c(con.keep, keep_cons)
  con.keep <- unique(con.keep)
  
  if (length(keep_cons) > 0) {
    
    treat.keep <- c(treat.keep, i)
    
  }
  
}

treat.dfx <- data %>% filter(ID %in% treat.keep)
con.dfx <- data %>% filter(ID %in% con.keep)
matched.data <- rbind(treat.dfx, con.dfx)

# Final data prep

matched.data22 <- matched.data %>% filter(Year < 2023)

# Remove units treated in first period (or prior)

reg.data2 <- matched.data %>% filter(Treat_Time != 1)
irs.data2 <- matched.data22 %>% filter(Treat_Time != 1)

# Running the staggered diff-in-diff for solar without controls

msdid1 <- att_gt(yname = 'Income2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = irs.data2)
msdid2 <- att_gt(yname = 'Income2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = irs.data2)

msdid3 <- att_gt(yname = 'Wages2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid4 <- att_gt(yname = 'Wages2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid5 <- att_gt(yname = 'HH_Income2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid6 <- att_gt(yname = 'HH_Income2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid7 <- att_gt(yname = 'Unemployment_Rate2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid8 <- att_gt(yname = 'Unemployment_Rate2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid9 <- att_gt(yname = 'LFP2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid10 <- att_gt(yname = 'LFP2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid11 <- att_gt(yname = 'Employees2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid12 <- att_gt(yname = 'Employees2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid13 <- att_gt(yname = 'Population2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid14 <- att_gt(yname = 'Population2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid15 <- att_gt(yname = 'Establishments2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid16 <- att_gt(yname = 'Establishments2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid17 <- att_gt(yname = 'Farms2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = irs.data2)
msdid18 <- att_gt(yname = 'Farms2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = irs.data2)

msdid19 <- att_gt(yname = 'J12', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid20 <- att_gt(yname = 'J12', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid21 <- att_gt(yname = 'J22', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid22 <- att_gt(yname = 'J22', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid23 <- att_gt(yname = 'J32', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid24 <- att_gt(yname = 'J32', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid25 <- att_gt(yname = 'J42', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid26 <- att_gt(yname = 'J42', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid27 <- att_gt(yname = 'J52', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid28 <- att_gt(yname = 'J52', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid29 <- att_gt(yname = 'J62', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid30 <- att_gt(yname = 'J62', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid31 <- att_gt(yname = 'J72', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid32 <- att_gt(yname = 'J72', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid33 <- att_gt(yname = 'J82', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid34 <- att_gt(yname = 'J82', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid35 <- att_gt(yname = 'J92', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid36 <- att_gt(yname = 'J92', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid37 <- att_gt(yname = 'J102', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid38 <- att_gt(yname = 'J102', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid39 <- att_gt(yname = 'J112', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid40 <- att_gt(yname = 'J112', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid41 <- att_gt(yname = 'J122', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid42 <- att_gt(yname = 'J122', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid43 <- att_gt(yname = 'J132', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid44 <- att_gt(yname = 'J132', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid45 <- att_gt(yname = 'ZHVI2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, clustervars = c('PreliminaryCZ2020'), data = reg.data2)
msdid46 <- att_gt(yname = 'ZHVI2', tname = 'Time', idname = 'ID', gname = 'Treat_Time', xformla = ~ College_Pct + Black + Hispanic + Elderly + Housing_Units, control_group = 'notyettreated', clustervars = c('PreliminaryCZ2020'), data = reg.data2)

msdid1_cs <- aggte(msdid1, type = 'dynamic', na.rm = TRUE)
msdid2_cs <- aggte(msdid2, type = 'dynamic', na.rm = TRUE)
msdid3_cs <- aggte(msdid3, type = 'dynamic', na.rm = TRUE)
msdid4_cs <- aggte(msdid4, type = 'dynamic', na.rm = TRUE)
msdid5_cs <- aggte(msdid5, type = 'dynamic', na.rm = TRUE)
msdid6_cs <- aggte(msdid6, type = 'dynamic', na.rm = TRUE)
msdid7_cs <- aggte(msdid7, type = 'dynamic', na.rm = TRUE)
msdid8_cs <- aggte(msdid8, type = 'dynamic', na.rm = TRUE)
msdid9_cs <- aggte(msdid9, type = 'dynamic', na.rm = TRUE)
msdid10_cs <- aggte(msdid10, type = 'dynamic', na.rm = TRUE)
msdid11_cs <- aggte(msdid11, type = 'dynamic', na.rm = TRUE)
msdid12_cs <- aggte(msdid12, type = 'dynamic', na.rm = TRUE)
msdid13_cs <- aggte(msdid13, type = 'dynamic', na.rm = TRUE)
msdid14_cs <- aggte(msdid14, type = 'dynamic', na.rm = TRUE)
msdid15_cs <- aggte(msdid15, type = 'dynamic', na.rm = TRUE)
msdid16_cs <- aggte(msdid16, type = 'dynamic', na.rm = TRUE)
msdid17_cs <- aggte(msdid17, type = 'dynamic', na.rm = TRUE)
msdid18_cs <- aggte(msdid18, type = 'dynamic', na.rm = TRUE)
msdid19_cs <- aggte(msdid19, type = 'dynamic', na.rm = TRUE)
msdid20_cs <- aggte(msdid20, type = 'dynamic', na.rm = TRUE)
msdid21_cs <- aggte(msdid21, type = 'dynamic', na.rm = TRUE)
msdid22_cs <- aggte(msdid22, type = 'dynamic', na.rm = TRUE)
msdid23_cs <- aggte(msdid23, type = 'dynamic', na.rm = TRUE)
msdid24_cs <- aggte(msdid24, type = 'dynamic', na.rm = TRUE)
msdid25_cs <- aggte(msdid25, type = 'dynamic', na.rm = TRUE)
msdid26_cs <- aggte(msdid26, type = 'dynamic', na.rm = TRUE)
msdid27_cs <- aggte(msdid27, type = 'dynamic', na.rm = TRUE)
msdid28_cs <- aggte(msdid28, type = 'dynamic', na.rm = TRUE)
msdid29_cs <- aggte(msdid29, type = 'dynamic', na.rm = TRUE)
msdid30_cs <- aggte(msdid30, type = 'dynamic', na.rm = TRUE)
msdid31_cs <- aggte(msdid31, type = 'dynamic', na.rm = TRUE)
msdid32_cs <- aggte(msdid32, type = 'dynamic', na.rm = TRUE)
msdid33_cs <- aggte(msdid33, type = 'dynamic', na.rm = TRUE)
msdid34_cs <- aggte(msdid34, type = 'dynamic', na.rm = TRUE)
msdid35_cs <- aggte(msdid35, type = 'dynamic', na.rm = TRUE)
msdid36_cs <- aggte(msdid36, type = 'dynamic', na.rm = TRUE)
msdid37_cs <- aggte(msdid37, type = 'dynamic', na.rm = TRUE)
msdid38_cs <- aggte(msdid38, type = 'dynamic', na.rm = TRUE)
msdid39_cs <- aggte(msdid39, type = 'dynamic', na.rm = TRUE)
msdid40_cs <- aggte(msdid40, type = 'dynamic', na.rm = TRUE)
msdid41_cs <- aggte(msdid41, type = 'dynamic', na.rm = TRUE)
msdid42_cs <- aggte(msdid42, type = 'dynamic', na.rm = TRUE)
msdid43_cs <- aggte(msdid43, type = 'dynamic', na.rm = TRUE)
msdid44_cs <- aggte(msdid44, type = 'dynamic', na.rm = TRUE)
msdid45_cs <- aggte(msdid45, type = 'dynamic', na.rm = TRUE)
msdid46_cs <- aggte(msdid46, type = 'dynamic', na.rm = TRUE)

# Viewing results

summary(msdid1_cs)
summary(msdid2_cs)
summary(msdid3_cs)
summary(msdid4_cs)
summary(msdid5_cs)
summary(msdid6_cs)
summary(msdid7_cs)
summary(msdid8_cs)
summary(msdid9_cs)
summary(msdid10_cs)
summary(msdid11_cs)
summary(msdid12_cs)
summary(msdid13_cs)
summary(msdid14_cs)
summary(msdid15_cs)
summary(msdid16_cs)
summary(msdid17_cs)
summary(msdid18_cs)
summary(msdid19_cs)
summary(msdid20_cs)
summary(msdid21_cs)
summary(msdid22_cs)
summary(msdid23_cs)
summary(msdid24_cs)
summary(msdid25_cs)
summary(msdid26_cs)
summary(msdid27_cs)
summary(msdid28_cs)
summary(msdid29_cs)
summary(msdid30_cs)
summary(msdid31_cs)
summary(msdid32_cs)
summary(msdid33_cs)
summary(msdid34_cs)
summary(msdid35_cs)
summary(msdid36_cs)
summary(msdid37_cs)
summary(msdid38_cs)
summary(msdid39_cs)
summary(msdid40_cs)
summary(msdid41_cs)
summary(msdid42_cs)
summary(msdid43_cs)
summary(msdid44_cs)
summary(msdid45_cs)
summary(msdid46_cs)

# Creating select event study type plots

ggdid(msdid1_cs, title = 'Average Effect by Length of Exposure\n\n- Spotted Lanternfly -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(msdid3_cs, title = 'Average Effect by Length of Exposure\n\n- Spotted Lanternfly -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(msdid5_cs, title = 'Average Effect by Length of Exposure\n\n- Spotted Lanternfly -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(msdid7_cs, title = 'Average Effect by Length of Exposure\n\n- Spotted Lanternfly -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(msdid9_cs, title = 'Average Effect by Length of Exposure\n\n- Spotted Lanternfly -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(msdid11_cs, title = 'Average Effect by Length of Exposure\n\n- Spotted Lanternfly -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(msdid13_cs, title = 'Average Effect by Length of Exposure\n\n- Spotted Lanternfly -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(msdid15_cs, title = 'Average Effect by Length of Exposure\n\n- Spotted Lanternfly -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(msdid17_cs, title = 'Average Effect by Length of Exposure\n\n- Spotted Lanternfly -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(msdid21_cs, title = 'Average Effect by Length of Exposure\n\n- Spotted Lanternfly -') + theme(plot.title = element_text(hjust = 0.5))
ggdid(msdid45_cs, title = 'Average Effect by Length of Exposure\n\n- Spotted Lanternfly -') + theme(plot.title = element_text(hjust = 0.5))

# Storing results

atandt <- cbind(c(sdid1_cs$overall.att, sdid3_cs$overall.att, sdid5_cs$overall.att, sdid7_cs$overall.att,
                  sdid9_cs$overall.att, sdid11_cs$overall.att, sdid13_cs$overall.att, sdid15_cs$overall.att,
                  sdid17_cs$overall.att, sdid19_cs$overall.att, sdid21_cs$overall.att, sdid23_cs$overall.att,
                  sdid25_cs$overall.att, sdid27_cs$overall.att, sdid29_cs$overall.att, sdid31_cs$overall.att,
                  sdid33_cs$overall.att, sdid35_cs$overall.att, sdid37_cs$overall.att, sdid39_cs$overall.att,
                  sdid41_cs$overall.att, sdid43_cs$overall.att, sdid45_cs$overall.att))

serrs <- cbind(c(sdid1_cs$overall.se, sdid3_cs$overall.se, sdid5_cs$overall.se, sdid7_cs$overall.se,
                 sdid9_cs$overall.se, sdid11_cs$overall.se, sdid13_cs$overall.se, sdid15_cs$overall.se,
                 sdid17_cs$overall.se, sdid19_cs$overall.se, sdid21_cs$overall.se, sdid23_cs$overall.se,
                 sdid25_cs$overall.se, sdid27_cs$overall.se, sdid29_cs$overall.se, sdid31_cs$overall.se,
                 sdid33_cs$overall.se, sdid35_cs$overall.se, sdid37_cs$overall.se, sdid39_cs$overall.se,
                 sdid41_cs$overall.se, sdid43_cs$overall.se, sdid45_cs$overall.se))

t.stats <- atandt / serrs

p.10 <- matrix(as.integer(abs(t.stats) > 1.645), 23, 1)
p.05 <- matrix(as.integer(abs(t.stats) > 1.960), 23, 1)
p.01 <- matrix(as.integer(abs(t.stats) > 2.576), 23, 1)

stars <- matrix(0, 23, 1)

for (i in 1:23) {
  
  if (p.01[i,1] == 1) {
    
    stars[i,1] <- 3
    
  } else if (p.05[i,1] == 1) {
    
    stars[i,1] <- 2
    
  } else if (p.10[i,1] == 1) {
    
    stars[i,1] <- 1
    
  }
  
}

atandt <- round(atandt, 3)
serrs <- round(serrs, 3)

res.df <- as.data.frame(cbind(atandt, serrs, stars))
colnames(res.df) <- c('Coef', 'Serr', 'Sig')

write.csv(res.df, paste0(direc, 'results/full_coefs.txt'), row.names = FALSE)

atandt <- cbind(c(sdid2_cs$overall.att, sdid4_cs$overall.att, sdid6_cs$overall.att, sdid8_cs$overall.att,
                  sdid10_cs$overall.att, sdid12_cs$overall.att, sdid14_cs$overall.att, sdid16_cs$overall.att,
                  sdid18_cs$overall.att, sdid20_cs$overall.att, sdid22_cs$overall.att, sdid23_cs$overall.att,
                  sdid26_cs$overall.att, sdid28_cs$overall.att, sdid30_cs$overall.att, sdid32_cs$overall.att,
                  sdid34_cs$overall.att, sdid36_cs$overall.att, sdid38_cs$overall.att, sdid40_cs$overall.att,
                  sdid42_cs$overall.att, sdid44_cs$overall.att, sdid46_cs$overall.att))

serrs <- cbind(c(sdid2_cs$overall.se, sdid4_cs$overall.se, sdid6_cs$overall.se, sdid8_cs$overall.se,
                 sdid10_cs$overall.se, sdid12_cs$overall.se, sdid14_cs$overall.se, sdid16_cs$overall.se,
                 sdid18_cs$overall.se, sdid20_cs$overall.se, sdid22_cs$overall.se, sdid24_cs$overall.se,
                 sdid26_cs$overall.se, sdid28_cs$overall.se, sdid30_cs$overall.se, sdid32_cs$overall.se,
                 sdid34_cs$overall.se, sdid36_cs$overall.se, sdid38_cs$overall.se, sdid40_cs$overall.se,
                 sdid42_cs$overall.se, sdid44_cs$overall.se, sdid46_cs$overall.se))

t.stats <- atandt / serrs

p.10 <- matrix(as.integer(abs(t.stats) > 1.645), 23, 1)
p.05 <- matrix(as.integer(abs(t.stats) > 1.960), 23, 1)
p.01 <- matrix(as.integer(abs(t.stats) > 2.576), 23, 1)

stars <- matrix(0, 23, 1)

for (i in 1:23) {
  
  if (p.01[i,1] == 1) {
    
    stars[i,1] <- 3
    
  } else if (p.05[i,1] == 1) {
    
    stars[i,1] <- 2
    
  } else if (p.10[i,1] == 1) {
    
    stars[i,1] <- 1
    
  }
  
}

atandt <- round(atandt, 3)
serrs <- round(serrs, 3)

res.df <- as.data.frame(cbind(atandt, serrs, stars))
colnames(res.df) <- c('Coef', 'Serr', 'Sig')

write.csv(res.df, paste0(direc, 'results/full_coefs_2.txt'), row.names = FALSE)

atandt <- cbind(c(msdid1_cs$overall.att, msdid3_cs$overall.att, msdid5_cs$overall.att, msdid7_cs$overall.att,
                  msdid9_cs$overall.att, msdid11_cs$overall.att, msdid13_cs$overall.att, msdid15_cs$overall.att,
                  msdid17_cs$overall.att, msdid19_cs$overall.att, msdid21_cs$overall.att, msdid23_cs$overall.att,
                  msdid25_cs$overall.att, msdid27_cs$overall.att, msdid29_cs$overall.att, msdid31_cs$overall.att,
                  msdid33_cs$overall.att, msdid35_cs$overall.att, msdid37_cs$overall.att, msdid39_cs$overall.att,
                  msdid41_cs$overall.att, msdid43_cs$overall.att, msdid45_cs$overall.att))

serrs <- cbind(c(msdid1_cs$overall.se, msdid3_cs$overall.se, msdid5_cs$overall.se, msdid7_cs$overall.se,
                 msdid9_cs$overall.se, msdid11_cs$overall.se, msdid13_cs$overall.se, msdid15_cs$overall.se,
                 msdid17_cs$overall.se, msdid19_cs$overall.se, msdid21_cs$overall.se, msdid23_cs$overall.se,
                 msdid25_cs$overall.se, msdid27_cs$overall.se, msdid29_cs$overall.se, msdid31_cs$overall.se,
                 msdid33_cs$overall.se, msdid35_cs$overall.se, msdid37_cs$overall.se, msdid39_cs$overall.se,
                 msdid41_cs$overall.se, msdid43_cs$overall.se, msdid45_cs$overall.se))

t.stats <- atandt / serrs

p.10 <- matrix(as.integer(abs(t.stats) > 1.645), 23, 1)
p.05 <- matrix(as.integer(abs(t.stats) > 1.960), 23, 1)
p.01 <- matrix(as.integer(abs(t.stats) > 2.576), 23, 1)

stars <- matrix(0, 23, 1)

for (i in 1:23) {
  
  if (p.01[i,1] == 1) {
    
    stars[i,1] <- 3
    
  } else if (p.05[i,1] == 1) {
    
    stars[i,1] <- 2
    
  } else if (p.10[i,1] == 1) {
    
    stars[i,1] <- 1
    
  }
  
}

atandt <- round(atandt, 3)
serrs <- round(serrs, 3)

res.df <- as.data.frame(cbind(atandt, serrs, stars))
colnames(res.df) <- c('Coef', 'Serr', 'Sig')

write.csv(res.df, paste0(direc, 'results/matched_coefs.txt'), row.names = FALSE)

atandt <- cbind(c(msdid2_cs$overall.att, msdid4_cs$overall.att, msdid6_cs$overall.att, msdid8_cs$overall.att,
                  msdid10_cs$overall.att, msdid12_cs$overall.att, msdid14_cs$overall.att, msdid16_cs$overall.att,
                  msdid18_cs$overall.att, msdid20_cs$overall.att, msdid22_cs$overall.att, msdid23_cs$overall.att,
                  msdid26_cs$overall.att, msdid28_cs$overall.att, msdid30_cs$overall.att, msdid32_cs$overall.att,
                  msdid34_cs$overall.att, msdid36_cs$overall.att, msdid38_cs$overall.att, msdid40_cs$overall.att,
                  msdid42_cs$overall.att, msdid44_cs$overall.att, msdid46_cs$overall.att))

serrs <- cbind(c(msdid2_cs$overall.se, msdid4_cs$overall.se, msdid6_cs$overall.se, msdid8_cs$overall.se,
                 msdid10_cs$overall.se, msdid12_cs$overall.se, msdid14_cs$overall.se, msdid16_cs$overall.se,
                 msdid18_cs$overall.se, msdid20_cs$overall.se, msdid22_cs$overall.se, msdid24_cs$overall.se,
                 msdid26_cs$overall.se, msdid28_cs$overall.se, msdid30_cs$overall.se, msdid32_cs$overall.se,
                 msdid34_cs$overall.se, msdid36_cs$overall.se, msdid38_cs$overall.se, msdid40_cs$overall.se,
                 msdid42_cs$overall.se, msdid44_cs$overall.se, msdid46_cs$overall.se))

t.stats <- atandt / serrs

p.10 <- matrix(as.integer(abs(t.stats) > 1.645), 23, 1)
p.05 <- matrix(as.integer(abs(t.stats) > 1.960), 23, 1)
p.01 <- matrix(as.integer(abs(t.stats) > 2.576), 23, 1)

stars <- matrix(0, 23, 1)

for (i in 1:23) {
  
  if (p.01[i,1] == 1) {
    
    stars[i,1] <- 3
    
  } else if (p.05[i,1] == 1) {
    
    stars[i,1] <- 2
    
  } else if (p.10[i,1] == 1) {
    
    stars[i,1] <- 1
    
  }
  
}

atandt <- round(atandt, 3)
serrs <- round(serrs, 3)

res.df <- as.data.frame(cbind(atandt, serrs, stars))
colnames(res.df) <- c('Coef', 'Serr', 'Sig')

write.csv(res.df, paste0(direc, 'results/matched_coefs_2.txt'), row.names = FALSE)

# Data prep for regressions for change in forest cover from 2010 through 2022

trees <- st_read(paste0(direc, 'data/DRYAD/eastern-us-counties-d4st.gpkg'))
trees$STATECO <- as.integer(trees$STATECO)

slfx <- slf %>% filter(Year < 2022)

treats <- c()
intents <- c()

for (i in 1:nrow(trees)) {
  
  print(i)
  
  tmp <- slfx %>% filter(County == trees$STATECO[i])
  tmp <- tmp %>% filter(SLF > 0)
  
  if (nrow(tmp) == 0) {
    
    treats <- c(treats, 0)
    intents <- c(intents, 0)
    
  } else {
    
    tmp <- tmp %>% filter()
    
    treats <- c(treats, 1)
    intents <- c(intents, 2022 - min(tmp$Year))
    
  }
  
}

trees$SLF <- treats
trees$SLFX <- intents

cp <- c()
bl <- c()
hi <- c()
el <- c()
hu <- c()

data_xxx <- data %>% filter(Year %in% c(2010, 2022))

for (i in 1:nrow(trees)) {
  
  print(i)
  tmp <- data_xxx %>% filter(Year %in% c(2010, 2022)) %>% filter(County == trees$STATECO[i])
  cp <- c(cp, tmp$College_Pct[2] - tmp$College_Pct[1])
  bl <- c(bl, tmp$Black[2] - tmp$Black[1])
  hi <- c(hi, tmp$Hispanic[2] - tmp$Hispanic[1])
  el <- c(el, tmp$Elderly[2] - tmp$Elderly[1])
  hu <- c(hu, tmp$Housing_Units[2] - tmp$Housing_Units[1])
  
}

trees$College_Pct <- cp
trees$Black <- bl
trees$Hispanic <- hi
trees$Elderly <- el
trees$Housing_Units <- hu

# Running regressions for the effect of SLF on forest loss

tree.mod1 <- lm(pct_d4st_perm ~ SLF + College_Pct + Black + Hispanic + Elderly + Housing_Units + pct_forest + factor(state), data = trees)
tree.mod2 <- lm(pct_forest_10_d4st_perm ~ SLF + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(state), data = trees)

tree.mod3 <- lm(pct_d4st_perm ~ SLFX + College_Pct + Black + Hispanic + Elderly + Housing_Units + pct_forest + factor(state), data = trees)
tree.mod4 <- lm(pct_forest_10_d4st_perm ~ SLFX + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(state), data = trees)

tree.mod1x <- coeftest(tree.mod1, vcov = vcovCL, cluster = ~state)
tree.mod2x <- coeftest(tree.mod2, vcov = vcovCL, cluster = ~state)

tree.mod3x <- coeftest(tree.mod3, vcov = vcovCL, cluster = ~state)
tree.mod4x <- coeftest(tree.mod4, vcov = vcovCL, cluster = ~state)

write.csv(stargazer(tree.mod1, tree.mod2, tree.mod3, tree.mod4, type = 'text', omit = c('state'), omit.stat = c('ser', 'f')), paste0(direc, 'results/trees.txt'))

write.csv(stargazer(tree.mod1x, tree.mod2x, tree.mod3x, tree.mod4x, type = 'text', omit = c('state')), paste0(direc, 'results/trees_robust.txt'))

# Creating a NLCD data set on land use changes

nlcd <- read.csv(paste0(direc, 'data/NLCD/county_level_proportions_2001_2011_2021.csv'))
nlcd <- nlcd %>% filter(Year > 2010)

cp <- c()
bl <- c()
hi <- c()
el <- c()
hu <- c()

data_xxx <- data %>% filter(Year %in% c(2011, 2021))

for (i in 1:nrow(nlcd)) {
  
  print(i)
  tmp <- data_xxx %>% filter(Year == nlcd$Year[i]) %>% filter(FIPS == nlcd$County[i])
  cp <- c(cp, tmp$College_Pct[1])
  bl <- c(bl, tmp$Black[1])
  hi <- c(hi, tmp$Hispanic[1])
  el <- c(el, tmp$Elderly[1])
  hu <- c(hu, tmp$Housing_Units[1])
  
}

nlcd$College_Pct <- cp
nlcd$Black <- bl
nlcd$Hispanic <- hi
nlcd$Elderly <- el
nlcd$Housing_Units <- hu

nlcd <- arrange(nlcd, desc(Year), County)

nlcd_xxx <- nlcd[1:3108,] - nlcd[3109:nrow(nlcd),]
nlcd_xxx$County <- nlcd$County[1:3108]

slfx <- slf %>% filter(Year < 2021)

treats <- c()
intents <- c()

for (i in 1:nrow(nlcd_xxx)) {
  
  print(i)
  
  tmp <- slfx %>% filter(County == nlcd_xxx$County[i])
  tmp <- tmp %>% filter(SLF > 0)
  
  if (nrow(tmp) == 0) {
    
    treats <- c(treats, 0)
    intents <- c(intents, 0)
    
  } else {
    
    tmp <- tmp %>% filter()
    
    treats <- c(treats, 1)
    intents <- c(intents, 2021 - min(tmp$Year))
    
  }
  
}

nlcd_xxx$SLF <- treats
nlcd_xxx$SLFX <- intents
nlcd_xxx$State <- round(nlcd_xxx$County / 1000, 0)

# Running regressions for the effect of SLF on land cover per NLCD

nlcd1 <- lm(Development ~ SLF + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd2 <- lm(Forests ~ SLF + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd3 <- lm(Agriculture ~ SLF + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd4 <- lm(Shrublands ~ SLF + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd5 <- lm(Grasslands ~ SLF + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd6 <- lm(Wetlands ~ SLF + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd7 <- lm(Water ~ SLF + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd8 <- lm(Barren ~ SLF + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)

nlcd12 <- lm(Development ~ SLFX + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd22 <- lm(Forests ~ SLFX + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd32 <- lm(Agriculture ~ SLFX + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd42 <- lm(Shrublands ~ SLFX + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd52 <- lm(Grasslands ~ SLFX + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd62 <- lm(Wetlands ~ SLFX + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd72 <- lm(Water ~ SLFX + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
nlcd82 <- lm(Barren ~ SLFX + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)

nlcd1x <- coeftest(nlcd1, vcov = vcovCL, cluster = ~State)
nlcd2x <- coeftest(nlcd2, vcov = vcovCL, cluster = ~State)
nlcd3x <- coeftest(nlcd3, vcov = vcovCL, cluster = ~State)
nlcd4x <- coeftest(nlcd4, vcov = vcovCL, cluster = ~State)
nlcd5x <- coeftest(nlcd5, vcov = vcovCL, cluster = ~State)
nlcd6x <- coeftest(nlcd6, vcov = vcovCL, cluster = ~State)
nlcd7x <- coeftest(nlcd7, vcov = vcovCL, cluster = ~State)
nlcd8x <- coeftest(nlcd8, vcov = vcovCL, cluster = ~State)

nlcd12x <- coeftest(nlcd12, vcov = vcovCL, cluster = ~State)
nlcd22x <- coeftest(nlcd22, vcov = vcovCL, cluster = ~State)
nlcd32x <- coeftest(nlcd32, vcov = vcovCL, cluster = ~State)
nlcd42x <- coeftest(nlcd42, vcov = vcovCL, cluster = ~State)
nlcd52x <- coeftest(nlcd52, vcov = vcovCL, cluster = ~State)
nlcd62x <- coeftest(nlcd62, vcov = vcovCL, cluster = ~State)
nlcd72x <- coeftest(nlcd72, vcov = vcovCL, cluster = ~State)
nlcd82x <- coeftest(nlcd82, vcov = vcovCL, cluster = ~State)

write.csv(stargazer(nlcd1, nlcd2, nlcd3, nlcd4, nlcd5, nlcd6, nlcd7, nlcd8, type = 'text', omit = c('State'), omit.stat = c('ser', 'f')), paste0(direc, 'results/nlcd_slf.txt'))

write.csv(stargazer(nlcd1x, nlcd2x, nlcd3x, nlcd4x, nlcd5x, nlcd6x, nlcd7x, nlcd8x, type = 'text', omit = c('State')), paste0(direc, 'results/nlcd_slf_robust.txt'))

write.csv(stargazer(nlcd12, nlcd22, nlcd32, nlcd42, nlcd52, nlcd62, nlcd72, nlcd82, type = 'text', omit = c('State'), omit.stat = c('ser', 'f')), paste0(direc, 'results/nlcd_slfx.txt'))

write.csv(stargazer(nlcd12x, nlcd22x, nlcd32x, nlcd42x, nlcd52x, nlcd62x, nlcd72x, nlcd82x, type = 'text', omit = c('State')), paste0(direc, 'results/nlcd_slfx_robust.txt'))

# Fixed effects regressions using the NLCD data

nlcd_xxx$SLFLS <- as.factor(nlcd_xxx$SLFX)
nlcd_xxx$SLFLS <- relevel(nlcd_xxx$SLFLS, ref = 1)

xnlcd1 <- lm(Development ~ SLFLS + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
xnlcd2 <- lm(Forests ~ SLFLS + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
xnlcd3 <- lm(Agriculture ~ SLFLS + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
xnlcd4 <- lm(Shrublands ~ SLFLS + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
xnlcd5 <- lm(Grasslands ~ SLFLS + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
xnlcd6 <- lm(Wetlands ~ SLFLS + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
xnlcd7 <- lm(Water ~ SLFLS + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)
xnlcd8 <- lm(Barren ~ SLFLS + College_Pct + Black + Hispanic + Elderly + Housing_Units + factor(State), data = nlcd_xxx)

xnlcd1x <- coeftest(xnlcd1, vcov = vcovCL, cluster = ~State)
xnlcd2x <- coeftest(xnlcd2, vcov = vcovCL, cluster = ~State)
xnlcd3x <- coeftest(xnlcd3, vcov = vcovCL, cluster = ~State)
xnlcd4x <- coeftest(xnlcd4, vcov = vcovCL, cluster = ~State)
xnlcd5x <- coeftest(xnlcd5, vcov = vcovCL, cluster = ~State)
xnlcd6x <- coeftest(xnlcd6, vcov = vcovCL, cluster = ~State)
xnlcd7x <- coeftest(xnlcd7, vcov = vcovCL, cluster = ~State)
xnlcd8x <- coeftest(xnlcd8, vcov = vcovCL, cluster = ~State)

write.csv(stargazer(xnlcd1, xnlcd2, xnlcd3, xnlcd4, xnlcd5, xnlcd6, xnlcd7, xnlcd8, type = 'text', omit = c('State'), omit.stat = c('ser', 'f')), paste0(direc, 'results/nlcd_slf_fe.txt'))

write.csv(stargazer(xnlcd1x, xnlcd2x, xnlcd3x, xnlcd4x, xnlcd5x, xnlcd6x, xnlcd7x, xnlcd8x, type = 'text', omit = c('State')), paste0(direc, 'results/nlcd_slf_fe_robust.txt'))

# Summary statistics

data.keepers <- c('SLF', 'Income', 'Wages', 'HH_Income', 'Unemployment_Rate', 'LFP', 'Population', 'Employees',
                  'Establishments', 'Farms', 'ZHVI', 'College_Pct', 'Black_Pct', 'Hispanic_Pct', 'Elderly', 'Housing_Units')

sumdat1 <- data[,which(colnames(data) %in% data.keepers)]
sumdat1 <- sumdat1[,c(10,11,14,4,3,2,1,13,15,12,16,6,7,8,5,9)]

colnames(sumdat1) <- c('Spotted Lanternfly Colony', 'Wages (IRS; $)', 'Wages (CBP; $)', 'Median Household Income ($)', 'Unemployment Rate',
                       'Labor Force Size', 'Population', 'Employees', 'Establishments', 'Farms', 'House Price Index ($)', 'College Graduates (%)',
                       'Black Population (%)', 'Hispanic Population (%)', 'Elderly Population (%)', 'Housing Units')

sumdat2 <- nlcd[,3:10]
sumdat2 <- sumdat2[,c(2,4,7,5,6,8,1,3)]

sumdat3 <- trees[,5:7]
sumdat3 <- sumdat3[,c(1,3,2)]

datasummary_skim(sumdat1, fmt = '%.3f')
datasummary_skim(sumdat2, fmt = '%.3f')
datasummary_skim(sumdat3, fmt = '%.3f')

# BoE annual wage losses in 2023 USD

boecos <- counties() %>% filter(GEOID < 57000) %>% filter(!STATEFP %in% c('02', '15'))

boetreats <- c()
boewages <- c()
boeemps <- c()

for (i in 1:nrow(boecos)) {
  
  print(i)
  tmp <- data[which(data$GEOID == boecos$GEOID[i]),]
  boetreats <- c(boetreats, max(tmp$SLF))
  
  if (2023 %in% tmp$Year) {
    
    boewages <- c(boewages, exp(tmp$Wages[which(tmp$Year == 2023)]))
    boeemps <- c(boeemps, tmp$Employees[which(tmp$Year == 2023)])
    
  } else {
    
    boewages <- c(boewages, NA)
    boeemps <- c(boeemps, NA)
    
  }
  
}

boecos$SLF <- boetreats
boecos$Wages <- boewages
boecos$Employees <- boeemps
boecos$TOT_WAGES <- boecos$Wages * boecos$Employees

boecos2 <- boecos %>% filter(SLF == 1)

losses <- .011 * sum(boecos2$TOT_WAGES, na.rm = TRUE) * 1000

# Reference for the lydemapr library and data

# De Bona, S., L. Barringer, P. Kurtz, J. Losiewicz, G.R. Parra, & M.R. Helmus. lydemapr: an R package to track the spread of the invasive spotted lanternfly (Lycorma delicatula, White 1845) (Hemiptera, Fulgoridae) in the United States. NeoBiota 86: 151-168. https://doi.org/10.3897/neobiota.86.101471

# https://www.mrlc.gov/data?f%5B0%5D=project_tax_term_term_parents_tax_term_name%3AAnnual%20NLCD

# Notes on estimated effects

# income < 0
# wages  < 0
# hh_inc = 0
# unempl < 0
# lfprat = 0
# employ > 0
# popula = 0
# establ = 0
# farms  = 0

# J2 > 0  [Construction]
# J11 > 0 [Arts, entertainment, and recreation, and accommodation and food services]
# ow = 0

# forest loss > 0

# jobs are either migrant construction workers or low paying service jobs for locals who were looking but unemployed

# developed land increases potentially because prematurely deforested land is (sold and) converted to development
# this leads to more urban amenities and higher house prices

# overall, SLF represents perhaps a small negative shock on natural amenities that leads to an increase in urban amenities and a net gain in total amenities for a location

