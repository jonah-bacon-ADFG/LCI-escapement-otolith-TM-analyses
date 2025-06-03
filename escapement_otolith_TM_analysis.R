library(readxl)
library(tidyverse)

getwd()
setwd("O:/DCF/SALMON/OTOLITH_STUDIES/2_ANALYSES_ESCAP/2018-2022")

raw.df <- read_xlsx(path = "data/1_raw/2018-2022 Final PINK ESC Reads from SEMR.xlsx", 
                    sheet = "2018-2022 Final Reads_by Sample",
                    range = "A3:BX167")
DIST.STOCKS <- read.csv("input/DISTRICT_STOCKS.csv")

escape.df <- raw.df %>% 
  select(c(Species,Stock,'Sample Date','#Rcvd','#Preps','Not Marked','Marked','# LCI','# PWS',
           '# KOD','# Other','TUTKA16PINK','TUTKA16PINKA','TUTKA17PINK','TUTKA17PINKA','TUTKA18PINK','TUTKA18PINKA',
           'TUTKA18PINKB','TUTKA18PINKC','TUTKA19PINK','TUTKA19PINKA','TUTKA19PINKB','TUTKA20PINKB','PORTGRAHAM16','PORTGRAHAM17',
           'PORTGRAHAM17A','PORTGRAHAM18','PORTGRAHAM19','PORTGRAHAM20','AFK16B','AFK17B','AFK18B','AFK19B','AFK20A','AFK20B','CCH16','CCH17',
           'CCH18','CCH19','CCH20','CCH20A','SGH16','SGH17','SGH18','SGH19','SGH20','WNH16PINKB','WNH17PINKB','WNH18PINKB','WNH19PINKB',
           'WNH20PINKB','KITOI17PINK','KITOI18PINK','KITOI19PINK')) %>% 
  rename(SampleDate = 'Sample Date',
         Rcvd = '#Rcvd',
         Preps = '#Preps',
         NotMarked = 'Not Marked',
         N.LCI = '# LCI',
         N.PWS = '# PWS',
         N.KOD = '# KOD',
         N.Other = '# Other') %>% 
  mutate(Species = ifelse(Species == "PINK","Pink",Species),
         Year = format(SampleDate, "%Y"),
         JulianDay = as.POSIXlt(SampleDate)$yday, ## converts date to Julian Date integer
         MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## create a MMDD date without year
         OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01"))) %>% 
  filter(Stock != "McNeil River") %>% 
  left_join(DIST.STOCKS,
            by = join_by(Species == Species,Stock == Stock))

summary.df <- escape.df %>% 
  group_by(Species,Stock,SampleDate,Year) %>% 
  reframe(PercentHatchery = (N.LCI + N.PWS + N.KOD + N.Other)/(Marked + NotMarked),
            PercentLCI = N.LCI/(Marked + NotMarked),
            PercentPWS = N.PWS/(Marked + NotMarked),
            PercentKOD = N.KOD/(Marked + NotMarked),
            PercentOther = N.Other/(Marked + NotMarked))
colnames(raw.df)
