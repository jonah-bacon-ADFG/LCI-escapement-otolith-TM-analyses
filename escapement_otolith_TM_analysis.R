library(readxl)
library(tidyverse)
library(fs)

getwd()
setwd("O:/DCF/SALMON/OTOLITH_STUDIES/2_ANALYSES_ESCAP/2018-2022")

raw.df <- read_xlsx(path = "data/1_raw/2018-2022 Final PINK ESC Reads from SEMR.xlsx", 
                    sheet = "2018-2022 Final Reads_by Sample",
                    range = "A3:BX167")

DIST.STOCKS <- read.csv("input/DISTRICT_STOCKS.csv")

sample.df <- raw.df %>% 
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
         JulianDay = as.POSIXlt(SampleDate)$yday) %>%
  filter(Stock != "McNeil River") %>% 
  left_join(DIST.STOCKS,
            by = join_by(Species == Species,Stock == Stock))

hatch.total.df <- sample.df %>% 
  group_by(Species,Stock,SampleDate,Year) %>% 
  summarise(Read = sum(Marked) + sum(NotMarked),
            PercentHatchery = sum(sum(N.LCI),sum(N.PWS),sum(N.KOD),sum(N.Other))/Read) %>% 
  mutate(CarcassDay = as.POSIXlt(SampleDate)$yday) %>% 
  filter(Read >= 30)

hatch.region.df <- sample.df %>% 
  group_by(Species,Stock,SampleDate,Year) %>% 
  summarise(Read = sum(Marked) + sum(NotMarked),
            Marked = sum(Marked),
            NotMarked = sum(NotMarked),
            N.LCI = sum(N.LCI),
            PercentLCI = sum(N.LCI)/Read,
            N.PWS = sum(N.PWS),
            PercentPWS = sum(N.PWS)/Read,
            N.KOD = sum(N.KOD),
            PercentKOD = sum(N.KOD)/Read,
            N.Other = sum(N.Other),
            PercentOther = sum(N.Other)/Read) %>% 
  mutate(CarcassDay = as.POSIXlt(SampleDate)$yday) %>% 
  filter(Read >= 30)

# Historical Run-Timing Curves --------------------------------------------

HIST.RTC.df <- read.csv("input/RUN_TIMING.csv") %>% 
  filter(Year >=1976, SPECIES == "Pink Salmon", STOCK %in% sample.df$Stock) %>%                                                       ## only include years after 1976
  mutate(Year = as.integer(Year), ## converts character to integer
         Date = as.Date(Date), ## converts character to date
         JulianDay = as.POSIXlt(Date)$yday, ## converts date to Julian Date integer
         MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## create a MMDD date without year
         OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
         # Do we want to have a Pink Salmon ODD/EVEN run-timing curve?
         PinkOddEven = if_else(SPECIES == "Pink Salmon" & Year %% 2 == 0,"Even-year",as.character("NA")),
         PinkOddEven = if_else(SPECIES == "Pink Salmon" & Year %% 2 != 0,"Odd-year",PinkOddEven)) %>% 
  full_join(DIST.STOCKS,
            by = join_by(SPECIES == Full_Species,STOCK == Stock)) %>% 
  filter(!is.na(Date))

JDAY.MODEL.df <- data.frame(JulianDay = c(min(HIST.RTC.df$JulianDay):max(HIST.RTC.df$JulianDay)))

i = 1 ; j = 1                                                                               
rm("RTC.df") ; for (i in c(1:length(unique(sample.df$Stock)))){
  HIST.AUC.df <- HIST.RTC.df %>% 
    filter(STOCK == unique(sample.df$Stock)[i])
  
    model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
    ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
    
    model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
      mutate(Stock = unique(sample.df$Stock)[i],
             MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
             OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
             PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3))
    
    if(exists('RTC.df') && is.data.frame(get('RTC.df'))){   ## checks to see if output dataframe exists
      RTC.df <- rbind(RTC.df,model.preds)                 ## if it does, it appends new data
    }
    else {                                                                      ## if it doesn't creates new dataframe
      RTC.df <- model.preds
    }
}

RTC.df <- RTC.df %>% 
  mutate(CarcassDay = JulianDay + 18,
         PercentCarcass = PercentEscape)




# AUC Escapement ----------------------------------------------------------

final.escape.2018 <- read_xlsx(path = "input/2018 Final Escapement Indices_WORKING COPY_10_26_18.xlsx", 
                               sheet = "Final Escapement Indices",
                               range = "A8:G108")
final.escape.2018$Year = 2018

final.escape.2019 <- read_xlsx(path = "input/2019 Final Escapement Indices_WORKING COPY_20191002.xlsx", 
                               sheet = "Final Escapement Indices",
                               range = "A8:G123")
final.escape.2019$Year = 2019

final.escape.2020 <- read_xlsx(path = "input/2020 Final Escapement Indices_WORKING COPY_20201001.xlsx", 
                               sheet = "Final Escapement Indices",
                               range = "A8:G99")
final.escape.2020$Year = 2020

final.escape.2021 <- read_xlsx(path = "input/2021 Final Escapement Indices_WORKING COPY.xlsx", 
                               sheet = "Final Escapement Indices",
                               range = "A8:G102")
final.escape.2021$Year = 2021

final.escape.2022 <- read_xlsx(path = "input/2022 Final Escapement Indices_WORKING COPY_20221011.xlsx", 
                               sheet = "Final Escapement Indices",
                               range = "A8:G94")
final.escape.2022$Year = 2022

escape.df <- rbind(final.escape.2018,final.escape.2019,final.escape.2020,final.escape.2021,final.escape.2022) %>% 
  filter(Species == "Pink Salmon", Stream %in% unique(sample.df$Stock)) %>% 
  mutate(Year = as.character(Year))

# Estimating hatchery proportion in escapement ----------------------------

hatch.escape.proportion.df <- hatch.total.df %>% 
  left_join(RTC.df, by = join_by(Stock, CarcassDay)) %>% 
  left_join(escape.df, by = join_by(Stock == Stream, Year)) %>% 
  select(Species.x,Stock,Year,SampleDate,CarcassDay,PercentHatchery,PercentCarcass,Escapement) %>% 
  rename(Species = Species.x)

year.stock.df <- data.frame(
  Year = c(rep(2018,length(unique(sample.df$Stock))),rep(2019,length(unique(sample.df$Stock))),rep(2020,length(unique(sample.df$Stock))),rep(2021,length(unique(sample.df$Stock))),rep(2022,length(unique(sample.df$Stock)))),
  Stock = rep(unique(sample.df$Stock),5)
)

i=1
rm(list = "hatch.percent.escape.df"); for (i in c(1:nrow(year.stock.df))){                              ## each group
  temp.df <- hatch.escape.proportion.df %>% 
    filter(Stock == year.stock.df$Stock[i] &
           Year == year.stock.df$Year[i])
  
    FirstRow <- head(temp.df,1) %>% 
      mutate(PercentCarcass = 0)
    LastRow <- tail(temp.df,1) %>% 
      mutate(PercentCarcass = 1)
    
    temp.df <- rbind(FirstRow,temp.df,LastRow)

  for (j in c(1:nrow(temp.df))){                                              ## Calculates columns Fishdays, SumFishDays, EscInd, and SumEscape
    if (j>1){
      temp.df$run.percent[j]  <- (temp.df$PercentCarcass[j]-temp.df$PercentCarcass[j-1])
      temp.df$escapement.est[j]  <- temp.df$run.percent[j]*temp.df$Escapement[j]
      temp.df$hatchery.est[j] <- temp.df$escapement.est[j]*temp.df$PercentHatchery[j]
    }
    else{
      temp.df$run.percent[j]  <- 0
      temp.df$escapement.est[j]  <- 0
      temp.df$hatchery.est[j] <- 0
    }
    
  }
  
  if(exists('hatch.percent.escape.df') && is.data.frame(get('hatch.percent.escape.df'))){                           ##checks to see if output dataframe exists
    hatch.percent.escape.df <- rbind(hatch.percent.escape.df,temp.df)       ##if it does, it appends new data
  }
  else {                                                                        ## if it doesn't creates new dataframe
    hatch.percent.escape.df <- temp.df
  }
  
}

summary.hatch.percent.esc.df <- hatch.percent.escape.df %>% 
  group_by(Species,Stock,Year) %>% 
  summarise(Escapement = unique(Escapement),
            Hatchery.Escapement.est = sum(hatchery.est),
            Hatchery.Escapement.percent = Hatchery.Escapement.est/Escapement)

# Save data ---------------------------------------------------------------

save(list = c("RTC.df","escape.df","summary.hatch.percent.esc.df","hatch.escape.proportion.df","hatch.total.df","hatch.region.df","sample.df"), 
     file = "data/escapement_otolith_TM_results.RData")
