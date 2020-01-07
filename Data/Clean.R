##=============================================================================##
## Project: PD distribution and capital savings                                ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Description: this file cleans the raw dataset in .R format                  ##
##=============================================================================##

##=============================================================================##
## Code setup                                                                  ##
##=============================================================================##

rm(list = ls())  
ptm <- proc.time()
library(tidyverse)                ## Data manipulation, pipe operator
library(zoo)                      ## Apply function to rolling margins of data
library(DescTools)                ## Command to winsorize variables
library(readxl)                   ## Command to open xlsx files                                                     
library(Hmisc)                    ## Weighted variance and mean functions
library(moments)                  ## Moments functions
library(splitstackshape)          ## Command to expand data
source("Auxiliar/Function.R")     ## Capital requirements functions

# Open dirt data frame
load("Data/Temp/BankScope.Rda")
load("Data/Temp/Pillar3Data.Rda")
load("Data/Temp/AuxiliarData.Rda")

##============================================================================##
## Pillar-III reports                                                         ##
##============================================================================##

#------------------------------------------------------------------------------#
# Basic cleaning and data creation of raw datasets                             #
#------------------------------------------------------------------------------#

IRB.data <- as.data.frame(pillar3.data) %>%
  filter(Method2 %in% c("IRB"),
         PD  < 0.99, EAD > 0) %>%
  mutate(LGD        = ifelse(is.na(LGD), 0.45, LGD),
         w.PD       = PD*EAD*LGD,
         w.EAD      = LGD*EAD,
         sum.EAD    = ave(w.EAD, bvdid, year, Portfolio_1, Portfolio_2, FUN = function(x) sum(x, na.rm = TRUE)),
         w.sum.PD   = ave(w.PD, bvdid, year, Portfolio_1, Portfolio_2, FUN = function(x) sum(x, na.rm = TRUE)),
         w.avg.PD   = w.sum.PD/sum.EAD) 
  
SA.data <- as.data.frame(pillar3.data) %>%
  filter(!Method2 %in% c("IRB"),
         EAD > 0)

#------------------------------------------------------------------------------#
# Capital requirement calculations                                             #
#------------------------------------------------------------------------------#

for (i in 1:nrow(IRB.data)){
  if (!IRB.data[i,"Portfolio_1"] %in% c("Wholesale","Equity")){
    next
  }
 
  IRB.data[i,"RWA"]     <- IRB.data[i,"EAD"]*mapping_wholesale(IRB.data[i,"PD"],
                                                               IRB.data[i,"LGD"])["RW"]
 
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"EAD"]*mapping_wholesale(IRB.data[i,"w.avg.PD"],
                                                               IRB.data[i,"LGD"])["RW"]
  
  IRB.data[i,"RWA.til"]     <- IRB.data[i,"EAD"]*mapping_wholesale(IRB.data[i,"PD"],
                                                               IRB.data[i,"LGD"],
                                                               a = -50,
                                                               R_l = 0.10,
                                                               R_u = 0.28)["RW"]
  
  IRB.data[i,"RWA.bar"] <- IRB.data[i,"EAD"]*mapping_wholesale(IRB.data[i,"w.avg.PD"],
                                                               IRB.data[i,"LGD"],
                                                               a = -50,
                                                               R_l = 0.10,
                                                               R_u = 0.28)["RW"]
}

for (i in 1:nrow(IRB.data)){
  if (is.na(IRB.data[i,"Portfolio_2"])){
    next
  }
  if (!IRB.data[i,"Portfolio_2"] %in% c("Real estate")){
    next
  }
  
  IRB.data[i,"RWA"]     <- IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"PD"],
                                                            IRB.data[i,"LGD"],
                                                            0.15)["RW"]
  
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"w.avg.PD"],
                                                            IRB.data[i,"LGD"],
                                                            0.15)["RW"]
  
}

for (i in 1:nrow(IRB.data)){
  if (is.na(IRB.data[i,"Portfolio_2"])){
    next
  }
  if (!IRB.data[i,"Portfolio_2"] %in% c("Qualifying revolving")){
    next
  }
  
  IRB.data[i,"RWA"]     <- IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"PD"],
                                                            IRB.data[i,"LGD"],
                                                            0.04)["RW"]
  
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"w.avg.PD"],
                                                            IRB.data[i,"LGD"],
                                                            0.04)["RW"]
}

for (i in 1:nrow(IRB.data)){
  if (IRB.data[i,"Portfolio_1"] %in% c("Wholesale","Equity")){
    next
  }
  if (is.na(IRB.data[i,"Portfolio_2"])){
    next
  }
  if (!IRB.data[i,"Portfolio_2"] %in% c("Other retail", "Total")){
    next
  }
  IRB.data[i,"RWA"]     <- IRB.data[i,"EAD"]*mapping_other_retail(IRB.data[i,"PD"],
                                                                  IRB.data[i,"LGD"])["RW"]
  
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"EAD"]*mapping_other_retail(IRB.data[i,"w.avg.PD"],
                                                                  IRB.data[i,"LGD"])["RW"] 
}


# Save clean data frame
save(pillar3.data,file=paste0("Data/Datasets/Pillar3Data.Rda"))

##============================================================================##
## Orbis Bank Focus (BankScope)                                               ##
##============================================================================##

# Auxiliar table used to include all bank observations when calculating the mean benchmark
sample <- read_xlsx("Data/Raw/Pillar3/Auxiliar/Auxiliar.xlsx",sheet = 1)  %>%
  rename_if(is.numeric, function(x) paste("IRB", x, sep = ".")) %>%
  reshape(varying   = c(grep("[0-9]", names(.))),
          direction = 'long', 
          timevar   ='year')%>%
  select(name, Country, bvdid = bvdid_new, year, IRB)%>%
  filter(!is.na(IRB)) %>%
  select(-IRB)

#------------------------------------------------------------------------------#
# Merge Pillar-III data by bank to bankscope                                   #
#------------------------------------------------------------------------------#

## Aggregated SA exposures at bank-year level
bank.data <- SA.data %>% 
  group_by(name,bvdid,Country,year) %>%
  filter(Method %in% c("SA"))%>%
  summarise(RWA.SA = sum(RWA, na.rm = TRUE),
            EAD.SA = sum(EAD, na.rm = TRUE))%>%
  ungroup()%>%
  full_join(sample, by=c("bvdid", "name", "Country","year"))%>%
  mutate(RW.SA = 100*RWA.SA/EAD.SA)
 
  
## Aggregated IRB exposures at bank-portfolio-year level 
for(i in c("Wholesale","Retail","Equity")) {
bank.data <- IRB.data %>% 
  group_by(name,bvdid,Country,year,Portfolio_1) %>%
  filter(Portfolio_1 %in% c(i)) %>%
  summarise(!!(paste("gini", "PD" ,        i, sep=".")) := Gini(PD,w.EAD, na.rm = TRUE),
            !!(paste("sd"  , "PD" ,        i, sep=".")) := sqrt(wtd.var(PD,w.EAD, na.rm = TRUE))*100,
            !!(paste("mean", "PD" ,        i, sep=".")) := wtd.mean(PD,w.EAD, na.rm = TRUE)*100,
            !!(paste(        "RWA",        i, sep=".")) := sum(RWA, na.rm = TRUE),
            !!(paste(        "RWA", "hat", i, sep=".")) := sum(RWA.hat, na.rm = TRUE),
            !!(paste(        "RWA", "til", i, sep=".")) := sum(RWA.til, na.rm = TRUE),
            !!(paste(        "RWA", "bar", i, sep=".")) := sum(RWA.bar, na.rm = TRUE),
            !!(paste(        "EAD",        i, sep=".")) := sum(EAD, na.rm = TRUE))%>%
  ungroup()%>%
  full_join(sample, by=c("bvdid","name","Country","year"))%>%
  mutate(!!(paste("RW" , "hat", i, sep=".")) := 100*get(paste("RWA","hat", i , sep="."))/get(paste("EAD", i , sep=".")),
         !!(paste("RWA", "s"  , i, sep=".")) := get(paste("RWA","hat", i, sep="."))-get(paste("RWA", i , sep=".")),
         !!(paste("RWA", "r"  , i, sep=".")) := get(paste("RWA","hat", i, sep="."))/get(paste("RWA", i , sep="."))-1,
         !!(paste("RWA", "c"  , i, sep=".")) := get(paste("RWA","bar", i, sep="."))/get(paste("RWA","til", i , sep="."))-1,
         !!(paste("RW" , "s"  , i, sep=".")) := 100*ifelse(get(paste("EAD", i, sep=".")) == 0, 0,
                                               get(paste("RWA","s", i , sep="."))/get(paste("EAD", i , sep="."))))%>%
  full_join(bank.data,by=c("bvdid","name","Country","year"))
}

## Aggregated IRB exposures at bank-portfolio-year level 
for(i in c("Corporate","Sovereign","Banks")) {
  bank.data <- IRB.data %>% 
    group_by(name,bvdid,Country,year,Portfolio_1,Portfolio_2) %>%
    filter(Portfolio_2 %in% c(i)) %>%
    summarise(!!(paste("gini", "PD" ,        i, sep=".")) := Gini(PD,w.EAD, na.rm = TRUE),
              !!(paste("sd"  , "PD" ,        i, sep=".")) := sqrt(wtd.var(PD,w.EAD, na.rm = TRUE))*100,
              !!(paste("mean", "PD" ,        i, sep=".")) := wtd.mean(PD,w.EAD, na.rm = TRUE)*100,
              !!(paste(        "RWA",        i, sep=".")) := sum(RWA, na.rm = TRUE),
              !!(paste(        "RWA", "hat", i, sep=".")) := sum(RWA.hat, na.rm = TRUE),
              !!(paste(        "RWA", "til", i, sep=".")) := sum(RWA.til, na.rm = TRUE),
              !!(paste(        "RWA", "bar", i, sep=".")) := sum(RWA.bar, na.rm = TRUE),
              !!(paste(        "EAD",        i, sep=".")) := sum(EAD, na.rm = TRUE))%>%
    ungroup()%>%
    full_join(sample, by=c("bvdid","name","Country","year"))%>%
    mutate(!!(paste("RW" ,"hat", i, sep=".")) := 100*get(paste("RWA","hat", i , sep="."))/get(paste("EAD", i , sep=".")),
           !!(paste("RWA","s"  , i, sep=".")) := get(paste("RWA","hat", i, sep="."))-get(paste("RWA", i , sep=".")),
           !!(paste("RWA","r"  , i, sep=".")) := get(paste("RWA","hat", i, sep="."))/get(paste("RWA", i , sep="."))-1,
           !!(paste("RWA","c"  , i, sep=".")) := get(paste("RWA","bar", i, sep="."))/get(paste("RWA","til" ,i , sep="."))-1,
           !!(paste("RW" ,"s"  , i, sep=".")) := 100*ifelse(get(paste("EAD", i, sep=".")) == 0, 0,
                                                 get(paste("RWA","s", i , sep="."))/get(paste("EAD", i , sep=".")))) %>% 
    full_join(bank.data,by=c("bvdid","name","Country","year"))
}

# Add PD moments for entire IRB portfolio
bank.data <- IRB.data %>%
  select(bvdid, year, w.EAD, PD) %>%
  mutate(w.EAD = round(w.EAD/10),
         PD = PD*100) %>%
  na.omit(cols=c("w.EAD")) %>%
  expandRows("w.EAD")%>%
  group_by(bvdid, year) %>% 
  summarise(mean.PD = mean(PD, na.rm = TRUE),
            var.PD  = var(PD, na.rm = TRUE),
            skew.PD = skewness(PD, na.rm = TRUE),
            kurt.PD = kurtosis(PD, na.rm = TRUE),
            gini.PD = Gini(PD, na.rm = TRUE)) %>%
  full_join(bank.data,by=c("bvdid","year"))

# Merge datasets
bank.data <- bank.data%>%
  # Add BankScope
  full_join(bankscope,by=c("bvdid", "name", "Country","year"))%>%
  # Add Basel 2 country level introduction year
  left_join(basel.indicator, by = c("Country")) %>%
  # Add EBA capital exercise information
  left_join(EBA.indicator, by = c("bvdid")) %>% 
  # Add macro variables
  left_join(WDI, by = c("Country", "year")) %>%
  # Add bank regulation variables
  mutate(survey = ifelse(year<2001, 1, 
                         ifelse(between(year, 2001, 2004), 2, 
                                ifelse(between(year, 2005, 2007), 3, 4))))%>%
  left_join(BRSS, by = c("Country", "survey"))

#------------------------------------------------------------------------------#
# Basic cleaning and variable calculation                                      #
#------------------------------------------------------------------------------#

bank.data <- bank.data %>% ungroup() %>% rowwise() %>%
   mutate(basel         = ifelse(basel>year,0,1),
          IRB.bvdid     = paste(bvdid,IRB,sep = ""),
          sample        = as.factor(ifelse(bvdid %in% pillar3.data$bvdid,"Yes","No")),
          EAD.IRB       = sum(EAD.Wholesale, EAD.Retail, EAD.Equity, na.rm = TRUE),
          EAD.SA        = ifelse(is.na(EAD.SA) & sample == "Yes",0,EAD.SA),  
          EAD           = sum(EAD.IRB, EAD.SA, na.rm = TRUE),
          RWA.IRB       = sum(RWA.Wholesale, RWA.Retail, RWA.Equity, na.rm = TRUE),
          RWA.til       = sum(RWA.til.Wholesale, RWA.Retail, RWA.til.Equity, na.rm = TRUE),
          RWA.SA        = ifelse(is.na(RWA.SA) & sample == "Yes",0,RWA.SA),  
          RWA           = sum(RWA.IRB, RWA.SA, na.rm = TRUE),
          RWA.alt       = 100*(totalcapitalmillcu/totalcapitalratio),
          RW            = 100*RWA/EAD,
          RW.IRB        = 100*RWA.IRB/EAD.IRB,
          RWA.s         = sum(RWA.s.Wholesale, RWA.s.Retail, RWA.s.Equity, na.rm = TRUE),
          RWA.hat       = sum(RWA.hat.Wholesale, RWA.hat.Retail, RWA.hat.Equity, na.rm = TRUE),
          RWA.bar       = sum(RWA.bar.Wholesale, RWA.Retail, RWA.bar.Equity, na.rm = TRUE),
          RWA.r         = RWA.hat/RWA.IRB-1,
          RWA.c         = RWA.bar/RWA.til-1,
          RWA.hat.total = sum(RWA, RWA.s, na.rm = TRUE),
          RWA.hat.alt   = sum(RWA.alt, RWA.s, na.rm = TRUE),
          RW.s          = 100*ifelse(EAD.IRB == 0 & IRB == 0, 0, RWA.s/EAD.IRB),
          RW.hat        = 100*ifelse(EAD.IRB == 0 & IRB == 0, RW, RWA.hat/EAD.IRB),
          q.SA          = ifelse(IRB == 0, 1, EAD.SA/EAD),
          q.Wholesale   = EAD.Wholesale/EAD.IRB,
          q.Retail      = EAD.Retail/EAD.IRB,
          q.Equity      = EAD.Equity/EAD.IRB,
          CAR.alt       = 100*(totalcapitalmillcu/RWA.hat.alt),
          CAR           = ifelse(RWA.IRB > 0 & totalcapitalmillcu>0,100*(totalcapitalmillcu/RWA.IRB),NA),
          CAR.til       = ifelse(RWA.til > 0 & totalcapitalmillcu>0,100*(totalcapitalmillcu/RWA.til),NA),
          tier          = 100*(tier1capitalmillcu/RWA),
          CAR.hat       = 100*(totalcapitalmillcu/RWA.hat.total),
          tier.hat      = 100*(tier1capitalmillcu/RWA.hat.total),
          CAR.dif.alt   = ifelse(EAD.IRB == 0 & IRB == 0, 0,totalcapitalratio - CAR.alt),
          CAR.dif       = ifelse(EAD.IRB == 0 & IRB == 0, 0,CAR - CAR.hat),
          tier.dif      = ifelse(EAD.IRB == 0 & IRB == 0, 0,tier - tier.hat),
          log.CAR       = log(CAR),
          log.CAR.til   = log(CAR.til),
          log.K         = log(totalcapitalmillcu),
          log.tier      = log(tier1capitalmillcu),
          log.asset     = log(totalassetsmillcu),
          log.gdp       = log(gdppc),
          log.loans     = log(grossloansmillcu*(1-loanlossresgrossloans/100)),
          deposit.ratio = 100*totalcustomerdepositsmillcu/totalassetsmillcu,
          loans.ratio   = 100*grossloansmillcu/totalassetsmillcu,
          NII.ratio     = 100*netinterestrevenuemillcu/operatingrevenueturnovermillcu,
          income.ratio  = 100*plforperiodnetincomemillcu/totalassetsmillcu,
          ROE           = 100*plbeforetaxmillcu/equitymillcu,
          ROA           = 100*plbeforetaxmillcu/totalassetsmillcu,
          leverage      = 100*equitymillcu/totalassetsmillcu,
          subordinated  = 100*subordinateddebtsmemomillcu/(totalassetsmillcu - equitymillcu)) %>%
   rename(NPL.ratio     = nonperfloansgrossloans,
          LLR.ratio     = loanlossresgrossloans,
          capital.ratio = totalcapitalratio) %>%
   # Log variables
   ungroup() %>%  
   mutate_at(vars(contains("RWA.s")), rlang::as_function(function(x) ifelse(is.na(x),0,x)))%>%
   mutate_at(vars(contains("RWA")), .funs = list(log = ~log(1+.)))%>%
   rename_at(vars(contains("_log")), list( ~paste("log", gsub("_log", "", .), sep = "."))) %>%
   # Moving average Z-score
   ungroup()%>% arrange(bvdid,year) %>% group_by(bvdid)%>%
   mutate(r.mean.ROA = rollapply(ROA, 5, mean, na.rm = TRUE, align='right', fill=NA),
          r.mean.CAR = rollapply(leverage, 5, mean, na.rm = TRUE, align='right', fill=NA),
          r.sd.ROA   = rollapply(ROA, 5, sd, na.rm = TRUE, align='right', fill=NA))%>%
          ungroup() %>%
   mutate(r.mean.ROA = Winsorize(r.mean.ROA, probs = c(0.05, 0.95), na.rm = TRUE),
          r.mean.CAR = Winsorize(r.mean.CAR, probs = c(0.05, 0.95), na.rm = TRUE),
          r.sd.ROA   = Winsorize(r.sd.ROA,   probs = c(0.05, 0.95), na.rm = TRUE),
          Zscore     = (r.mean.ROA + r.mean.CAR)/r.sd.ROA)%>%
   # Growth rates
   ungroup() %>% group_by(bvdid) %>%
   mutate_at(vars(matches("log.|.ratio")), .funs = list(gr= ~ifelse(. > 0 & lag(., order_by = year)>0, 
                                                                    . - lag(., order_by = year), NA))) %>%
   mutate_at(vars(matches(".PD")), .funs = list(gr= ~ifelse(. > 0 & lag(., order_by = year)>0, 
                                                            log(.) - log(lag(., order_by = year)), NA))) %>%
  # Remove IRB not collected  
   mutate(IRB.indicator = mean(IRB),
          not.collect   = ifelse(IRB.indicator >0 & sample == "No", 1, 0)) %>%
   filter(not.collect == 0) %>%
   # Remove auxiliar variables
   select(everything(), -contains("Portfolio"),
          -starts_with("r."), -c("not.collect", "IRB.indicator")) 

# Save clean data frame
save(bank.data,file=paste0("Data/Datasets/BankData.Rda"))

##============================================================================##
## RW decompostion datasets                                                   ##
##============================================================================##

# Auxiliar table used to include all bank observations when calculating the mean benchmark
sample <- pillar3.data %>% 
  distinct(bvdid, name, Country, .keep_all = FALSE)
#----------------------------------------------------------------------------#
# Cross-section decompostion                                                 #
#----------------------------------------------------------------------------#

## SA portfolio exposure decomposition 
cross.section.decomposition <- SA.data %>% 
  group_by(name, bvdid, Country, year) %>%
  summarise(RWA.SA = sum(RWA, na.rm = TRUE),
            EAD.SA = sum(EAD, na.rm = TRUE))%>%
  group_by(name, bvdid, Country) %>%
  summarise_if(is.numeric, funs(mean), na.rm = TRUE)%>%
  ungroup()%>%select(-year)%>%
  full_join(sample, by=c("bvdid", "name", "Country"))%>%
  mutate(RW.SA       = RWA.SA/EAD.SA,
         mean.RWA.SA = mean(RWA.SA, na.rm = TRUE),
         mean.EAD.SA = mean(EAD.SA, na.rm = TRUE),
         mean.RW.SA  = mean.RWA.SA/mean.EAD.SA)%>%
  ungroup()

## IRB portfolios exposures decomposition 
for(i in c("Wholesale", "Retail", "Equity")) {
  cross.section.decomposition <- IRB.data %>% 
    group_by(name, bvdid, Country, year, Portfolio_1) %>%
    filter(Portfolio_1 %in% c(i)) %>%
    summarise(!!(paste("RWA",        i, sep=".")) := sum(RWA    , na.rm = TRUE),
              !!(paste("RWA", "hat", i, sep=".")) := sum(RWA.hat, na.rm = TRUE),
              !!(paste("EAD",        i, sep=".")) := sum(EAD    , na.rm = TRUE))%>%
    group_by(name, bvdid, Country) %>%
    summarise_if(is.numeric, funs(mean), na.rm = TRUE)%>%
    ungroup()%>%select(-year)%>%
    full_join(sample, by=c("bvdid","name","Country"))%>%
    mutate(!!(paste(       "RWA", "s"  , i, sep=".")) :=      get(paste(        "RWA", "hat", i, sep="."))-get(paste("RWA", i, sep=".")),
           !!(paste(       "RW" , "hat", i, sep=".")) :=      get(paste(        "RWA", "hat", i, sep="."))/get(paste("EAD", i, sep=".")),
           !!(paste(       "RW" , "IRB", i, sep=".")) :=      get(paste(        "RWA",        i, sep="."))/get(paste("EAD", i, sep=".")),
           !!(paste("mean","RWA",        i, sep=".")) := mean(get(paste(        "RWA",        i, sep=".")), na.rm = TRUE),
           !!(paste("mean","RWA", "hat", i, sep=".")) := mean(get(paste(        "RWA", "hat", i, sep=".")), na.rm = TRUE),
           !!(paste("mean","EAD",        i, sep=".")) := mean(get(paste(        "EAD",        i, sep=".")), na.rm = TRUE),
           !!(paste("mean","RWA", "s"  , i, sep=".")) :=      get(paste("mean", "RWA", "hat", i, sep="."))-get(paste("mean", "RWA", i, sep=".")),
           !!(paste("mean","RW" , "hat", i, sep=".")) :=      get(paste("mean", "RWA", "hat", i, sep="."))/get(paste("mean", "EAD", i, sep=".")),
           !!(paste("mean","RW" , "IRB", i, sep=".")) :=      get(paste("mean", "RWA",        i, sep="."))/get(paste("mean", "EAD", i, sep="."))) %>%
    full_join(cross.section.decomposition,by=c("bvdid","name","Country"))
}

## Create portfolio shares, benchmarks, and remaning variables
cross.section.decomposition <- cross.section.decomposition %>%
  rowwise() %>%
  mutate(EAD.IRB     = sum(EAD.Wholesale, EAD.Retail, EAD.Equity, na.rm = TRUE),
         EAD         = sum(EAD.IRB, EAD.SA, na.rm = TRUE),
         RWA.IRB     = sum(RWA.Wholesale, RWA.Retail, RWA.Equity, na.rm = TRUE),
         RW          = sum(RWA.IRB, RWA.SA, na.rm = TRUE)/EAD,
         RW.IRB      = RWA.IRB/EAD.IRB,
         RWA.s       = sum(RWA.s.Wholesale, RWA.s.Retail, RWA.s.Equity, na.rm = TRUE),
         RWA.hat     = sum(RWA.hat.Wholesale, RWA.hat.Retail, RWA.hat.Equity, na.rm = TRUE),
         RW.s        = RWA.s/EAD.IRB,
         RW.hat      = RWA.hat/EAD.IRB,
         q.SA        = EAD.SA/EAD,
         q.Wholesale = EAD.Wholesale/EAD.IRB,
         q.Retail    = EAD.Retail/EAD.IRB,
         q.Equity    = EAD.Equity/EAD.IRB,
         # Mean benchmark
         mean.EAD.IRB     = sum(mean.EAD.Wholesale, mean.EAD.Retail, mean.EAD.Equity, na.rm = TRUE),
         mean.RWA.IRB     = sum(mean.RWA.Wholesale, mean.RWA.Retail, mean.RWA.Equity, na.rm = TRUE),
         mean.EAD         = sum(mean.EAD.IRB, mean.EAD.SA, na.rm = TRUE),
         mean.RW          = sum(mean.RWA.IRB, mean.RWA.SA, na.rm = TRUE)/mean.EAD,
         mean.RWA.s       = sum(mean.RWA.s.Wholesale, mean.RWA.s.Retail, mean.RWA.s.Equity, na.rm = TRUE),
         mean.RW.s        = mean.RWA.s/mean.EAD.IRB,
         mean.q.SA        = mean.EAD.SA/mean.EAD,
         mean.q.Wholesale = mean.EAD.Wholesale/mean.EAD.IRB,
         mean.q.Retail    = mean.EAD.Retail/mean.EAD.IRB,
         mean.q.Equity    = mean.EAD.Equity/mean.EAD.IRB,
         ## Replace missing RW's and q's with values that lead to zero contribution to total RW
         RW.hat.Wholesale = ifelse(is.na(RW.hat.Wholesale),mean.RW.hat.Wholesale,RW.hat.Wholesale),
         RW.hat.Retail    = ifelse(is.na(RW.hat.Retail),mean.RW.hat.Retail,RW.hat.Retail),
         RW.hat.Equity    = ifelse(is.na(RW.hat.Equity),mean.RW.hat.Equity,RW.hat.Equity),
         RW.IRB.Wholesale = ifelse(is.na(RW.IRB.Wholesale),mean.RW.IRB.Wholesale,RW.IRB.Wholesale),
         RW.IRB.Retail    = ifelse(is.na(RW.IRB.Retail),mean.RW.IRB.Retail,RW.IRB.Retail),
         RW.IRB.Equity    = ifelse(is.na(RW.IRB.Equity),mean.RW.IRB.Equity,RW.IRB.Equity),
         RW.SA            = ifelse(is.na(RW.SA),mean.RW.SA,RW.SA))%>%
  mutate_at(vars(contains("q.")), 
            rlang::as_function(function(x) ifelse(is.na(x),0,x)))%>%
         ## Components of decomposition WITH PD distribution effect
  mutate(IRB.gain     = (1-mean.q.SA)*(mean.q.Wholesale*(RW.hat.Wholesale-mean.RW.hat.Wholesale)+
                                       mean.q.Retail*(RW.hat.Retail-mean.RW.hat.Retail)+
                                       mean.q.Equity*(RW.hat.Equity-mean.RW.hat.Equity)),
         
         mix.gain     = (1-mean.q.SA)*((q.Wholesale-mean.q.Wholesale)*(RW.hat.Wholesale)+
                                       (q.Retail-mean.q.Retail)*(RW.hat.Retail)+
                                       (q.Equity-mean.q.Equity)*(RW.hat.Equity)),
         rollout.gain = (q.SA-mean.q.SA)*(RW.SA-RW.hat),
         int.gain     = (1-mean.q.SA)*(mean.RW.s-RW.s),
         ext.gain     = (q.SA-mean.q.SA)*(RW.s),
         ## Components of decomposition WITHOUT PD distribution effect      
         IRB.gain.alt     = (1-mean.q.SA)*(mean.q.Wholesale*(RW.IRB.Wholesale-mean.RW.IRB.Wholesale)+
                                           mean.q.Retail*(RW.IRB.Retail-mean.RW.IRB.Retail)+
                                           mean.q.Equity*(RW.IRB.Equity-mean.RW.IRB.Equity)),
         mix.gain.alt     = (1-mean.q.SA)*((q.Wholesale-mean.q.Wholesale)*(RW.IRB.Wholesale)+
                                           (q.Retail-mean.q.Retail)*(RW.IRB.Retail)+
                                           (q.Equity-mean.q.Equity)*(RW.IRB.Equity)),
         rollout.gain.alt = (q.SA-mean.q.SA)*(RW.SA-RW.IRB),
         ## Common components between decompositions and net components
         SA.gain    = mean.q.SA*(RW.SA-mean.RW.SA),
         other.gain = SA.gain + IRB.gain + mix.gain + rollout.gain,
         PD.gain    = int.gain + ext.gain,
         total.gain = RW-mean.RW)%>%
  ungroup()

# Save clean data frame
save(cross.section.decomposition,file=paste0("Data/Datasets/CrossSectionDecomposition.Rda"))

#----------------------------------------------------------------------------#
# Time decompostion                                                          #
#----------------------------------------------------------------------------#

# This value is used to choose the minimum number of year to compute the relative time evolution
threshold <- 8

for(j in c("year","time")) { 
## SA portfolio exposure decomposition 
  SA.data %>% filter(year>2007) %>%
  group_by(name, bvdid, Country, year) %>%
  summarise(RWA.SA = sum(RWA, na.rm = TRUE),
            EAD.SA = sum(EAD, na.rm = TRUE)) %>%
  group_by(bvdid) %>% arrange(year) %>%
  mutate(time = year - first(year),
         last = last(time))%>%
  {if(j %in% c("time")) filter(.,last > threshold, time < threshold + 2) else .}%>%
  group_by_at(vars(contains(j))) %>%
  summarise_if(is.numeric, funs(mean), na.rm = TRUE) %>%
  mutate(RW.SA = RWA.SA/EAD.SA) %>% 
  ungroup() %>% 
  assign(paste(j, "decomposition", sep="."),.,inherits = TRUE)

## IRB portfolios exposures decomposition 
for(i in c("Wholesale", "Retail", "Equity")) {
    IRB.data %>% filter(year>2007) %>%
    group_by(name, bvdid, Country, year, Portfolio_1) %>%
    filter(Portfolio_1 %in% c(i)) %>%
    summarise(!!(paste("RWA",        i, sep=".")) := sum(RWA, na.rm = TRUE),
              !!(paste("RWA", "hat", i, sep=".")) := sum(RWA.hat, na.rm = TRUE),
              !!(paste("EAD",        i, sep=".")) := sum(EAD, na.rm = TRUE))%>%
    group_by(bvdid) %>% arrange(year) %>%
    mutate(time = year - first(year),
           last = last(time))%>%
    {if(j %in% c("time")) filter(.,last > threshold, time < threshold + 2) else .}%>%
    group_by_at(vars(contains(j))) %>%
    summarise_if(is.numeric, funs(mean), na.rm = TRUE)%>%
    ungroup()%>%
    mutate(!!(paste("RW" ,"hat", i, sep=".")) := get(paste("RWA", "hat", i, sep ="."))/get(paste("EAD", i, sep=".")),
           !!(paste("RWA","s"  , i, sep=".")) := get(paste("RWA", "hat", i, sep="."))-get(paste("RWA", i, sep=".")),
           !!(paste("RW" ,"s"  , i, sep=".")) := get(paste("RWA", "s", i, sep="."))/get(paste("EAD", i, sep=".")))%>%
    full_join(get(paste(j, "decomposition", sep=".")), by=c(j))%>% 
    assign(paste(j, "decomposition", sep="."),.,inherits = TRUE)
}

## Create portfolio shares, benchmarks, and remaning variables
  get(paste(j, "decomposition", sep=".")) %>%
  rowwise() %>%
  mutate(EAD.IRB      = sum(EAD.Wholesale, EAD.Retail, EAD.Equity, na.rm = TRUE),
         EAD          = sum(EAD.IRB, EAD.SA, na.rm = TRUE),
         RWA.IRB      = sum(RWA.Wholesale, RWA.Retail, RWA.Equity, na.rm = TRUE),
         RW           = sum(RWA.IRB, RWA.SA, na.rm = TRUE)/EAD,
         RW.IRB       = RWA.IRB/EAD.IRB,
         RWA.s        = sum(RWA.s.Wholesale, RWA.s.Retail, RWA.s.Equity, na.rm = TRUE),
         RWA.hat      = sum(RWA.hat.Wholesale, RWA.hat.Retail, RWA.hat.Equity, na.rm = TRUE),
         RW.s         = RWA.s/EAD.IRB,
         RW.hat       = RWA.hat/EAD.IRB,
         q.SA         = EAD.SA/EAD,
         q.Wholesale  = EAD.Wholesale/EAD.IRB,
         q.Retail     = EAD.Retail/EAD.IRB,
         q.Equity     = EAD.Equity/EAD.IRB)%>%
# First year benchmark
  ungroup()%>%
  arrange(get(j)) %>% 
  mutate(t0.RW.s             = first(RW.s),
         t0.RW               = first(RW),
         t0.RW.SA            = first(RW.SA),
         t0.RW.hat.Wholesale = first(RW.hat.Wholesale),
         t0.RW.hat.Retail    = first(RW.hat.Retail),
         t0.RW.hat.Equity    = first(RW.hat.Equity),
         t0.q.SA         = first(q.SA),
         t0.q.Wholesale  = first(q.Wholesale),
         t0.q.Retail     = first(q.Retail),
         t0.q.Equity     = first(q.Equity),
         SA.gain        = t0.q.SA*(RW.SA-t0.RW.SA),
         IRB.gain       = (1-t0.q.SA)*(t0.q.Wholesale*(RW.hat.Wholesale-t0.RW.hat.Wholesale)+
                                       t0.q.Retail*(RW.hat.Retail-t0.RW.hat.Retail)+
                                       t0.q.Equity*(RW.hat.Equity-t0.RW.hat.Equity)),
         mix.gain       = (1-t0.q.SA)*((q.Wholesale-t0.q.Wholesale)*(RW.hat.Wholesale)+
                                       (q.Retail-t0.q.Retail)*(RW.hat.Retail)+
                                       (q.Equity-t0.q.Equity)*(RW.hat.Equity)),
         rollout.gain    = (q.SA-t0.q.SA)*(RW.SA-RW.hat),
         other.gain   = SA.gain + IRB.gain + mix.gain + rollout.gain,
         int.gain     = (1-t0.q.SA)*(t0.RW.s-RW.s),
         ext.gain     = (q.SA-t0.q.SA)*(RW.s),
         PD.gain      = int.gain + ext.gain,
         total.gain = RW-t0.RW)%>% ungroup() %>%
  assign(paste(j, "decomposition", sep="."),.,inherits = TRUE)
  # Save clean data frame
  save(list=paste0(j, ".decomposition"),
       file=paste0("Data/Datasets/",j, "decomposition",".Rda"))
}

proc.time() - ptm