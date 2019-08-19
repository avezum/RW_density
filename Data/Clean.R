##=============================================================================##
## Project: PD distribution and capital savings                                ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Description: this file cleans the raw dataset in .R format                  ##
##=============================================================================##

##=============================================================================##
## Code setup                                                                  ##
##=============================================================================##

rm(list = ls())  
library(tidyverse)                ## Data manipulation, pipe operator
library(zoo)                      ## Apply function to rolling margins of data
library(DescTools)                ## Command to winsorize variables
library(readxl)       ## Command to open xlsx files                                                     

source("Auxiliar/Function.R")     ## Capital requirements functions

# Open dirt data frame
load("Data/Temp/BankScope.Rda")
load("Data/Temp/Pillar3Data.Rda")



##============================================================================##
## Pillar-III reports                                                         ##
##============================================================================##

#------------------------------------------------------------------------------#
# Basic cleaning and data creation of raw datasets                             #
#------------------------------------------------------------------------------#

pillar3.data <- as.data.frame(pillar3.data) %>%
  mutate_at(vars(EAD:PD),as.numeric)%>%
  rename(Portfolio_1 = `Portfolio - level 1`,
         Portfolio_2 = `Portfolio - level 2`) %>%
  mutate(month       = as.numeric(sapply(Date, str_sub, start= 4, end=5)),
         year        = as.numeric(sapply(Date, str_sub, start= -4)),
         year        = ifelse(is.na(month), year, ifelse(month<6, year-1, year)),
         bvdid       = as.factor(bvdid),
         Method      = as.factor(Method),
         Method2     = as.factor(ifelse(Method %in% c("IRB"), "IRB", sapply(Method, str_sub, start= 2, end=4))),
         Portfolio_1 = as.factor(Portfolio_1),
         Portfolio_2 = as.factor(Portfolio_2))

# Conver currency values to USD
pillar3.data <- bankscope %>%
  select(bvdid,year,exchangeratefromoriginalcurrencyusd) %>%
  right_join(pillar3.data,by=c("bvdid","year")) %>%
  mutate(EAD = EAD*exchangeratefromoriginalcurrencyusd,
         RWA = RWA*exchangeratefromoriginalcurrencyusd)
  

IRB.data <- as.data.frame(pillar3.data) %>%
  filter(Method2 %in% c("IRB"),
         PD  < 1,  PD  > 0, EAD > 0) %>%
  mutate(LGD         = ifelse(is.na(LGD), 0.45, LGD),
         w.PD        = PD*EAD*LGD,
         w.EAD       = LGD*EAD,
         sum.EAD     = ave(w.EAD, bvdid, year, Portfolio_1, Portfolio_2, FUN = function(x) sum(x, na.rm = TRUE)),
         w.sum.PD    = ave(w.PD, bvdid, year, Portfolio_1, Portfolio_2, FUN = function(x) sum(x, na.rm = TRUE)),
         w.avg.PD    = w.sum.PD/sum.EAD) 
  
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

#IRB.data <- IRB.data %>% group_by(bvdid) %>%
#  mutate(RWA.hat = Winsorize(RWA.hat,probs = c(0.01, 0.99) ,na.rm = TRUE),
#         RWA.hat = ifelse(RWA.hat < 0, 0, RWA.hat)) 

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
  summarize(RWA.SA = sum(RWA, na.rm = TRUE),
            EAD.SA = sum(EAD, na.rm = TRUE))%>%
  ungroup()%>%
  full_join(sample, by=c("bvdid", "name", "Country","year"))%>%
  mutate(RW.SA       = 100*RWA.SA/EAD.SA,
         mean.RWA.SA = mean(RWA.SA, na.rm = TRUE),
         mean.EAD.SA = mean(EAD.SA, na.rm = TRUE),
         mean.RW.SA  = 100*mean.RWA.SA/mean.EAD.SA)%>%
  ungroup()%>%
  right_join(bankscope,by=c("bvdid", "name", "Country","year"))

## Aggregated IRB exposures at bank-portfolio-year level 
for(i in c("Wholesale","Retail","Equity")) {
bank.data <- IRB.data %>% 
  group_by(name,bvdid,Country,year,Portfolio_1) %>%
  filter(Portfolio_1 %in% c(i)) %>%
  summarize(!!(paste(       "gini",        i, sep=".")) := DescTools::Gini(PD,w.EAD, na.rm = TRUE),
            !!(paste("sd"  ,"PD"  ,        i, sep=".")) := sd(PD)*100,
            !!(paste("mean","PD"  ,        i, sep=".")) := weighted.mean(PD,w.EAD, na.rm = TRUE)*100,
            !!(paste(       "RWA" ,        i, sep=".")) := sum(RWA, na.rm = TRUE),
            !!(paste(       "RWA" , "hat", i, sep=".")) := sum(RWA.hat, na.rm = TRUE),
            !!(paste(       "EAD" ,        i, sep=".")) := sum(EAD, na.rm = TRUE))%>%
  ungroup()%>%
  full_join(sample, by=c("bvdid","name","Country","year"))%>%
  mutate(!!(paste("RW" ,"hat"    , i , sep=".")) := 100*get(paste("RWA","hat", i , sep="."))/get(paste("EAD", i , sep=".")),
         !!(paste("RWA","s", i , sep=".")) := get(paste("RWA","hat", i, sep="."))-get(paste("RWA", i , sep=".")),
         !!(paste("RW" ,"s", i , sep=".")) := 100*ifelse(get(paste("EAD", i, sep=".")) == 0, 0,
                                                     get(paste("RWA","s", i , sep="."))/get(paste("EAD", i , sep="."))),
         !!(paste("mean","RWA",        i, sep=".")) := mean(get(paste(        "RWA",        i, sep=".")), na.rm = TRUE),
         !!(paste("mean","RWA", "hat", i, sep=".")) := mean(get(paste(        "RWA", "hat", i, sep=".")), na.rm = TRUE),
         !!(paste("mean","EAD",        i, sep=".")) := mean(get(paste(        "EAD",        i, sep=".")), na.rm = TRUE),
         !!(paste("mean","RWA", "s"  , i, sep=".")) :=      get(paste("mean", "RWA", "hat", i, sep="."))-get(paste("mean", "RWA", i, sep=".")),
         !!(paste("mean","RW" , "hat", i, sep=".")) :=  100*get(paste("mean", "RWA", "hat", i, sep="."))/get(paste("mean", "EAD", i, sep="."))) %>%
  right_join(bank.data,by=c("bvdid","name","Country","year"))
}

#------------------------------------------------------------------------------#
# Basic cleaning and variable calculation                                      #
#------------------------------------------------------------------------------#

bank.data <- bank.data %>% ungroup() %>% rowwise() %>%
   mutate(EAD.IRB       = sum(EAD.Wholesale, EAD.Retail, EAD.Equity, na.rm = TRUE),
          EAD           = sum(EAD.IRB, EAD.SA, na.rm = TRUE),
          RWA.IRB       = sum(RWA.Wholesale, RWA.Retail, RWA.Equity, na.rm = TRUE),
          RWA           = sum(RWA.IRB, RWA.SA, na.rm = TRUE),
          RW            = RWA/EAD,
          RW.IRB        = 100*RWA.IRB/EAD.IRB,
          RWA.s         = sum(RWA.s.Wholesale, RWA.s.Retail, RWA.s.Equity, na.rm = TRUE),
          RWA.hat       = sum(RWA.hat.Wholesale, RWA.hat.Retail, RWA.hat.Equity, na.rm = TRUE),
          RW.s          = 100*ifelse(EAD.IRB == 0 & IRB == 0, 0, RWA.s/EAD.IRB),
          RW.hat        = 100*ifelse(EAD.IRB == 0 & IRB == 0, RW, RWA.hat/EAD.IRB),
          q.SA          = EAD.SA/EAD,
          q.Wholesale   = EAD.Wholesale/EAD.IRB,
          q.Retail      = EAD.Retail/EAD.IRB,
          q.Equity      = EAD.Equity/EAD.IRB,
          sample        = as.factor(ifelse(bvdid %in% pillar3.data$bvdid,"Yes","No")),
          RWA.total     = totalcapitalmillcu/totalcapitalratio*100,
          RWA.other     = sum(RWA.total, -RWA, na.rm = TRUE),
          RWA.hat.total = RWA.total + RWA.s,
          CAR.hat       = 100*totalcapitalmillcu/RWA.hat.total,
          tier.hat      = 100*tier1capitalmillcu/RWA.hat.total,
          basel         = ifelse(basel>year,0,1),
          IRB.bvdid     = paste(bvdid,IRB,sep = ""),
          # Main dependet variables
          CAR.dif        = totalcapitalratio - CAR.hat,
          tier.dif       = tier1ratio - tier.hat,
          log.RWA.total  = log(1+RWA.total),
          log.RWA.credit = log(1+RWA),
          log.RWA.other  = log(1+RWA.other),
          log.RWA.SA     = log(1+RWA.SA),
          log.RWA.IRB    = log(1+RWA.IRB),
          log.RWA.s      = log(1+RWA.s),
          log.RWA.hat    = log(1+RWA.hat),
          log.K          = log(1+totalcapitalmillcu),
          log.tier       = log(1+tier1capitalmillcu),
          # Control variables
          log.asset     = log(1+totalassetsmillcu),
          deposit.ratio = 100*totalcustomerdepositsmillcu/totalassetsmillcu,
          loans.ratio   = 100*grossloansmillcu/totalassetsmillcu,
          NII.ratio     = 100*netinterestrevenuemillcu/operatingrevenueturnovermillcu,
          income.ratio  = 100*plforperiodnetincomemillcu/totalassetsmillcu,
          # Other dependent variables
          ROE           = plbeforetaxmillcu/equitymillcu*100,
          ROA           = plbeforetaxmillcu/totalassetsmillcu*100,
          leverage      = totalcapitalmillcu/totalassetsmillcu*100,
          subordinated  = 100*subordinateddebtsmemomillcu/(totalassetsmillcu - equitymillcu)) %>%
   rename(NPL           = nonperfloansgrossloans,
          LLR           = loanlossresgrossloans) %>%
          # Moving average Z-score
          ungroup()%>% arrange(bvdid,year) %>% group_by(bvdid)%>%
   mutate(r.mean.ROA = rollapply(ROA, 3, mean, na.rm = TRUE, align='right', fill=NA),
          r.mean.CAR = rollapply(leverage, 3, mean, na.rm = TRUE, align='right', fill=NA),
          r.sd.ROA   = rollapply(ROA, 3, sd, na.rm = TRUE, align='right', fill=NA),
          Zscore     = (r.mean.ROA + r.mean.CAR)/r.sd.ROA)%>%
          # Decomposition at bank-level
          ungroup() %>% rowwise() %>%
   mutate(mean.EAD.IRB     = sum(mean.EAD.Wholesale, mean.EAD.Retail, mean.EAD.Equity, na.rm = TRUE),
          mean.EAD         = sum(mean.EAD.IRB, mean.EAD.SA, na.rm = TRUE),
          mean.RWA.IRB     = sum(mean.RWA.Wholesale, mean.RWA.Retail, mean.RWA.Equity, na.rm = TRUE),
          mean.RW          = 100*sum(mean.RWA.IRB, mean.RWA.SA, na.rm = TRUE)/mean.EAD,
          mean.RW.IRB      = 100*mean.RWA.IRB/mean.EAD.IRB,
          mean.RWA.s       = sum(mean.RWA.s.Wholesale, mean.RWA.s.Retail, mean.RWA.s.Equity, na.rm = TRUE),
          mean.RW.s        = 100*mean.RWA.s/mean.EAD.IRB,
          mean.q.SA        = mean.EAD.SA/mean.EAD,
          mean.q.Wholesale = mean.EAD.Wholesale/mean.EAD.IRB,
          mean.q.Retail    = mean.EAD.Retail/mean.EAD.IRB,
          mean.q.Equity    = mean.EAD.Equity/mean.EAD.IRB,
          RW.hat.Wholesale = ifelse(is.na(RW.hat.Wholesale),mean.RW.hat.Wholesale,RW.hat.Wholesale),
          RW.hat.Retail    = ifelse(is.na(RW.hat.Retail),mean.RW.hat.Retail,RW.hat.Retail),
          RW.hat.Equity    = ifelse(is.na(RW.hat.Equity),mean.RW.hat.Equity,RW.hat.Equity),
          RW.SA            = ifelse(is.na(RW.SA),mean.RW.SA,RW.SA))%>%
  mutate_at(vars(contains("q.")), rlang::as_function(function(x) ifelse(is.na(x),0,x)))%>%
          # Decomposition variables
  ungroup() %>%
  mutate(SA.gain      = mean.q.SA*(RW.SA-mean.RW.SA),
         IRB.gain     = (1-mean.q.SA)*(mean.q.Wholesale*(RW.hat.Wholesale-mean.RW.hat.Wholesale)+
                                       mean.q.Retail*(RW.hat.Retail-mean.RW.hat.Retail)+
                                       mean.q.Equity*(RW.hat.Equity-mean.RW.hat.Equity)),
         mix.gain     = (1-mean.q.SA)*((q.Wholesale-mean.q.Wholesale)*(RW.hat.Wholesale)+
                                       (q.Retail-mean.q.Retail)*(RW.hat.Retail)+
                                       (q.Equity-mean.q.Equity)*(RW.hat.Equity)),
         rollout.gain = (q.SA-mean.q.SA)*(RW.SA-RW.hat),
         other.gain   = SA.gain + IRB.gain + mix.gain + rollout.gain,
         int.gain     = (1-mean.q.SA)*(mean.RW.s-RW.s),
         ext.gain     = (q.SA-mean.q.SA)*(RW.s),
         PD.gain      = int.gain + ext.gain,
         total.gain   = RW-mean.RW)
        
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
  summarize(RWA.SA = sum(RWA, na.rm = TRUE),
            EAD.SA = sum(EAD, na.rm = TRUE))%>%
  group_by(name, bvdid, Country) %>%
  summarize_if(is.numeric, funs(mean), na.rm = TRUE)%>%
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
    summarize(!!(paste("RWA",        i, sep=".")) := sum(RWA    , na.rm = TRUE),
              !!(paste("RWA", "hat", i, sep=".")) := sum(RWA.hat, na.rm = TRUE),
              !!(paste("EAD",        i, sep=".")) := sum(EAD    , na.rm = TRUE))%>%
    group_by(name, bvdid, Country) %>%
    summarize_if(is.numeric, funs(mean), na.rm = TRUE)%>%
    ungroup()%>%select(-year)%>%
    full_join(sample, by=c("bvdid","name","Country"))%>%
    mutate(!!(paste(       "RWA", "s"  , i, sep=".")) :=      get(paste(        "RWA", "hat", i, sep="."))-get(paste("RWA", i, sep=".")),
           !!(paste(       "RW" , "hat", i, sep=".")) :=      get(paste(        "RWA", "hat", i, sep="."))/get(paste("EAD", i, sep=".")),
           !!(paste("mean","RWA",        i, sep=".")) := mean(get(paste(        "RWA",        i, sep=".")), na.rm = TRUE),
           !!(paste("mean","RWA", "hat", i, sep=".")) := mean(get(paste(        "RWA", "hat", i, sep=".")), na.rm = TRUE),
           !!(paste("mean","EAD",        i, sep=".")) := mean(get(paste(        "EAD",        i, sep=".")), na.rm = TRUE),
           !!(paste("mean","RWA", "s"  , i, sep=".")) :=      get(paste("mean", "RWA", "hat", i, sep="."))-get(paste("mean", "RWA", i, sep=".")),
           !!(paste("mean","RW" , "hat", i, sep=".")) :=      get(paste("mean", "RWA", "hat", i, sep="."))/get(paste("mean", "EAD", i, sep="."))) %>%
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
         mean.EAD         = sum(mean.EAD.IRB, mean.EAD.SA, na.rm = TRUE),
         mean.RWA.IRB     = sum(mean.RWA.Wholesale, mean.RWA.Retail, mean.RWA.Equity, na.rm = TRUE),
         mean.RW          = sum(mean.RWA.IRB, mean.RWA.SA, na.rm = TRUE)/mean.EAD,
         mean.RW.IRB      = mean.RWA.IRB/mean.EAD.IRB,
         mean.RWA.s       = sum(mean.RWA.s.Wholesale, mean.RWA.s.Retail, mean.RWA.s.Equity, na.rm = TRUE),
         mean.RW.s        = mean.RWA.s/mean.EAD.IRB,
         mean.q.SA        = mean.EAD.SA/mean.EAD,
         mean.q.Wholesale = mean.EAD.Wholesale/mean.EAD.IRB,
         mean.q.Retail    = mean.EAD.Retail/mean.EAD.IRB,
         mean.q.Equity    = mean.EAD.Equity/mean.EAD.IRB,
         RW.hat.Wholesale = ifelse(is.na(RW.hat.Wholesale),mean.RW.hat.Wholesale,RW.hat.Wholesale),
         RW.hat.Retail    = ifelse(is.na(RW.hat.Retail),mean.RW.hat.Retail,RW.hat.Retail),
         RW.hat.Equity    = ifelse(is.na(RW.hat.Equity),mean.RW.hat.Equity,RW.hat.Equity),
         RW.SA            = ifelse(is.na(RW.SA),mean.RW.SA,RW.SA))%>%
  mutate_at(vars(contains("q.")), 
            rlang::as_function(function(x) ifelse(is.na(x),0,x)))%>%
  mutate(SA.gain     = mean.q.SA*(RW.SA-mean.RW.SA),
         IRB.gain    = (1-mean.q.SA)*(mean.q.Wholesale*(RW.hat.Wholesale-mean.RW.hat.Wholesale)+
                                         mean.q.Retail*(RW.hat.Retail-mean.RW.hat.Retail)+
                                         mean.q.Equity*(RW.hat.Equity-mean.RW.hat.Equity)),
         mix.gain    = (1-mean.q.SA)*((q.Wholesale-mean.q.Wholesale)*(RW.hat.Wholesale)+
                                         (q.Retail-mean.q.Retail)*(RW.hat.Retail)+
                                         (q.Equity-mean.q.Equity)*(RW.hat.Equity)),
        rollout.gain = (q.SA-mean.q.SA)*(RW.SA-RW.hat),
        other.gain   = SA.gain + IRB.gain + mix.gain + rollout.gain,
        int.gain     = (1-mean.q.SA)*(mean.RW.s-RW.s),
        ext.gain     = (q.SA-mean.q.SA)*(RW.s),
        PD.gain      = int.gain + ext.gain,
        total.gain   = RW-mean.RW)%>%
  # Specific bank benchmark
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
  summarize(RWA.SA = sum(RWA, na.rm = TRUE),
            EAD.SA = sum(EAD, na.rm = TRUE)) %>%
  group_by(bvdid) %>% arrange(year) %>%
  mutate(time = year - first(year),
         last = last(time))%>%
  {if(j %in% c("time")) filter(.,last > threshold, time < threshold + 2) else .}%>%
  group_by_at(vars(contains(j))) %>%
  summarize_if(is.numeric, funs(mean), na.rm = TRUE) %>%
  mutate(RW.SA = RWA.SA/EAD.SA) %>% 
  ungroup() %>% 
  assign(paste(j, "decomposition", sep="."),.,inherits = TRUE)

## IRB portfolios exposures decomposition 
for(i in c("Wholesale", "Retail", "Equity")) {
    IRB.data %>% filter(year>2007) %>%
    group_by(name, bvdid, Country, year, Portfolio_1) %>%
    filter(Portfolio_1 %in% c(i)) %>%
    summarize(!!(paste("RWA",        i, sep=".")) := sum(RWA, na.rm = TRUE),
              !!(paste("RWA", "hat", i, sep=".")) := sum(RWA.hat, na.rm = TRUE),
              !!(paste("EAD",        i, sep=".")) := sum(EAD, na.rm = TRUE))%>%
    group_by(bvdid) %>% arrange(year) %>%
    mutate(time = year - first(year),
           last = last(time))%>%
    {if(j %in% c("time")) filter(.,last > threshold, time < threshold + 2) else .}%>%
    group_by_at(vars(contains(j))) %>%
    summarize_if(is.numeric, funs(mean), na.rm = TRUE)%>%
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

