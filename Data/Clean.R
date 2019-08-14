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
  if (is.na(IRB.data[i,"RW"])){
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"EAD"]*mapping_wholesale(IRB.data[i,"w.avg.PD"],
                                                                 IRB.data[i,"LGD"])["RW"]
  } else {
  IRB.data[i,"b"]       <- mapping_wholesale(IRB.data[i,"PD"],IRB.data[i,"LGD"])["b"]
  IRB.data[i,"M"]       <- 2.5+(IRB.data[i,"RW"]*(1-1.5*(IRB.data[i,"b"]))/(12.5*mapping_wholesale(IRB.data[i,"PD"],IRB.data[i,"LGD"])["f"])-1)/IRB.data[i,"b"]
  
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"EAD"]*mapping_wholesale(IRB.data[i,"w.avg.PD"],
                                                               IRB.data[i,"LGD"],
                                                              IRB.data[i,"M"])["RW"]
  }
}

for (i in 1:nrow(IRB.data)){
  if (is.na(IRB.data[i,"Portfolio_2"])){
    next
  }
  if (!IRB.data[i,"Portfolio_2"] %in% c("Real estate")){
    next
  }
  if (is.na(IRB.data[i,"RW"])){
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"w.avg.PD"],
                                                            IRB.data[i,"LGD"],
                                                            0.15)["RW"]
  } else {
  IRB.data[i,"M"]       <- IRB.data[i,"RW"]/(mapping_retail(IRB.data[i,"PD"], IRB.data[i,"LGD"], 0.15)["RW"])
  
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"M"]*IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"w.avg.PD"],
                                                                            IRB.data[i,"LGD"],
                                                                            0.15)["RW"]
  }
}

for (i in 1:nrow(IRB.data)){
  if (is.na(IRB.data[i,"Portfolio_2"])){
    next
  }
  if (!IRB.data[i,"Portfolio_2"] %in% c("Qualifying revolving")){
    next
  }
  if (is.na(IRB.data[i,"RW"])){
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"w.avg.PD"],
                                                            IRB.data[i,"LGD"],
                                                            0.04)["RW"]
  } else {
  IRB.data[i,"M"]       <- IRB.data[i,"RW"]/(mapping_retail(IRB.data[i,"PD"],
                                                            IRB.data[i,"LGD"],
                                                            0.04)["RW"])
  
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"M"]*IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"w.avg.PD"],
                                                                            IRB.data[i,"LGD"],
                                                                            0.04)["RW"]
  }
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
  if (is.na(IRB.data[i,"RW"])){
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"EAD"]*mapping_other_retail(IRB.data[i,"w.avg.PD"],
                                                                  IRB.data[i,"LGD"])["RW"]
  } else {
  IRB.data[i,"M"]       <- IRB.data[i,"RW"]/(mapping_other_retail(IRB.data[i,"PD"],
                                                                  IRB.data[i,"LGD"])["RW"])
  
  IRB.data[i,"RWA.hat"] <- IRB.data[i,"M"]*IRB.data[i,"EAD"]*mapping_other_retail(IRB.data[i,"w.avg.PD"],
                                                                                  IRB.data[i,"LGD"])["RW"] 
  }
}

IRB.data <- IRB.data %>%
  mutate(RWA.hat = Winsorize(RWA.hat,probs = c(0.001, 0.999) ,na.rm = TRUE),
         RWA.hat = ifelse(RWA.hat < 0, 0, RWA.hat)) 

# Save clean data frame
save(pillar3.data,file=paste0("Data/Datasets/Pillar3Data.Rda"))

##============================================================================##
## Orbis Bank Focus (BankScope)                                               ##
##============================================================================##

#------------------------------------------------------------------------------#
# Merge Pillar-III data by bank to bankscope                                   #
#------------------------------------------------------------------------------#

## Aggregated SA exposures at bank-year level
bank.data <- SA.data %>% 
  group_by(name,bvdid,Country,year) %>%
  filter(Method %in% c("SA"))%>%
  summarize(RWA.SA = sum(RWA, na.rm = TRUE),
            EAD.SA = sum(EAD, na.rm = TRUE))%>%
  mutate(RW.SA = RWA.SA/EAD.SA,
         mean.RWA.SA = mean(RWA.SA, na.rm = TRUE),
         mean.EAD.SA = mean(EAD.SA, na.rm = TRUE),
         mean.RW.SA  = mean.RWA.SA/mean.EAD.SA)%>%
  ungroup()%>%
  full_join(bankscope,by=c("bvdid", "name", "Country","year"))

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
  mutate(!!(paste("RW" ,"hat"    , i , sep=".")) := 100*get(paste("RWA","hat", i , sep="."))/get(paste("EAD", i , sep=".")),
         !!(paste("RWA","s", i , sep=".")) := get(paste("RWA","hat", i, sep="."))-get(paste("RWA", i , sep=".")),
         !!(paste("RW" ,"s", i , sep=".")) := 100*ifelse(get(paste("EAD", i, sep=".")) == 0, 0,
                                                     get(paste("RWA","s", i , sep="."))/get(paste("EAD", i , sep="."))),
         !!(paste("mean","RWA",        i, sep=".")) := mean(get(paste(        "RWA",        i, sep=".")), na.rm = TRUE),
         !!(paste("mean","RWA", "hat", i, sep=".")) := mean(get(paste(        "RWA", "hat", i, sep=".")), na.rm = TRUE),
         !!(paste("mean","EAD",        i, sep=".")) := mean(get(paste(        "EAD",        i, sep=".")), na.rm = TRUE),
         !!(paste("mean","RWA", "s"  , i, sep=".")) :=      get(paste("mean", "RWA", "hat", i, sep="."))-get(paste("mean", "RWA", i, sep=".")),
         !!(paste("mean","RW" , "hat", i, sep=".")) :=  100*get(paste("mean", "RWA", "hat", i, sep="."))/get(paste("mean", "EAD", i, sep="."))) %>%
  full_join(bank.data,by=c("bvdid","year","name","Country"))
}

#------------------------------------------------------------------------------#
# Basic cleaning and variable calculation                                      #
#------------------------------------------------------------------------------#

bank.data <- bank.data %>% ungroup() %>% rowwise() %>%
   mutate(EAD.IRB       = sum(EAD.Wholesale, EAD.Retail, EAD.Equity, na.rm = TRUE),
          EAD           = sum(EAD.IRB, EAD.SA, na.rm = TRUE),
          RWA.IRB       = sum(RWA.Wholesale, RWA.Retail, RWA.Equity, na.rm = TRUE),
          RW            = 100*sum(RWA.IRB, RWA.SA, na.rm = TRUE)/EAD,
          RW.IRB        = RWA.IRB/EAD.IRB,
          RWA.s         = sum(RWA.s.Wholesale, RWA.s.Retail, RWA.s.Equity, na.rm = TRUE),
          RWA.hat       = sum(RWA.hat.Wholesale, RWA.hat.Retail, RWA.hat.Equity, na.rm = TRUE),
          RW.s          = 100*ifelse(EAD.IRB == 0 & IRB == 0, 0, RWA.s/EAD.IRB),
          RW.hat        = 100*ifelse(EAD.IRB == 0 & IRB == 0, RW, RWA.hat/EAD.IRB),
          q.SA          = 100*EAD.SA/EAD,
          q.Wholesale   = 100*EAD.Wholesale/EAD.IRB,
          q.Retail      = 100*EAD.Retail/EAD.IRB,
          q.Equity      = 100*EAD.Equity/EAD.IRB,
          sample        = as.factor(ifelse(bvdid %in% pillar3.data$bvdid,"Yes","No")),
          ROE           = plbeforetaxmillcu/equitymillcu*100,
          ROA           = plbeforetaxmillcu/totalassetsmillcu*100,
          RWA.total     = totalcapitalmillcu/totalcapitalratio*100,
          leverage      = totalcapitalmillcu/totalassetsmillcu*100,
          RWA.hat.total = RWA.total + RWA.s,
          CAR.hat       = 100*totalcapitalmillcu/RWA.hat.total,
          tier.hat      = 100*tier1capitalmillcu/RWA.hat.total,
          CAR.dif       = totalcapitalratio - CAR.hat,
          tier.dif      = tier1ratio - tier.hat,
          basel         = ifelse(basel>year,0,1),
          log.RWA.total = log(RWA.total),
          log.RWA.s     = log(RWA.s),
          log.RWA.hat   = log(RWA.hat),
          log.K         = log(totalcapitalmillcu),
          log.tier      = log(tier1capitalmillcu),
          # Benchmark
          mean.EAD.IRB     = sum(mean.EAD.Wholesale, mean.EAD.Retail, mean.EAD.Equity, na.rm = TRUE),
          mean.EAD         = sum(mean.EAD.IRB, mean.EAD.SA, na.rm = TRUE),
          mean.RWA.IRB     = sum(mean.RWA.Wholesale, mean.RWA.Retail, mean.RWA.Equity, na.rm = TRUE),
          mean.RW          = 100*sum(mean.RWA.IRB, mean.RWA.SA, na.rm = TRUE)/mean.EAD,
          mean.RW.IRB      = 100*mean.RWA.IRB/mean.EAD.IRB,
          mean.RWA.s       = sum(mean.RWA.s.Wholesale, mean.RWA.s.Retail, mean.RWA.s.Equity, na.rm = TRUE),
          mean.RW.s        = 100*mean.RWA.s/mean.EAD.IRB,
          mean.q.SA        = 100*mean.EAD.SA/mean.EAD,
          mean.q.Wholesale = 100*mean.EAD.Wholesale/mean.EAD.IRB,
          mean.q.Retail    = 100*mean.EAD.Retail/mean.EAD.IRB,
          mean.q.Equity    = 100*mean.EAD.Equity/mean.EAD.IRB,
          RW.hat.Wholesale = ifelse(is.na(RW.hat.Wholesale),mean.RW.hat.Wholesale,RW.hat.Wholesale),
          RW.hat.Retail    = ifelse(is.na(RW.hat.Retail),mean.RW.hat.Retail,RW.hat.Retail),
          RW.hat.Equity    = ifelse(is.na(RW.hat.Equity),mean.RW.hat.Equity,RW.hat.Equity),
          RW.SA            = ifelse(is.na(RW.SA),mean.RW.SA,RW.SA))%>%
  mutate_at(vars(contains("q.")), rlang::as_function(function(x) ifelse(is.na(x),0,x)))%>%
          # Decomposition variables
  ungroup() %>%
  mutate(mean.K       = mean(totalcapitalmillcu, na.rm = TRUE),
         mean.tier    = mean(tier1capitalmillcu, na.rm = TRUE),
         mean.RWA     = mean(tier1capitalmillcu, na.rm = TRUE),
         CAR.gain     = totalcapitalratio - 100*mean.K/mean.RWA,
         tier.gain    = tier1ratio - 100*mean.tier/mean.RWA,
         SA.gain      = mean.q.SA*(RW.SA-mean.RW.SA),
         IRB.gain     = (100-mean.q.SA)*(mean.q.Wholesale*(RW.hat.Wholesale-mean.RW.hat.Wholesale)+
                                       mean.q.Retail*(RW.hat.Retail-mean.RW.hat.Retail)+
                                       mean.q.Equity*(RW.hat.Equity-mean.RW.hat.Equity)),
         mix.gain     = (100-mean.q.SA)*((q.Wholesale-mean.q.Wholesale)*(RW.hat.Wholesale)+
                                       (q.Retail-mean.q.Retail)*(RW.hat.Retail)+
                                       (q.Equity-mean.q.Equity)*(RW.hat.Equity)),
         PD.gain      = (100-mean.q.SA)*(mean.RW.s-RW.s),
         rollout.gain = (q.SA-mean.q.SA)*(RW.SA-RW.IRB),
         total.gain   = RW-mean.RW)%>%
         # Moving average Z-score
  ungroup()%>% arrange(bvdid,year) %>% group_by(bvdid)%>%
  mutate(r.mean.ROA = rollapply(ROA, 3, mean, na.rm = TRUE, align='right', fill=NA),
         r.mean.CAR = rollapply(leverage, 3, mean, na.rm = TRUE, align='right', fill=NA),
         r.sd.ROA   = rollapply(ROA, 3, sd, na.rm = TRUE, align='right', fill=NA),
         Zscore     = (r.mean.ROA + r.mean.CAR)/r.sd.ROA)

# Save clean data frame
save(bank.data,file=paste0("Data/Datasets/BankData.Rda"))

#------------------------------------------------------------------------------#
# Diff-in-Diff data frame                                                      #
#------------------------------------------------------------------------------#
pre.treat.out <- bank.data %>%
  filter(year %in% c("2009","2010")) %>%
  select(bvdid,tier1ratio,RW.s,RW.s.Wholesale,RW.s.Retail,RW.s.Equity) %>%
  group_by(bvdid) %>%
  summarize_if(is.numeric, funs(mean), na.rm = TRUE)

post.treat.out <- bank.data %>%
  filter(year %in% c("2012","2013")) %>%
  select(bvdid,tier1ratio,RW.s,RW.s.Wholesale,RW.s.Retail,RW.s.Equity) %>%
  group_by(bvdid) %>%
  summarize_if(is.numeric, funs(mean), na.rm = TRUE)

DiD.data <- bank.data %>%
  filter(year %in% c("2010")) %>%
  mutate(Shortfall.RW.2 = ifelse(EBAcountry == 1,
                                 ifelse(EBAbank == 1,Shortfall.RW, 0), NA))%>%
  full_join(pre.treat.out,  by = c("bvdid"), suffix = c("",".pre")) %>%
  full_join(post.treat.out, by = c("bvdid"), suffix = c("",".post")) %>%
  mutate(outcome           = RW.s.post - RW.s.pre,
         outcome.Wholesale = RW.s.Wholesale.post - RW.s.Wholesale.pre,
         outcome.Retail    = RW.s.Retail.post - RW.s.Retail.pre,
         outcome.Equity    = RW.s.Equity.post - RW.s.Equity.pre,
         Country = as.factor(Country))

# Save clean data frame
save(DiD.data,file=paste0("Data/Datasets/DifInDif.Rda"))

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
         RW.s        = RWA.s/EAD.IRB,
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
  mutate(SA.gain = mean.q.SA*(RW.SA-mean.RW.SA),
         IRB.gain       = (1-mean.q.SA)*(mean.q.Wholesale*(RW.hat.Wholesale-mean.RW.hat.Wholesale)+
                                         mean.q.Retail*(RW.hat.Retail-mean.RW.hat.Retail)+
                                         mean.q.Equity*(RW.hat.Equity-mean.RW.hat.Equity)),
         mix.gain       = (1-mean.q.SA)*((q.Wholesale-mean.q.Wholesale)*(RW.hat.Wholesale)+
                                         (q.Retail-mean.q.Retail)*(RW.hat.Retail)+
                                         (q.Equity-mean.q.Equity)*(RW.hat.Equity)),
        dispersion.gain = (1-mean.q.SA)*(mean.RW.s-RW.s),
        rollout.gain    = (q.SA-mean.q.SA)*(RW.SA-RW.IRB),
        total.gain = RW-mean.RW)%>%
  # Specific bank benchmark
  ungroup()

  #arrange(desc(RW.IRB)) %>% 
  #mutate(high.RW.SA            = nth(RW.SA, 2),
  #       high.RW.savings       = nth(RW.savings, 2),
  #       high.RW.hat.Wholesale = nth(RW.hat.Wholesale, 2),
  #       high.RW.hat.Retail    = nth(RW.hat.Retail, 2),
  #       high.RW.hat.Equity    = nth(RW.hat.Equity, 2),
  #       high.q.SA        = nth(q.SA, 2),
  #       high.q.Wholesale = nth(q.Wholesale, 2),
  #       high.q.Retail    = nth(q.Retail, 2),
  #       high.q.Equity    = nth(q.Equity, 2))

# Save clean data frame
save(cross.section.decomposition,file=paste0("Data/Datasets/CrossSectionDecomposition.Rda"))

#----------------------------------------------------------------------------#
# Time decompostion                                                          #
#----------------------------------------------------------------------------#

# This value is used to choose the minimum number of year to compute the relative time evolution
threshold <- 3

for(j in c("year","time")) { 
## SA portfolio exposure decomposition 
  SA.data %>% 
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
    IRB.data %>% 
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
         RW.s         = RWA.s/EAD.IRB,
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
         dispersion.gain = (1-t0.q.SA)*(t0.RW.s-RW.s),
         rollout.gain    = (q.SA-t0.q.SA)*(RW.SA-RW.IRB),
         total.gain = RW-t0.RW)%>% 
  assign(paste(j, "decomposition", sep="."),.,inherits = TRUE)
  # Save clean data frame
  save(list=paste0(j, ".decomposition"),
       file=paste0("Data/Datasets/",j, "decomposition",".Rda"))
}

