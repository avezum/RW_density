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
source("Auxiliar/Function.R")     ## Capital requirements functions

##============================================================================##
## Pillar-III reports                                                         ##
##============================================================================##

# Open dirt data frame
load("Data/Temp/Pillar3Data.Rda")

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

IRB.data <- pillar3.data %>%
  filter(Method2 %in% c("IRB"),
         PD  < 1,  PD  > 0, EAD > 0) %>%
  mutate(LGD         = ifelse(is.na(LGD), 0.45, LGD),
         w.PD        = PD*EAD*LGD,
         w.EAD       = LGD*EAD,
         sum.EAD     = ave(w.EAD, bvdid, year, Portfolio_1, Portfolio_2, FUN = function(x) sum(x, na.rm = TRUE)),
         w.sum.PD    = ave(w.PD, bvdid, year, Portfolio_1, Portfolio_2, FUN = function(x) sum(x, na.rm = TRUE)),
         w.avg.PD    = w.sum.PD/sum.EAD) 
  
SA.data <- pillar3.data %>%
  filter(!Method2 %in% c("IRB"),
         EAD > 0)

#------------------------------------------------------------------------------#
# Capital requirement calculations                                             #
#------------------------------------------------------------------------------#

for (i in 1:nrow(IRB.data)){
  if (!IRB.data[i,"Portfolio_1"] %in% c("Wholesale","Equity")){
    next
  }
  IRB.data[i,"K"]     <- IRB.data[i,"EAD"]*mapping_wholesale(IRB.data[i,"PD"],
                                                             IRB.data[i,"LGD"])["K"]
  IRB.data[i,"K.hat"] <- IRB.data[i,"EAD"]*mapping_wholesale(IRB.data[i,"w.avg.PD"],
                                                             IRB.data[i,"LGD"])["K"]
}

for (i in 1:nrow(IRB.data)){
  if (is.na(IRB.data[i,"Portfolio_2"])){
    next
  }
  if (!IRB.data[i,"Portfolio_2"] %in% c("Real estate")){
    next
  }
  IRB.data[i,"K"]     <- IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"PD"],
                                                          IRB.data[i,"LGD"],
                                                                  0.15)["K"]
  IRB.data[i,"K.hat"] <- IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"w.avg.PD"],
                                                          IRB.data[i,"LGD"],
                                                                  0.15)["K"]
}

for (i in 1:nrow(IRB.data)){
  if (is.na(IRB.data[i,"Portfolio_2"])){
    next
  }
  if (!IRB.data[i,"Portfolio_2"] %in% c("Qualifying revolving")){
    next
  }
  IRB.data[i,"K"]     <- IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"PD"],
                                                          IRB.data[i,"LGD"],
                                                                  0.04)["K"]
  IRB.data[i,"K.hat"] <- IRB.data[i,"EAD"]*mapping_retail(IRB.data[i,"w.avg.PD"],
                                                          IRB.data[i,"LGD"],
                                                                  0.04)["K"]
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
  IRB.data[i,"K"]     <- IRB.data[i,"EAD"]*mapping_other_retail(IRB.data[i,"PD"],
                                                                IRB.data[i,"LGD"])["K"]
  IRB.data[i,"K.hat"] <- IRB.data[i,"EAD"]*mapping_other_retail(IRB.data[i,"w.avg.PD"],
                                                                IRB.data[i,"LGD"])["K"] 
}

# Save clean data frame
save(pillar3.data,file=paste0("Data/Datasets/Pillar3Data.Rda"))

##============================================================================##
## Orbis Bank Focus (BankScope)                                               ##
##============================================================================##

# Open dirt data frame
load("Data/Temp/BankScope.Rda")

#------------------------------------------------------------------------------#
# Merge Pillar-III data by bank to bankscope                                   #
#------------------------------------------------------------------------------#

## Aggregated IRB exposures at bank-portfolio-year level 
for(i in c("Wholesale","Retail","Equity")) {
bank.data <- IRB.data %>% 
  group_by(name,bvdid,Country,year,Portfolio_1) %>%
  filter(Portfolio_1 %in% c(i)) %>%
  summarize(!!(paste(       "gini",        i, sep=".")) := DescTools::Gini(PD,w.EAD, na.rm = TRUE),
            !!(paste("sd"  ,"PD"  ,        i, sep=".")) := sd(PD)*100,
            !!(paste("mean","PD"  ,        i, sep=".")) := weighted.mean(PD,w.EAD, na.rm = TRUE)*100,
            !!(paste(       "K"   ,        i, sep=".")) := sum(K, na.rm = TRUE),
            !!(paste(       "K"   , "hat", i, sep=".")) := sum(K.hat, na.rm = TRUE),
            !!(paste(       "EAD" ,        i, sep=".")) := sum(EAD, na.rm = TRUE))%>%
  mutate(!!(paste("RW" ,"hat"    , i , sep=".")) := get(paste("K","hat", i , sep="."))*12.5/get(paste("EAD", i , sep=".")),
         !!(paste("RWA","savings", i , sep=".")) := (get(paste("K","hat", i , sep="."))-get(paste("K", i , sep=".")))*12.5,
         !!(paste("RW" ,"savings", i , sep=".")) := get(paste("RWA","savings", i , sep="."))/get(paste("EAD", i , sep=".")))%>%
  full_join(bankscope,by=c("bvdid","year"))
}

## Aggregated SA exposures at bank-year level
bank.data <- SA.data %>% 
  group_by(name,bvdid,Country,year) %>%
  filter(Method %in% c("SA"))%>%
  summarize(RWA.SA = sum(RWA, na.rm = TRUE),
            EAD.SA = sum(EAD, na.rm = TRUE))%>%
  mutate(RW.SA = RWA.SA/EAD.SA)%>%
  ungroup()%>%
  full_join(bank.data,by=c("bvdid","year"))%>%
  
#------------------------------------------------------------------------------#
# Basic cleaning and variable calculation                                      #
#------------------------------------------------------------------------------#
  rowwise() %>%
   mutate(EAD.IRB     = sum(EAD.Wholesale, EAD.Retail, EAD.Equity, na.rm = TRUE),
          EAD         = sum(EAD.IRB, EAD.SA, na.rm = TRUE),
          RWA.IRB     = sum(K.Wholesale, K.Retail, K.Equity, na.rm = TRUE)*12.5,
          RW          = sum(RWA.IRB, RWA.SA, na.rm = TRUE)/EAD,
          RW.IRB      = RWA.IRB/EAD.IRB,
          RWA.savings = sum(RWA.savings.Wholesale, RWA.savings.Retail, RWA.savings.Equity, na.rm = TRUE),
          RW.savings  = RWA.savings/EAD.IRB,
          sample   = as.factor(ifelse(bvdid %in% pillar3.data$bvdid,"Yes","No")),
          ROE      = plbeforetaxmillcu/equitymillcu*100,
          ROA      = plbeforetaxmillcu/totalassetsmillcu*100,
          RWA      = totalcapitalmillcu/totalcapitalratio*100,
          leverage = totalcapitalmillcu/totalassetsmillcu*100,
          RWA.hat  = RWA + RWA.savings,
          CAR.hat  = 100*totalcapitalmillcu/RWA.hat,
          tier.hat = 100*tier1capitalmillcu/RWA.hat) %>%
  ungroup()%>%
  mutate(mean.ROA = ave(ROA,bvdid,FUN = function(x) mean(x, na.rm = TRUE)),
         sd.ROA   = ave(ROA,bvdid,FUN = function(x) sd(x, na.rm = TRUE)),
         mean.CAR = ave(leverage,bvdid,FUN = function(x) mean(x, na.rm = TRUE)),
         Zscore   = (mean.ROA+mean.CAR)/sd.ROA)

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
    summarize(!!(paste("K"  ,       i, sep=".")) := sum(K, na.rm = TRUE),
              !!(paste("K"  ,"hat", i, sep=".")) := sum(K.hat, na.rm = TRUE),
              !!(paste("EAD",       i, sep=".")) := sum(EAD, na.rm = TRUE))%>%
    group_by(name, bvdid, Country) %>%
    summarize_if(is.numeric, funs(mean), na.rm = TRUE)%>%
    ungroup()%>%select(-year)%>%
    full_join(sample, by=c("bvdid","name","Country"))%>%
    mutate(!!(paste(       "RW" ,           i, sep=".")) := get(paste("K", i, sep="."))*12.5/get(paste("EAD", i, sep=".")),
           !!(paste(       "RW" , "hat",    i, sep=".")) := get(paste("K", "hat", i, sep="."))*12.5/get(paste("EAD", i, sep=".")),
           !!(paste(       "RWA","savings", i, sep=".")) := (get(paste("K", "hat", i, sep="."))-get(paste("K", i, sep=".")))*12.5,
           !!(paste(       "RW" ,"savings", i, sep=".")) := get(paste("RWA","savings", i , sep="."))/get(paste("EAD", i, sep=".")),
           !!(paste("mean","RWA",           i, sep=".")) := mean(get(paste("K", i, sep=".")), na.rm = TRUE)*12.5,
           !!(paste("mean","RWA","hat",     i, sep=".")) := mean(get(paste("K","hat", i, sep=".")), na.rm = TRUE)*12.5,
           !!(paste("mean","EAD",           i, sep=".")) := mean(get(paste("EAD", i, sep=".")), na.rm = TRUE),
           !!(paste("mean","RW" ,"hat",     i, sep=".")) := get(paste("mean", "RWA", "hat", i, sep="."))/get(paste("mean", "EAD", i, sep=".")),
           !!(paste("mean","RWA","savings", i, sep=".")) := get(paste("mean", "RWA", "hat", i, sep="."))-get(paste("mean", "RWA", i, sep=".")),
           !!(paste("mean","RW" ,"savings", i, sep=".")) := get(paste("mean", "RWA", "savings", i, sep="."))/get(paste("mean", "EAD", i, sep=".")))%>%
    full_join(cross.section.decomposition,by=c("bvdid","name","Country"))
}

## Create portfolio shares, benchmarks, and remaning variables
cross.section.decomposition <- cross.section.decomposition %>%
  rowwise() %>%
  mutate(EAD.IRB     = sum(EAD.Wholesale, EAD.Retail, EAD.Equity, na.rm = TRUE),
         EAD         = sum(EAD.IRB, EAD.SA, na.rm = TRUE),
         RWA.IRB     = sum(K.Wholesale, K.Retail, K.Equity, na.rm = TRUE)*12.5,
         RW          = sum(RWA.IRB, RWA.SA, na.rm = TRUE)/EAD,
         RW.IRB      = RWA.IRB/EAD.IRB,
         RWA.savings = sum(RWA.savings.Wholesale, RWA.savings.Retail, RWA.savings.Equity, na.rm = TRUE),
         RW.savings  = RWA.savings/EAD.IRB,
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
         mean.RWA.savings = sum(mean.RWA.savings.Wholesale, mean.RWA.savings.Retail, mean.RWA.savings.Equity, na.rm = TRUE),
         mean.RW.savings  = mean.RWA.savings/mean.EAD.IRB,
         mean.q.SA        = mean.EAD.SA/(mean.EAD.SA + mean.EAD.IRB),
         mean.q.Wholesale = mean.EAD.Wholesale/mean.EAD.IRB,
         mean.q.Retail    = mean.EAD.Retail/mean.EAD.IRB,
         mean.q.Equity    = mean.EAD.Equity/mean.EAD.IRB,
         RW.hat.Wholesale  = ifelse(is.na(RW.hat.Wholesale),mean.RW.hat.Wholesale,RW.hat.Wholesale),
         RW.hat.Retail    = ifelse(is.na(RW.hat.Retail),mean.RW.hat.Retail,RW.hat.Retail),
         RW.hat.Equity    = ifelse(is.na(RW.hat.Equity),mean.RW.hat.Equity,RW.hat.Equity),
         RW.SA            = ifelse(is.na(RW.SA),mean.RW.SA,RW.SA))%>%
  mutate_at(vars(contains("q")), 
            rlang::as_function(function(x) ifelse(is.na(x),0,x)))%>%
  # Specific bank benchmark
  ungroup()%>%
  arrange(desc(RW.IRB)) %>% 
  mutate(high.RW.SA            = nth(RW.SA, 2),
         high.RW.savings       = nth(RW.savings, 2),
         high.RW.hat.Wholesale = nth(RW.hat.Wholesale, 2),
         high.RW.hat.Retail    = nth(RW.hat.Retail, 2),
         high.RW.hat.Equity    = nth(RW.hat.Equity, 2),
         high.q.SA        = nth(q.SA, 2),
         high.q.Wholesale = nth(q.Wholesale, 2),
         high.q.Retail    = nth(q.Retail, 2),
         high.q.Equity    = nth(q.Equity, 2))

# Save clean data frame
save(cross.section.decomposition,file=paste0("Data/Datasets/CrossSectionDecomposition.Rda"))

#----------------------------------------------------------------------------#
# Time decompostion                                                          #
#----------------------------------------------------------------------------#

# This value is used to choose the minimum number of year to compute the relative time evolution
threshold <- 4

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
    summarize(!!(paste("K"  ,        i, sep=".")) := sum(K, na.rm = TRUE),
              !!(paste("K"  , "hat", i, sep=".")) := sum(K.hat, na.rm = TRUE),
              !!(paste("EAD",        i, sep=".")) := sum(EAD, na.rm = TRUE))%>%
    group_by(bvdid) %>% arrange(year) %>%
    mutate(time = year - first(year),
           last = last(time))%>%
    {if(j %in% c("time")) filter(.,last > threshold, time < threshold + 2) else .}%>%
    group_by_at(vars(contains(j))) %>%
    summarize_if(is.numeric, funs(mean), na.rm = TRUE)%>%
    ungroup()%>%
    mutate(!!(paste("RW" ,"hat",     i, sep=".")) := get(paste("K", "hat", i, sep ="."))*12.5/get(paste("EAD", i, sep=".")),
           !!(paste("RWA","savings", i, sep=".")) := (get(paste("K", "hat", i, sep="."))-get(paste("K", i, sep=".")))*12.5,
           !!(paste("RW" ,"savings", i, sep=".")) := get(paste("RWA", "savings", i, sep="."))/get(paste("EAD", i, sep=".")))%>%
    full_join(get(paste(j, "decomposition", sep=".")), by=c(j))%>% 
    assign(paste(j, "decomposition", sep="."),.,inherits = TRUE)
}

## Create portfolio shares, benchmarks, and remaning variables
  get(paste(j, "decomposition", sep=".")) %>%
  rowwise() %>%
  mutate(EAD.IRB      = sum(EAD.Wholesale, EAD.Retail, EAD.Equity, na.rm = TRUE),
         EAD          = sum(EAD.IRB, EAD.SA, na.rm = TRUE),
         RWA.IRB      = sum(K.Wholesale, K.Retail, K.Equity, na.rm = TRUE)*12.5,
         RW           = sum(RWA.IRB, RWA.SA, na.rm = TRUE)/EAD,
         RW.IRB       = RWA.IRB/EAD.IRB,
         RWA.savings  = sum(RWA.savings.Wholesale, RWA.savings.Retail, RWA.savings.Equity, na.rm = TRUE),
         RW.savings   = RWA.savings/EAD.IRB,
         q.SA         = EAD.SA/EAD,
         q.Wholesale  = EAD.Wholesale/EAD.IRB,
         q.Retail     = EAD.Retail/EAD.IRB,
         q.Equity     = EAD.Equity/EAD.IRB)%>%
# First year benchmark
  ungroup()%>%
  arrange(get(j)) %>% 
  mutate(t0.RW.savings       = first(RW.savings),
         t0.RW.SA            = first(RW.SA),
         t0.RW.hat.Wholesale = first(RW.hat.Wholesale),
         t0.RW.hat.Retail    = first(RW.hat.Retail),
         t0.RW.hat.Equity    = first(RW.hat.Equity),
         t0.q.SA         = first(q.SA),
         t0.q.Wholesale  = first(q.Wholesale),
         t0.q.Retail     = first(q.Retail),
         t0.q.Equity     = first(q.Equity))%>% 
  assign(paste(j, "decomposition", sep="."),.,inherits = TRUE)
  # Save clean data frame
  save(list=paste0(j, ".decomposition"),
       file=paste0("Data/Datasets/",j, "decomposition",".Rda"))
}

