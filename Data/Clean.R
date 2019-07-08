##-----------------------------------------------------------------------------##
## Project: PD distribution and capital savings                                ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Description: this file cleans the raw dataset in .R format                  ##
##-----------------------------------------------------------------------------##

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

# Create and adjust variables 
pillar3.data <- pillar3.data %>%
  rename(Portfolio_1 = `Portfolio - level 1`,
         Portfolio_2 = `Portfolio - level 2`) %>%
  mutate(month       = as.numeric(sapply(Date,str_sub,start= 4,end=5)),
         year        = as.numeric(sapply(Date,str_sub,start= -4)),
         year        = ifelse(is.na(month),year,ifelse(month<7,year-1,year)),
         PD          = as.numeric(PD),
         RW          = as.numeric(RW),
         EAD         = as.numeric(EAD),
         bvdid       = as.factor(bvdid),
         Method      = as.factor(Method),
         Method2     = as.factor(ifelse(Method %in% c("IRB"), "IRB",sapply(Method,str_sub,start= 2,end=4))),
         Portfolio_1 = as.factor(Portfolio_1),
         Portfolio_2 = as.factor(Portfolio_2),
         K_original  = RW*EAD*0.08,
         PD_w        = PD*EAD*LGD,
         EAD_w       = LGD*EAD,
         EAD_sum     = ave(EAD_w,Bank,year,Portfolio_1,Portfolio_2,FUN = function(x) sum(x,na.rm = TRUE)),
         PD_w_sum    = ave(PD_w,Bank,year,Portfolio_1,Portfolio_2,FUN = function(x) sum(x,na.rm = TRUE)),
         PD_w_avg    = PD_w_sum/EAD_sum) 



# Calculate capital requirement
for (i in 1:nrow(pillar3.data)){
  if (pillar3.data[i,"Portfolio_1"]!="Wholesale"){
    next
  }
  pillar3.data[i,"K_calculated"] <- pillar3.data[i,"EAD"]*mapping_wholesale(pillar3.data[i,"PD"],pillar3.data[i,"LGD"],2.5,0.999,"")["K"]
  
  pillar3.data[i,"K_counterfactual"] <- pillar3.data[i,"EAD"]*mapping_wholesale(pillar3.data[i,"PD_w_avg"],pillar3.data[i,"LGD"],2.5,0.999,"")["K"]
}

for (i in 1:nrow(pillar3.data)){
  if (is.na(pillar3.data[i,"Portfolio_2"])){
    next
  }
  if (!pillar3.data[i,"Portfolio_2"]%in%c("Real estate")){
    next
  }
  pillar3.data[i,"K_calculated"] <- pillar3.data[i,"EAD"]*mapping_retail(pillar3.data[i,"PD"],pillar3.data[i,"LGD"],0.15,0.999,"")["K"]
  
  pillar3.data[i,"K_counterfactual"] <- pillar3.data[i,"EAD"]*mapping_retail(pillar3.data[i,"PD_w_avg"],pillar3.data[i,"LGD"],0.15,0.999,"")["K"]
  
}

for (i in 1:nrow(pillar3.data)){
  if (is.na(pillar3.data[i,"Portfolio_2"])){
    next
  }
  if (!pillar3.data[i,"Portfolio_2"]%in%c("Qualifying revolving")){
    next
  }
  pillar3.data[i,"K_calculated"] <- pillar3.data[i,"EAD"]*mapping_retail(pillar3.data[i,"PD"],pillar3.data[i,"LGD"],0.04,0.999,"")["K"]
  
  pillar3.data[i,"K_counterfactual"] <- pillar3.data[i,"EAD"]*mapping_retail(pillar3.data[i,"PD_w_avg"],pillar3.data[i,"LGD"],0.04,0.999,"")["K"]
  
}
for (i in 1:nrow(pillar3.data)){
  if (pillar3.data[i,"Portfolio_1"]%in%c("Wholesale")){
    next
  }
  if (is.na(pillar3.data[i,"Portfolio_2"])){
    next
  }
  if (!pillar3.data[i,"Portfolio_2"]%in%c("Total","Other retail")){
    next
  }
  pillar3.data[i,"K_calculated"] <- pillar3.data[i,"EAD"]*mapping_other_retail(pillar3.data[i,"PD"],pillar3.data[i,"LGD"],0.999,"")["K"]
  
  pillar3.data[i,"K_counterfactual"] <- pillar3.data[i,"EAD"]*mapping_other_retail(pillar3.data[i,"PD_w_avg"],pillar3.data[i,"LGD"],0.999,"")["K"] 
  
}


# Save clean data frame
save(pillar3.data,file=paste0("Data/Datasets/Pillar3Data.Rda"))

##============================================================================##
## Orbis Bank Focus (BankScope)                                               ##
##============================================================================##

# Open dirt data frame
load("Data/Temp/BankScope.Rda")

bank.data <- bankscope %>%
  mutate(sample = as.factor(ifelse(bvdid %in% pillar3.data$bvdid,"Yes","No")),
         mean.ROA = ave(ROA.using.P.L.before.tax....,bvdid,FUN = function(x) mean(x, na.rm = TRUE)),
         sd.ROA   = ave(ROA.using.P.L.before.tax....,bvdid,FUN = function(x) sd(x, na.rm = TRUE)),
         mean.CAR = ave(Equity...Total.assets....,bvdid,FUN = function(x) mean(x, na.rm = TRUE))) %>%
  mutate('Z-score'=(mean.ROA+mean.CAR)/sd.ROA)


# Save clean data frame
save(bank.data,file=paste0("Data/Datasets/BankData.Rda"))