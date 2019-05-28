##-----------------------------------------------------------------------------##
## Project: Bank Regulation and Capital Structure                              ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Date: 10/10/2018                                                            ##
## Description: this file opens, merges and saves in .Rda format the following ## 
## datasets: Dealscan, EBA-transparency exercises                              ##
##-----------------------------------------------------------------------------##
  
##=============================================================================##
## Code setup                                                                  ##
##=============================================================================##

rm(list = ls())                                                                ## Clean enviroment,
library(tidyverse)                                                             ## excel files and,
library(readxl)                                                            ## load the libraries.
library(data.table)
library(foreign)
library(lubridate)
library(lfe)
library(stargazer)
library(ineq)
library(DescTools)
library(gglorenz)

##============================================================================##
## Dealscan                                                                   ##
##============================================================================##


data.path  <- "../../../Data/Financial/Exposure_data_collection/Data/Second_list"
data.names <-  list.files(path = data.path,pattern="\\.xlsx",full.names = TRUE)
data.list  <- data.names %>%
  lapply(read_xlsx,sheet = 1,  skip=1)   
 

data <-  rbindlist(data.list, use.names=TRUE,fill = TRUE) %>%
  select("Bank","date","Method","Portfolio- level 1","Portfolio- level 2","Portfolio- level 3",
         "CPCR","SL","Location of Borrower",
         "Exposure at default - mln (a)","Risk-weight (c)= (b)/(a)",
         "LGD (exposure-weighted average)",
         "PD (exposure-weighted average)")

data <- plyr::rename(data,c("Portfolio- level 1"="Portfolio_lv1","Portfolio- level 2"="Portfolio_lv2",
                            "Portfolio- level 3"="Portfolio_lv3",
                            "Location of Borrower"="Location",
                            "Exposure at default - mln (a)"="EAD",
                            "Risk-weight (c)= (b)/(a)"="RW",
                            "LGD (exposure-weighted average)"="LGD",
                            "PD (exposure-weighted average)"="PD"))
data <- filter(data,Location=="Aggregate"|Location=="Aggregated"& Bank != "Citi")

#data$year <- sapply(EBA.df$item,toString) 
data$month <- sapply(data$date,str_sub,start= 4,end=5)
data$year <- sapply(data$date,str_sub,start= -4)
data <- mutate(data,year=as.numeric(year),month=as.numeric(month)) 
data <- mutate(data,year=ifelse(month<7,year-1,year))
data <- mutate(data,Portfolio_lv2=ifelse(Portfolio_lv2=="Real Estate","Real estate",Portfolio_lv2))
data <- mutate(data,Portfolio_lv2=ifelse(Portfolio_lv2=="Qualifying revolving credit","Qualifying revolving",Portfolio_lv2))
data <- mutate(data,Portfolio_lv2=ifelse(is.na(Portfolio_lv2),"nothing",Portfolio_lv2))
data <- filter(data,Method != "SA", PD != 1, !is.na(PD),!is.na(EAD),!is.na(LGD),EAD>0,Portfolio_lv2 != "Total")
data <- mutate(data,RW=as.numeric(RW))

#setnames(get(paste0(i,".df")),tolower(names(get(paste0(i,".df")))))                                       
save(data,file=paste0("Data/Temp/Raw_data.Rda"))

##============================================================================##
## Basel formula                                                              ##
##============================================================================##
mapping_wholesale <- function(PD,LGD,M,alpha,portfolio){
  R  <- 0.12*(1-exp(-50*PD))/(1-exp(-50))+0.24*(1-(1-exp(-50*PD))/(1-exp(-50)))
  b  <- (0.11852-0.05478*log(PD))^2
  K  <- LGD*(pnorm(((1-R)^(-0.5))*qnorm(PD)+((R/(1-R))^(0.5))*qnorm(alpha))-PD)*((1-1.5*b)^-1*(1+(M-2.5)*b))
  RW <- K*12.5
  data <- data.frame(K,RW,PD,LGD,M,R,portfolio)
  return(data)
}

mapping_retail <- function(PD,LGD,R,alpha,portfolio){
  K  <- LGD*(pnorm(((1-R)^(-0.5))*qnorm(PD)+((R/(1-R))^(0.5))*qnorm(alpha))-PD)
  RW <- K*12.5
  data <- data.frame(K,RW,PD,LGD,R,portfolio)
  return(data)
}


mapping_other_retail <- function(PD,LGD,alpha,portfolio){
  R  <- 0.03*(1-exp(-35*PD))/(1-exp(-35))+0.16*(1-(1-exp(-35*PD))/(1-exp(-35)))
  K  <- LGD*(pnorm(((1-R)^(-0.5))*qnorm(PD)+((R/(1-R))^(0.5))*qnorm(alpha))-PD)
  RW <- K*12.5
  data <- data.frame(K,RW,PD,LGD,R,portfolio)
  return(data)
}

##============================================================================##
## Capital savings simulation                                                 ##
##============================================================================##

data   <- mutate(data,K_original=RW*EAD*0.08,PD_ead_w=PD*EAD,PD_lgd_w=PD*EAD*LGD,LGD_ead_w=LGD*EAD)

data$EAD_sum <- ave(data$EAD,data$Bank,data$year,data$Portfolio_lv1,data$Portfolio_lv2,FUN = sum) 
data$PD_ead_w_sum <- ave(data$PD_ead_w,data$Bank,data$year,data$Portfolio_lv1,data$Portfolio_lv2,FUN = sum) 
data$PD_lgd_w_sum <- ave(data$PD_lgd_w,data$Bank,data$year,data$Portfolio_lv1,data$Portfolio_lv2,FUN = sum) 
data$LGD_ead_w_sum <- ave(data$LGD_ead_w,data$Bank,data$year,data$Portfolio_lv1,data$Portfolio_lv2,FUN = sum) 

data$PD_ead_avg <- data$PD_ead_w_sum/data$EAD_sum
data$PD_lgd_avg <- data$PD_lgd_w_sum/data$LGD_ead_w_sum

data$LGD_ead_avg <- data$LGD_ead_w_sum/data$EAD_sum
data$LGD_pd_avg <- data$PD_lgd_w_sum/data$PD_ead_w_sum

for (i in 1:nrow(data)){
  if (data[i,"Portfolio_lv1"]!="Wholesale"){
    next
  }
  data[i,"K_calculated"] <- data[i,"EAD"]*mapping_wholesale(data[i,"PD"],data[i,"LGD"],2.5,0.999,"")["K"]
  data[i,"K_counterfactual_PD_lgd"] <- data[i,"EAD"]*mapping_wholesale(data[i,"PD_lgd_avg"],data[i,"LGD"],2.5,0.999,"")["K"]
  data[i,"K_counterfactual_PD_avg"] <- data[i,"EAD"]*mapping_wholesale(data[i,"PD_ead_avg"],data[i,"LGD"],2.5,0.999,"")["K"]
  data[i,"K_counterfactual_LGD_pd"] <- data[i,"EAD"]*mapping_wholesale(data[i,"PD"],data[i,"LGD_pd_avg"],2.5,0.999,"")["K"]
  data[i,"K_counterfactual_LGD_avg"] <- data[i,"EAD"]*mapping_wholesale(data[i,"PD"],data[i,"LGD_ead_avg"],2.5,0.999,"")["K"]
  data[i,"K_counterfactual_PD_avg_LGD"] <- data[i,"EAD"]*mapping_wholesale(data[i,"PD_lgd_avg"],data[i,"LGD_ead_avg"],2.5,0.999,"")["K"]
  
  }

for (i in 1:nrow(data)){
  if (is.na(data[i,"Portfolio_lv2"])){
    next
  }
  if (data[i,"Portfolio_lv2"]!="Real estate"){
    next
  }
  data[i,"K_calculated"] <- data[i,"EAD"]*mapping_retail(data[i,"PD"],data[i,"LGD"],0.15,0.999,"")["K"]
  data[i,"K_counterfactual_PD_lgd"] <- data[i,"EAD"]*mapping_retail(data[i,"PD_lgd_avg"],data[i,"LGD"],0.15,0.999,"")["K"]
  data[i,"K_counterfactual_PD_avg"] <- data[i,"EAD"]*mapping_retail(data[i,"PD_ead_avg"],data[i,"LGD"],0.15,0.999,"")["K"]
  data[i,"K_counterfactual_LGD_pd"] <- data[i,"EAD"]*mapping_retail(data[i,"PD"],data[i,"LGD_pd_avg"],0.15,0.999,"")["K"]
  data[i,"K_counterfactual_LGD_avg"] <- data[i,"EAD"]*mapping_retail(data[i,"PD"],data[i,"LGD_ead_avg"],0.15,0.999,"")["K"]
  data[i,"K_counterfactual_PD_avg_LGD"] <- data[i,"EAD"]*mapping_retail(data[i,"PD_lgd_avg"],data[i,"LGD_ead_avg"],0.15,0.999,"")["K"]
  
   }

for (i in 1:nrow(data)){
  if (is.na(data[i,"Portfolio_lv2"])){
    next
  }
  if (data[i,"Portfolio_lv2"]!="Qualifying revolving"){
    next
  }
  data[i,"K_calculated"] <- data[i,"EAD"]*mapping_retail(data[i,"PD"],data[i,"LGD"],0.04,0.999,"")["K"] 
  data[i,"K_counterfactual_PD_lgd"] <- data[i,"EAD"]*mapping_retail(data[i,"PD_lgd_avg"],data[i,"LGD"],0.04,0.999,"")["K"] 
  data[i,"K_counterfactual_PD_avg"] <- data[i,"EAD"]*mapping_retail(data[i,"PD_ead_avg"],data[i,"LGD"],0.04,0.999,"")["K"] 
  data[i,"K_counterfactual_LGD_pd"] <- data[i,"EAD"]*mapping_retail(data[i,"PD"],data[i,"LGD_pd_avg"],0.04,0.999,"")["K"] 
  data[i,"K_counterfactual_LGD_avg"] <- data[i,"EAD"]*mapping_retail(data[i,"PD"],data[i,"LGD_ead_avg"],0.04,0.999,"")["K"] 
  data[i,"K_counterfactual_PD_avg_LGD"] <- data[i,"EAD"]*mapping_retail(data[i,"PD_lgd_avg"],data[i,"LGD_ead_avg"],0.04,0.999,"")["K"] 
   }

for (i in 1:nrow(data)){
  if (is.na(data[i,"Portfolio_lv2"])){
    next
  }
  if (data[i,"Portfolio_lv2"]!="Other retail"){
    next
  }
  data[i,"K_calculated"] <- data[i,"EAD"]*mapping_other_retail(data[i,"PD"],data[i,"LGD"],0.999,"")["K"] 
  data[i,"K_counterfactual_PD_lgd"] <- data[i,"EAD"]*mapping_other_retail(data[i,"PD_lgd_avg"],data[i,"LGD"],0.999,"")["K"] 
  data[i,"K_counterfactual_PD_avg"] <- data[i,"EAD"]*mapping_other_retail(data[i,"PD_ead_avg"],data[i,"LGD"],0.999,"")["K"] 
  data[i,"K_counterfactual_LGD_pd"] <- data[i,"EAD"]*mapping_other_retail(data[i,"PD"],data[i,"LGD_pd_avg"],0.999,"")["K"] 
  data[i,"K_counterfactual_LGD_avg"] <- data[i,"EAD"]*mapping_other_retail(data[i,"PD"],data[i,"LGD_ead_avg"],0.999,"")["K"] 
  data[i,"K_counterfactual_PD_avg_LGD"] <- data[i,"EAD"]*mapping_other_retail(data[i,"PD_lgd_avg"],data[i,"LGD_ead_avg"],0.999,"")["K"] 
  
      }


data.reduced <- data %>% group_by(Bank,year) %>%
  summarise_if(is.numeric,funs(sum),na.rm=TRUE)
data.reduced <- mutate(data.reduced,error = (K_calculated - K_original)/K_original,
                                    savings_PD_lgd = (K_calculated-K_counterfactual_PD_lgd)/K_counterfactual_PD_lgd,
                       savings_PD_avg = (K_calculated-K_counterfactual_PD_avg)/K_counterfactual_PD_avg,
                       savings_LGD_pd = (K_calculated-K_counterfactual_LGD_pd)/K_counterfactual_LGD_pd,
                       savings_LGD_avg = (K_calculated-K_counterfactual_LGD_avg)/K_counterfactual_LGD_avg,
                       savings_LGD_constant = (K_calculated-K_counterfactual_PD_avg_LGD)/K_counterfactual_PD_avg_LGD,
                       savings_alternative = (K_counterfactual_LGD_avg-K_counterfactual_PD_avg_LGD)/K_counterfactual_PD_avg_LGD)


sample.average <- data.reduced %>% ungroup() %>%
  summarise_if(is.numeric,funs(mean),na.rm=TRUE)
save(data,file=paste0("Data/Temp/Raw_data_2.Rda"))
##============================================================================##
## Gini coefficient                                                           ##
##============================================================================##

data.gini <- data %>% group_by(Bank,year,Portfolio_lv1,Portfolio_lv2) %>%
  summarize(gini = DescTools::Gini(PD,EAD),
            K_calculated=sum(K_calculated),
            K_counterfactual_PD_lgd=sum(K_counterfactual_PD_lgd))
data.gini <- mutate(data.gini,savings = (K_counterfactual_PD_lgd-K_calculated)/K_counterfactual_PD_lgd)

cor(data.gini$savings,data.gini$gini)
lc.wholesale <- data %>% filter(Portfolio_lv1 == "Wholesale") 
lc.wholesale <- Lc(lc.wholesale$PD,lc.wholesale$EAD*lc.wholesale$LGD)

lc.retail <- data %>% filter(Portfolio_lv1 == "Retail") 
lc.retail <- Lc(lc.retail$PD,lc.retail$EAD*lc.retail$LGD)

plot(lc.wholesale,
     col="black",
     type="l",      # !is not working
     lty=1,
     lwd=3,
     main="Lorenz Curve by Portfolio"     
)

lines(lc.retail, lty=2, lwd=3)


legend("topleft",
       c("Wholesale", "Retail"),
       lty=c(1,2,3),
       lwd=3)


sample.average.2 <- data.gini %>% ungroup() %>%
  summarise_if(is.numeric,funs(mean),na.rm=TRUE)




##============================================================================##
## LGD-PD correlation                                                         ##
##============================================================================##
rm(list = ls()) 
data.path  <- "../../../Data/Financial/Exposure_data_collection/Data/Second_list"
data.names <-  list.files(path = data.path,pattern="\\.xlsx",full.names = TRUE)
data.list  <- data.names %>%
  lapply(read_xlsx,sheet = 1,  skip=1)  


data <-  rbindlist(data.list, use.names=TRUE,fill = TRUE) %>%
  select("Bank","date","Method","Portfolio- level 1","Portfolio- level 2","Portfolio- level 3",
         "CPCR","SL","Location of Borrower",
         "Exposure at default - mln (a)","Risk-weight (c)= (b)/(a)",
         "LGD (exposure-weighted average)",
         "PD (exposure-weighted average)")

data <- plyr::rename(data,c("Portfolio- level 1"="Portfolio_lv1","Portfolio- level 2"="Portfolio",
                            "Portfolio- level 3"="Portfolio_lv3",
                            "Location of Borrower"="Location",
                            "Exposure at default - mln (a)"="EAD",
                            "Risk-weight (c)= (b)/(a)"="RW",
                            "LGD (exposure-weighted average)"="LGD",
                            "PD (exposure-weighted average)"="PD"))
data <- filter(data,Location=="Aggregate"|Location=="Aggregated")

#data$year <- sapply(EBA.df$item,toString) 
data$month <- sapply(data$date,str_sub,start= 4,end=5)
data$year <- sapply(data$date,str_sub,start= -4)
data <- mutate(data,year=as.numeric(year),month=as.numeric(month)) 
data <- mutate(data,year=ifelse(month<7,year-1,year))
data <- mutate(data,Portfolio=ifelse(Portfolio=="Real Estate","Real estate",Portfolio))
data <- mutate(data,Portfolio=ifelse(Portfolio=="Qualifying revolving credit","Qualifying revolving",Portfolio))
data <- filter(data,Method != "SA")
save(data,file=paste0("Data/Temp/PD_LGD_correlation.Rda"))
reg.hdfe1 <- felm(LGD~ PD | Portfolio+Bank|0|0,
                  data = subset(data,subset = PD<1))
robust.hdfe1 <- summary(reg.hdfe1,robust=TRUE)$coefficients[,2]

reg.hdfe2 <- felm(LGD~ PD | Bank|0|0,
                  data = subset(data,subset = PD<1 & Portfolio == "Real estate"))
robust.hdfe2 <- summary(reg.hdfe2,robust=TRUE)$coefficients[,2]

reg.hdfe3 <- felm(LGD~ PD | Bank|0|0,
                  data = subset(data,subset = PD<1 & Portfolio == "Qualifying revolving" ))
robust.hdfe3 <- summary(reg.hdfe3,robust=TRUE)$coefficients[,2]

reg.hdfe4 <- felm(LGD~ PD |  Bank|0|0,
                  data = subset(data,subset = PD<1 & Portfolio == "Other retail"))
robust.hdfe4 <- summary(reg.hdfe4,robust=TRUE)$coefficients[,2]

reg.hdfe5 <- felm(LGD~ PD |  Bank|0|0,
                  data = subset(data,subset = PD<1 & Portfolio == "Corporate"))
robust.hdfe5 <- summary(reg.hdfe5,robust=TRUE)$coefficients[,2]

reg.hdfe6 <- felm(LGD~ PD |  Bank|0|0,
                  data = subset(data,subset = PD<1 & Portfolio == "Banks"))
robust.hdfe6 <- summary(reg.hdfe6,robust=TRUE)$coefficients[,2]

reg.hdfe7 <- felm(LGD~ PD |  Bank|0|0,
                  data = subset(data,subset = PD<1 & Portfolio == "Sovereign"))
robust.hdfe7 <- summary(reg.hdfe7,robust=TRUE)$coefficients[,2]

stargazer(reg.hdfe1,reg.hdfe2,reg.hdfe3,reg.hdfe4,reg.hdfe5,reg.hdfe6,reg.hdfe7,
          header = FALSE, type="latex", align = TRUE, keep.stat = c("n","rsq"), font.size = "tiny", 
          label = "",se = list(robust.hdfe1,robust.hdfe2,robust.hdfe3,robust.hdfe4,robust.hdfe5,robust.hdfe6,robust.hdfe7),
          style = "qje", column.labels   = c("", "Real estate","Credit cards", "Other retail",
                                             "Corporates","Banks","Sovereign"),
          dep.var.labels = "LGD", column.sep.width = "2pt",
          add.lines = list(c("Portfolio FE", "Yes", "-","-","-","-","-","-"),
                           c("Bank FE","Yes", "Yes","Yes","Yes","Yes","Yes","Yes")))

p <- ggplot(data = subset(data,subset = PD<1), 
            mapping = aes(x=PD,y=LGD))
p + geom_point()+
  geom_smooth(method = "lm") 
##============================================================================##
## EBA-tranparency exercises                                                  ##
##============================================================================##

data.path <- "../../../data/Financial/EBA-transparency_exercise/"             ## Files' location.
data.name <- "/tr_cre.csv"                                                    ## Files' name.

for(i in 2015:2017) {
  assign(paste0("credit_",i), read.csv(paste0(data.path,i,data.name)))        ## I set variables' name to 
  setnames(get(paste0("credit_", i)),                                         ## lower-case so we have no
           tolower(names(get(paste0("credit_",i)))))                          ## problem when merging files.                
}                                                                                 
EBA.df <- rbindlist(lapply(ls(pattern="credit_*"), get), fill = TRUE) 

EBA.ID.df <- readWorksheetFromFile(paste0(data.path,"2017/TR_Metadata.xlsx"), ## This file contain the match 
                                   1,startRow=2)                              ## between different identifiers 
                                                                              ## used by EBA in different exercises.
setnames(EBA.ID.df, tolower(names(EBA.ID.df)))                                       

EBA.ID.df <- tibble::rowid_to_column(EBA.ID.df, "id")                         ## I create a unique identifier. 

EBA.ID.df[,c("finrep","fin_year_end","periods","tr_16","tr_15")] <- NULL      ## A bit of cleaning before turning
                                                                              ## the dataset to long format.
EBA.ID.df <- melt(EBA.ID.df, 
                  id.vars = c("id","country","desc_country","ssm","name"),
                  variable.name = "index", 
                  value.name = "lei_code")
EBA.ID.df[,c("index")] <- NULL                                                ## We need to remove the index                                            
EBA.ID.df <- unique(EBA.ID.df)                                                ## variable, NA and, non-unique 
EBA.ID.df <- na.omit(EBA.ID.df)                                               ## observations to merge correctly.

EBA.df <- merge(EBA.ID.df,EBA.df,by="lei_code",all = TRUE)                       
EBA.df[,c("label","footnote","status","perf_status")] <- NULL   
rm(list = ls(pattern="credit_*"))                                                 

EBA.country.df <- readWorksheetFromFile(paste0(data.path,                     ## This file contain the match 
                                               "2017/TR_Metadata.xlsx"),      ## between country number identifier
                                   4,startRow=2)                              ## used by EBA and country name.
                                                                              
setnames(EBA.country.df, tolower(names(EBA.country.df)))                      ## Variables are set to lower case
names(EBA.country.df)[names(EBA.country.df)=="country"] <-  "country.y"       ## and names are changed to merge.
names(EBA.country.df)[names(EBA.country.df)=="label"] <-  "borrower.country"
EBA.df <- merge(EBA.country.df,EBA.df,by="country.y",all = TRUE) 

variables <- c("0522","0532","0512","0542")                                   ## Identifier numbers for variables of 
EBA.df$item <- sapply(EBA.df$item,toString)                                   ## interest (check lines 94-97) are
EBA.df$item_global <- sapply(EBA.df$item,str_sub,start= -4)                   ## taken from file TR_Data_Dictionary.
EBA.df$item <- NULL                                                            

EBA.df <- subset(EBA.df,                                                      ## I keep only corporate lending 
                      subset = EBA.df$item_global %in% variables &            ## (item 303).
                        EBA.df$exposure %in% 303)

EBA.df <- dcast(EBA.df, lei_code + ... ~ item_global, value.var="amount")     ## Split Item variables in exposures 
EBA.df <- plyr::rename(EBA.df,c("0522"="Exposure",                            ## variables and rename them.
                                "0532"="Risk_exposure",
                                "0512"="Exposure_default",
                                "0542"="Risk_exposure_default"))

save(EBA.df,file="Data/EBA.Rda")

