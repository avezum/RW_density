##-----------------------------------------------------------------------------##
## Project: PD distribution and capital savings                                ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Description: this file opens, merges and saves in .Rda format               ##
##-----------------------------------------------------------------------------##
  
##=============================================================================##
## Code setup                                                                  ##
##=============================================================================##


library(tidyverse)    ## Data manipulation, pipe operator                                                             
library(readxl)       ## Command to open xlsx files                                                     
library(data.table)   ## Command to bind lists
library(stringr)      ## Command to clean variables names in Orbis
library(zoo)          ## Command to replace missing values by other group's mean value 

rm(list = ls()) 

##============================================================================##
## Auxiliar data                                                              ##
##============================================================================##

# IRB adoption year
IRB.indicator <- read_xlsx("Data/Raw/Pillar3/Auxiliar/Auxiliar.xlsx",sheet = 1)  %>%
  pivot_longer(cols      = where(is.numeric),
               names_to  = "year",
               values_to = "IRB")%>%
  select(name, Country, bvdid_new, bvdid_old, year, IRB)%>%
  mutate(year= as.numeric(year))

# Basel II introduction
basel.indicator <- read_xlsx("Data/Raw/Pillar3/Auxiliar/Auxiliar.xlsx",sheet = 2) %>%
  select(Country, basel = year)

# EBA capital exercise
EBA.indicator <- read_xlsx("Data/Raw/Pillar3/Auxiliar/Auxiliar.xlsx",sheet = 3) %>%
  select(bvdid, EBAbank, EBAcountry, Shortfall.mn, Shortfall.RW)

# Macro variables
country.code <- read.csv("Data/Raw/WDI/WDICountry.csv") %>%
  rename_all(tolower)%>%
  select(country.code = ï..country.code,
         Country = x2.alpha.code,
         currency.unit)%>%
  mutate(currency.unit = ifelse(country.code=="EMU", "Euro", as.character(currency.unit)))

WDI <- read.csv("Data/Raw/WDI/WDIData.csv") %>%
  filter(Indicator.Code %in% c("NY.GDP.DEFL.ZS","NY.GDP.PCAP.KD","NY.GDP.PCAP.KN", "PA.NUS.FCRF", "PX.REX.REER",
                               "FS.AST.DOMS.GD.ZS","FS.AST.PRVT.GD.ZS","FD.AST.PRVT.GD.ZS", "FP.CPI.TOTL")) %>%
  rename_all(funs(str_replace_all(.,"([\\w])([0-9]+)$", "\\1\\.\\2"))) %>%
  reshape(varying   = c(grep("X.", names(.))),
                      direction = 'long', 
                      timevar   = 'year')%>%
  select(Country.Code,Indicator.Code,X,year) %>%
  reshape(direction = 'wide',
          idvar     = c('Country.Code','year'), 
          timevar   = 'Indicator.Code') %>%
  rename_all(tolower) %>%
  inner_join(country.code)%>%
  select(deflator        = x.ny.gdp.defl.zs,
         cpi             = x.fp.cpi.totl,
         gdppc.us        = x.ny.gdp.pcap.kd,
         gdppc           = x.ny.gdp.pcap.kn,
         er              = x.pa.nus.fcrf,
         reer            = x.px.rex.reer,
         credit_gdp      = x.fs.ast.doms.gd.zs,
         priv_credit_gdp = x.fs.ast.prvt.gd.zs,
         bank_credit_gdp = x.fd.ast.prvt.gd.zs,
         year,
         Country,
         currency.unit)%>%
  mutate(credit_gdp      = credit_gdp/100,
         priv_credit_gdp = priv_credit_gdp/100,
         bank_credit_gdp = bank_credit_gdp/100,
         cpi             = cpi/100,
         group.var       = ifelse(is.na(er)| Country == "XC", paste(currency.unit, year, sep = ""), NA),
         er              = na.aggregate(er, by = group.var )) %>%
  select(-group.var, -currency.unit)
         


# Bank Regulation and Supervision
country.code <- read.csv("Data/Raw/WDI/WDICountry.csv") %>%
  rename_all(tolower)%>%
  select(country.name = short.name,
         Country      = x2.alpha.code)

# BRSS <- read_xls("Data/Raw/BRSS/BCL_Sup_Reg_Data_13JAN2013.xls",sheet = 4) %>%
#   select(-c("Number","Question response"),-contains("value"))%>%
#   pivot_longer(-c('Name','Survey'), names_to = "Country", values_to = "value")%>%
#   pivot_wider(names_from = Name, values_from = value)%>%
#   filter(!is.na(Survey))%>%
#   rename_at(vars(matches("[|]|*")),funs(str_replace_all(., "\\(|\\)|\\[|\\]|\\*|[0-9]", "")))%>%
#   rename_all(tolower) %>%
#   mutate_at(vars(-country),as.numeric)%>%
#   select_if(~!all(is.na(.)))%>%
#   select(survey, country.name = country, ovr_cap_string, init_cap_strin, cap_reg, sup_power)%>%
#   inner_join(country.code)
load("Data/Raw/BRSS/Output/BRSS.Rdata")
  
  save(list = c("IRB.indicator","basel.indicator","EBA.indicator","WDI","BRSS"),
       file=paste0("Data/Temp/AuxiliarData.Rda"))

##============================================================================##
## Pillar-III reports                                                         ##
##============================================================================##

# Import collected datasets from xlsx files and merge all in data frame data.pillar3
data.path  <- "Data/Raw/Pillar3"
data.names <-  list.files(path = data.path,pattern="\\.xlsx",full.names = TRUE)
data.list  <- data.names %>%
  lapply(read_xlsx,sheet = 1,  skip=1)   
pillar3.data <-  rbindlist(data.list, use.names=TRUE,fill = TRUE) %>%
  select(1:25)

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

# Add short names to data frame and currency and inflation conversion
pillar3.data <- IRB.indicator %>%
  select(name, bvdid = bvdid_new) %>% 
  distinct(bvdid, .keep_all = TRUE) %>%
  inner_join(pillar3.data) %>%
  # Add macro variables
  left_join(select(WDI, Country, year, er), by = c("Country", "year")) %>%
  # Convert and deflate using REER
  mutate(EAD = EAD/er,
         RWA = RWA/er)

# Save data frame
save(pillar3.data,file=paste0("Data/Temp/Pillar3Data.Rda"))

##============================================================================##
## Orbis Bank Focus (BankScope)                                               ##
##============================================================================##

# Import Orbis dataset from excel files 
data.path  <- "Data/Raw/BankScope"
data.names <-  list.files(path = data.path,pattern=".*(201.)\\.xlsx",full.names = TRUE)
data.list  <- data.names %>%
  lapply(read_xlsx,sheet = 1) 

#------------------------------------------------------------------------------#
# Reshape data from wide to long                                               #
#------------------------------------------------------------------------------#

for(i in 1:length(data.list)) {
  data.list[[i]] <- data.list[[i]] %>%
    # Remove row index, set variables names to lower case, rename id variable, and keep only selected sample
    rename_all(tolower)%>%
    rename_at(vars(contains("bvd")), 
              funs(str_replace(.,"bvd .*", "bvdid")))%>%
    mutate(disk = paste(i)) %>% select(disk, everything())%>%
    # Standardize variables names to reshape dataset from wide to long
    rename_all(funs(str_replace_all(.,"[:space:]|[:punct:]|year|=","")))%>%
    rename_at(vars(contains("lastavailyr")), 
              funs(str_replace(.,"last.*", "10")))%>%
    rename_all(funs(str_replace_all(.,"([\\w])([0-9]+)$", "\\1\\.\\2")))%>%
    # Turn numeric variables to numeric format
    mutate_at(c(grep("usd", names(.))[1]:ncol(.)),as.numeric) %>%
    # Reshape datasets to long format
    pivot_longer(cols = c(grep("\\.\\d", names(.))),
                 names_to = c(".value", "year"),
                 names_pattern = "(.+)\\.(.+)")
    # Standardize variable names across datasets
    names(data.list[[i]]) <- names(data.list[[1]])
}

# Bind datasets from different disks  
bankscope <-  rbindlist(data.list, use.names=TRUE,fill = TRUE)

#------------------------------------------------------------------------------#
# First layer of data cleaning                                                 #
#------------------------------------------------------------------------------#

bankscope <- bankscope %>%
  # Create and correct year variable
  mutate(month = as.numeric(sapply(closingdate,str_sub,start= 6,end=7)),
         year  = as.numeric(sapply(closingdate,str_sub,start= 1,end=4)),
         year  = ifelse(is.na(month),year,ifelse(month<6,year-1,year))) %>%
  # Harmonize bvdid (for some banks and countries it has changed from one disk to another)
  full_join(IRB.indicator, by = c("bvdid" = "bvdid_old", "year")) %>%
  full_join(IRB.indicator, by = c("bvdid" = "bvdid_new", "year")) %>%
  mutate(bvdid = ifelse(is.na(bvdid_old), ifelse(is.na(bvdid_new), bvdid, bvdid_new), bvdid)) %>%
  # Keep unique bank-year observations from sample, and remove strange observations from 1969 (?!?)
  filter(bvdid %in% IRB.indicator$bvdid_new, year != 1969, conscode %in% c("C1","C2","U1")) %>%
  group_by(bvdid,year) %>% arrange(desc(disk), .by_group = TRUE) %>%
  distinct(bvdid,year, .keep_all = TRUE) %>% ungroup() %>%
  # Fix variables from the IRB indicator data frame
  mutate(IRB     = ifelse(is.na(IRB.x),IRB.y,IRB.x),
         name    = ifelse(is.na(name.x),name.y,name.x),
         Country = ifelse(is.na(Country.x),Country.y,Country.x)) %>%
  # Reorder variables and remove auxiliar variables
  select(name,  Country, IRB, everything(), -contains("_"), -contains("."),
         -c("companyname", "countryisocode","lastavail", "guoname", "conscode",
            "status", "listeddelistedunlisted", "delisteddate", "guobvdid", "guocountryisocode",
            "guotype", "closingdate", "month", "1", "IRB"))

# Save data frame
save(bankscope,file=paste0("Data/Temp/BankScope.Rda"))


