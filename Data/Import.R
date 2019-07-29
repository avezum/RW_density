##-----------------------------------------------------------------------------##
## Project: PD distribution and capital savings                                ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Description: this file opens, merges and saves in .Rda format               ##
##-----------------------------------------------------------------------------##
  
##=============================================================================##
## Code setup                                                                  ##
##=============================================================================##

rm(list = ls()) 
library(tidyverse)    ## Data manipulation, pipe operator                                                             
library(readxl)       ## Command to open xlsx files                                                     
library(data.table)   ## Command to bind lists
library(reshape2)     ## Command to transform data from wide to long
library(stringr)

# Load auxiliar data frame: list of parent banks with all BvDid's and IRB adoption year
IRB.indicator <- read_xlsx("Data/Raw/Pillar3/SA_banks/SA_banks.xlsx",sheet = 1) 

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

# Add short names to data frame
pillar3.data <- IRB.indicator %>%
  select(name, bvdid_new) %>% rename(bvdid = bvdid_new) %>%
  inner_join(pillar3.data)

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

for(i in 1:length(data.list)) {
  data.list[[i]] <- data.list[[i]] %>%
    # Remove row index, set variables names to lower case, rename id variable, and keep only selected sample
    select(-contains("_"))  %>%
    rename_all(tolower)%>%
    rename_at(vars(contains("bvd")), 
              funs(str_replace(.,"bvd .*", "bvdid")))%>%
    # Standardize variables names to reshape dataset from wide to long
    # The first two lines  code remove several caracteres, the second add a dot before the year indicator
    rename_at(vars(contains("last")), 
              funs(str_replace(.,"last .*", "10")))%>%
  rename_all(funs(str_replace_all(.,"[[:space:]]|[\\(*\\),]|\\[|\\]|\\%|\\/|\\.|\\=|\\-|year","")))%>%
  rename_all(funs(str_replace_all(.,"([\\w])([0-9]+)$", "\\1\\.\\2")))

  # Reshape datasets to long format
  data.list[[i]] <- reshape(data.list[[i]],
                            varying   = c(grep("10", names(data.list[[i]]))[1]:ncol(data.list[[i]])),
                            direction = 'long', 
                            timevar   = 'year')
  names(data.list[[i]]) <- names(data.list[[1]])
  }

# Bind datasets from different disks  
bankscope <-  rbindlist(data.list, use.names=TRUE,fill = TRUE)

# Standardize bvdid and add IRB adoption variable
IRB.indicator.long <- IRB.indicator %>%
  rename_if(is.numeric, function(x) paste("IRB", x, sep = ".")) %>%
  reshape(varying   = c(grep("[0-9]", names(.))),
          direction = 'long', 
          timevar   ='year')%>%
  select(name, Country, bvdid_new, bvdid_old, year, IRB)

bankscope <- bankscope %>% 
  mutate(month = as.numeric(sapply(closingdate,str_sub,start= 6,end=7)),
         year  = as.numeric(sapply(closingdate,str_sub,start= 1,end=4)),
         year  = ifelse(is.na(month),year,ifelse(month<6,year-1,year))) %>%
  full_join(IRB.indicator.long, by = c("bvdid" = "bvdid_old", "year")) %>%
  full_join(IRB.indicator.long, by = c("bvdid" = "bvdid_new", "year")) %>%
    mutate(bvdid = ifelse(is.na(bvdid_old), 
                          ifelse(is.na(bvdid_new), bvdid, bvdid_new), 
                          bvdid)) %>%
  # Keep unique bank-year observations
  filter(conscode %in% c("C1","C2","U1")) %>%
  distinct(bvdid,year, .keep_all = TRUE) %>%
  mutate(IRB = ifelse(is.na(IRB.x),IRB.y,IRB.x)) %>%
  filter(bvdid %in% IRB.indicator$bvdid_new) %>%
  # Remove auxiliar variables
  select(-contains("_")) %>% select(-contains(".")) %>%
  select(-c("companyname", "countryisocode","lastavail", "id")) 
  # Turn numeric variables to numeric format
  bankscope <- bankscope %>% 
  mutate_at(c(grep("usd", names(bankscope))[1]:ncol(bankscope)),as.numeric)

# Save data frame
save(bankscope,file=paste0("Data/Temp/BankScope.Rda"))


