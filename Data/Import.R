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

# Load auxiliar data frame: list of parent banks
guo.list <- read_xlsx("../../../Data/Financial/Exposure_data_collection/Auxiliar/List.xlsx",sheet = 1) %>%
  select(name,bvdid)

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
pillar3.data <- inner_join(pillar3.data, guo.list)

# Save data frame
save(pillar3.data,file=paste0("Data/Temp/Pillar3Data.Rda"))

##============================================================================##
## Orbis Bank Focus (BankScope)                                               ##
##============================================================================##

# Import Orbis dataset from txt file 
data.path  <- "Data/Raw/BankScope"
data.names <-  list.files(path = data.path,pattern=".txt",full.names = TRUE)
data.list  <- data.names %>% lapply(read.delim) 

# Create variable year and adjust by month (change to previous year if June of before),
# keep consolidate information

for(i in 1:length(data.list)) {
data.list[[i]] <- data.list[[i]] %>%
  mutate(month=as.numeric(sapply(Closing.date,str_sub,start= 5,end=6)),
         year=as.numeric(sapply(Closing.date,str_sub,start= 1,end=4)),
         sample.version = i) %>%
  mutate(year=ifelse(is.na(month),year,
                     ifelse(month<7,year-1,year))) %>% 
  rename(bvdid=colnames(data.list[[i]])[1]) %>%
  filter(Consolidation.code %in% c("C1","C2"))
}

# Bind old (2007-2016) and new dataset (2008-2018) 
bankscope <-  rbindlist(data.list, use.names=TRUE,fill = TRUE) %>%
  distinct(bvdid,year, .keep_all = TRUE) 

# Save data frame
save(bankscope,file=paste0("Data/Temp/BankScope.Rda"))



bankscope <- read_xlsx("Data/Raw/BankScope/BankFocus_Export.xlsx",sheet = 2)
bankscope <- select(bankscope,-contains("_"))
threshold <- which(colnames(bankscope)%in%grep("2018", names(bankscope), value=TRUE))[1]
names(bankscope) <- gsub("[[:space:]]", "", names(bankscope))
names(bankscope) <- gsub("[\\(*\\),]", "",names(bankscope))
names(bankscope) <- gsub("[\\%,]", "",names(bankscope))
names(bankscope) <- gsub("[\\/,]", "",names(bankscope))
names(bankscope) <- gsub("[\\.,]", "",names(bankscope))
names(bankscope) <- gsub("(*)([0-9]+)", "\\1\\.\\2", names(bankscope))
names(bankscope) <- gsub("(\\.)(1)", "\\2", names(bankscope))

test <-reshape(bankscope, 
               varying   = c(threshold:ncol(bankscope)),
               direction = 'long', 
               timevar   ='year')





