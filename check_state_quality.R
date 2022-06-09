## ---------------------------
## Filename: check_state_quality.r
## Project: mandated_reporters
## Description: Assess quality of NCANDS drug abuse variables
## by state and year, output index of state-years with viable data
##
## Author: Frank Edwards
##
## Date Created: 4/30/21
##
## Date last edited: 4/30/21
##
## Email: frank.edwards@rutgers.edu
##
## ---------------------------
##
## Notes: Depends on NCANDS child file FY 2010-2019
##   
##
## ---------------------------
## load packages

library(data.table)
library(tidyverse)
library(lubridate)
library(mice)

## set options 

options(scipen = 6) 
theme_set(theme_bw())

## ---------------------------
ncands_path<-"~/Projects/ndacan_data/ncands/"
ncands_files<-paste(ncands_path,
                    list.files(ncands_path),
                    sep = "")

### set up 10-19 for 10 year panel on NCANDS
ncands_files<-paste(ncands_path, list.files(ncands_path), sep = "")[9:18]

ncands<-lapply(ncands_files, fread)

for(i in 1:length(ncands_files)){
  ncands[[i]]<-ncands[[i]]%>%
    rename_all(tolower) %>%
    select(staterr,
           rptsrc,
           cddrug,
           fcdrug,
           cdalc,
           fcalc,
           chage, 
           subyr,
           chage) %>% 
    filter(chage == 0 | chage == 77) %>% 
    filter(rptsrc == 2) %>% 
    mutate(ipse = 
             case_when(
               cddrug == 1 ~ T,
               fcdrug == 1 ~ T,
               fcalc == 1 ~ T,
               cdalc == 1 ~ T,
               cddrug == 2 | fcdrug == 2 | fcalc == 2 | cdalc == 2 ~ F
             )) %>% 
    select(staterr, ipse, subyr)
  
}

ncands1<-bind_rows(ncands)

### --------------------------------
### aggregate cddrug, fcdrug
### drop states where prop_na>0.05
### drop states where count of drugs cases is 0
### for sensitivity later, can increase to 0.01 prop
### if child or caretaker drugs == 1, drugs == 1
### regardless of missing, same for 2

### after some looks at the state-level time series
### clear that there's a disjoint in AZ
### IL has unbelievably low N

### --------------------------------
#### DESCRIPTION AND VISUALS
#### check for proportion missing on all state years
prop_missing<-ncands1 %>% 
  group_by(staterr, subyr) %>% 
  summarise(prop_ipse_missing = sum(is.na(ipse))/n())

#### visualize
ggplot(prop_missing,
       aes(y = prop_ipse_missing,
           x = subyr)) + 
  geom_line() + 
  facet_wrap(~staterr)

### now check whether there are non-zero values
any_ones<-ncands1 %>% 
  group_by(staterr, subyr) %>% 
  summarise(ipse_one = sum(ipse, na.rm=T)/n(),
            ipse_tot = sum(ipse, na.rm=T)) 


ncands_quality<-prop_missing %>% 
  left_join(any_ones) %>% 
  filter(!staterr%in%c("XX", "PR", "PA", "FL", "IA", "NY", "ND"))

### remove states identified with unreliable data in child maltreatment report 2019
## PA, FL, IA, NY, ND, 

#### SCREEN CASES FOR VALID STATES - ADD A QUALITY DROP FLAG TO DATA
#### 
### use the 20 percent // 0.01 percent thresholds
### use 10 // 0.05 in sensitivity (more conservative)
ncands_quality<-ncands_quality %>% 
  mutate(flag_1 = prop_ipse_missing<=0.2 & ipse_one>=0.01)

write_csv(ncands_quality, file = "./data/ncands_drugs_quality_check.csv")