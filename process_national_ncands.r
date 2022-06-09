## ---------------------------
## Filename: process_national_ncands.r
## Project: med_ipse_surveillance
## Description: make national counts
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


rm(list=ls())
gc()
library(tidyverse)
library(data.table)
library(mice)
library(lubridate)

setwd("~/Projects/ndacan_data/ncands/")
### bring in 09-19, 09 only for deduplicating infants in 10
files<-list.files()[8:18]

ncands<-list()
for(i in 1:length(files)){
  ncands[[i]]<-fread(files[i])
  ncands[[i]]<-ncands[[i]] %>% 
    rename_all(tolower) %>% 
    filter(chage==0 | chage==77) %>% 
    select(subyr, staterr, chid, rptsrc, chmal1, chmal2, chmal3, chmal4)
}

ncands<-rbindlist(ncands) 

ncands<-ncands%>% 
  filter(!staterr%in%c("PA", "PR", "XX")) %>% 
  mutate(st_id = paste(staterr, chid, sep = "")) %>% 
  filter(!duplicated(st_id)) 

### make binaries for maltype
### neglect = 2 | 3
ncands<-ncands %>% 
  mutate(neglect = chmal1 == 2 | chmal2 == 2 | chmal3 == 2 | chmal4 == 2 |
           chmal1 == 3 | chmal2 == 3 | chmal3 == 3 | chmal4 == 3 ,
         phy_abuse = chmal1 == 1 | chmal2 == 1 | chmal3 == 1 | chmal4 == 1,
         sex_abuse = chmal1 == 4 | chmal2 == 4 | chmal3 == 4 | chmal4 == 4,
         other = chmal1 >4 | chmal2 >4 | chmal3 >4 | chmal4 >4 ) 
### convert maltype binaries to F instead of NA
ncands[is.na(ncands)]<-F
### convert reporter to medical, ! medical
ncands<-ncands %>% 
  mutate(medical_rptsrc = rptsrc==2) %>% 
  select(st_id, subyr, staterr, medical_rptsrc,
         neglect, phy_abuse, sex_abuse, other)
#### ncands is a list of all uniquely ID'd unborn and infants 2010-2019

### pull in pop data
pop <- read_fwf("~/Projects/data/us.1990_2019.singleages.adjusted.txt",
                fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                           c("year", "state", "st_fips",
                             "cnty_fips", "reg", "race",
                             "hisp", "sex", "age", "pop"))) %>% 
  filter(year>2009, age == "00")%>%
  mutate(pop = as.integer(pop))%>%
  group_by(year, state) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() 

### create counts, join pop
ncands_pop<-ncands %>% 
  rename(state = staterr,
         year = subyr) %>% 
  group_by(state, year, medical_rptsrc) %>% 
  summarize(phy_abuse = sum(phy_abuse),
            neglect = sum(neglect),
            sex_abuse = sum(sex_abuse),
            other = sum(other)) %>% 
  left_join(pop) %>% 
  group_by(year, medical_rptsrc) %>% 
  summarize_at(vars(phy_abuse:pop), sum) 

### then compute rate per 1000 for med, percent of total reports
ncands_med<-ncands_pop %>% 
  group_by(medical_rptsrc) %>% 
  pivot_longer(cols = phy_abuse:other,
               names_to = "type",
               values_to = "cases") %>% 
  mutate(rate = cases / pop * 1e3)

write_csv(ncands_med, "~/Projects/med_ipse_surveillance/data/ncands_natl_med.csv")


