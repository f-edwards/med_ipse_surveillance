# read_ncands.r------------------------------------------------------------
# project: med_ipse_surveillance ------------------------------------------
# Author: Frank Edwards
# Email:  frank.edwards@rutgers.edu
#
# read, process, and impute NCANDS child file
#
# log: pre-review cleanup (6/8/22)
#

# load packages -----------------------------------------------------------

library(data.table)
library(tidyverse)
library(lubridate)
library(mice)

set.seed(57)


# read first investigation id index file from ndacan_processing -----------

investigation<-fread("~/Projects/ndacan_processing/data/ncands_first_report_index.csv") 

index_first_investigation<-index_first_investigation %>% 
  mutate(first = T)


# prepare list of child files ---------------------------------------------

#loop over each year of ncands 2010 - 2019
files<-c("~/Projects/ndacan_data/ncands/CF2010v5.tab",
         "~/Projects/ndacan_data/ncands/CF2011v5.tab",
         "~/Projects/ndacan_data/ncands/CF2012v4.tab",
         "~/Projects/ndacan_data/ncands/CF2013v3.tab",
         "~/Projects/ndacan_data/ncands/CF2014v3.tab",
         "~/Projects/ndacan_data/ncands/CF2015v4.tab",
         "~/Projects/ndacan_data/ncands/CF2016v3.tab",
         "~/Projects/ndacan_data/ncands/CF2017v2.tab",
         "~/Projects/ndacan_data/ncands/CF2018v1.tab",
         "~/Projects/ndacan_data/ncands/CF2019v1.tab")

# read and process SEER ---------------------------------------------------

### load pop data for imputation predictors
### read census
pop <- read_fwf("~/Projects/data/us.1990_2019.singleages.adjusted.txt",
                fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                           c("year", "state", "st_fips",
                             "cnty_fips", "reg", "race",
                             "hisp", "sex", "age", "pop"))) %>% 
  filter(year>2009) 

pop<-pop%>%
  mutate(pop = as.integer(pop),
         age = as.integer(age))%>%
  filter(age==0) %>% 
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AIAN", 
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Latinx")) %>% 
  group_by(year, state, st_fips, race_ethn, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

pop_st <- pop %>% 
  filter(age<=18) %>% 
  group_by(state, year, race_ethn) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() 

pop_st<-pop_st %>% 
  group_by(state, year) %>% 
  mutate(pct_pop = pop/sum(pop) * 100) %>% 
  select(-pop)

pop_st<-pop_st %>% 
  pivot_wider(id_cols = c(state, year),
              names_from=race_ethn,
              values_from=pct_pop,
              names_prefix = "pct_") %>% 
  select(-pct_White)%>% 
  rename(pct_aian = `pct_AIAN`,
         pct_api = `pct_Asian/PI`) %>% 
  ungroup() 


# read, process, subset child file --------------------------------------

for(i in 1:length(files)){
gc()
ncands<-fread(files[i])

### clean and recode
ncands<-ncands%>%
  rename_all(tolower)

### preserve only first investigations, remove miscellaneous duplicates
ncands<-ncands %>% 
  mutate(st_id = paste(staterr, chid, sep = "")) %>% 
  left_join(index_first_investigation)

ncands<-ncands %>% 
  filter(first==T) %>% 
  filter(!duplicated(st_id))

### confirm deduplication
length(unique(ncands$st_id)) == nrow(ncands)

### recode 77 (prenatal) -> 0
ncands<-ncands %>% 
  mutate(chage = ifelse(chage==77, 0, chage))

ncands <- ncands%>% 
  mutate(rptsrc = 
           case_when(
             rptsrc==1 ~ "social services",
             rptsrc==2 ~ "medical",
             rptsrc==3 ~ "mental health",
             rptsrc==4 ~ "police",
             rptsrc==5 ~ "education",
             rptsrc==6 ~ "day care",
             rptsrc==7 ~ "substitute care ",
             rptsrc!=99 ~ "non-professional"
           )) 

### create IPSE variable (T/F/NA)

ncands <- ncands %>% 
  mutate(ipse = 
           case_when(
             cddrug == 1 | fcdrug == 1 | 
               fcalc == 1 | cdalc == 1 ~ T,
             cddrug == 2 | fcdrug == 2 | 
               fcalc == 2 | cdalc == 2 ~ F
           ))

### create race/ethnicity
ncands <- ncands %>% 
  mutate(race_ethn =
           case_when(
             chracbl==1 ~ "Black",
             chracai==1 ~ "AIAN",
             chracas==1 | chracnh==1 ~ "Asian/PI",
             cethn==1 ~ "Latinx",
             chracwh == 1 ~ "White"))

#### read in quality check
### subset to passing states, prepare for imputation

quality<-read_csv("./data/ncands_drugs_quality_check.csv") %>% 
  select(staterr, subyr, flag_1)

### join to ncands, filter for valid
ncands_valid<-ncands %>% 
  left_join(quality) %>% 
  filter(flag_1==T)

### subset to focal vars
ncands_valid <- ncands_valid %>% 
  select(staterr, subyr, rptsrc,
         ipse, chage, 
         race_ethn,
         chmal1)

### recode missings - age
ncands_valid<-ncands_valid %>% 
  mutate(chage = ifelse(chage==99, NA, chage))

## recode missing - chmal1
ncands_valid<-ncands_valid %>% 
  mutate(chmal1 = ifelse(chmal1==9, NA, chmal1),
         chmal1 = factor(chmal1))

### prepare for join

ncands_pop<-ncands_valid %>% 
  rename(year = subyr,
         age = chage,
         state = staterr) %>% 
  left_join(pop_st)

# impute ------------------------------------------------------------------

# impute m = 10 on 5 cores for states that meed validity check

library(mice)

ncands_pop<-ncands_pop %>% 
  mutate(rptsrc = factor(rptsrc),
         race_ethn = factor(race_ethn))

imp_temp<-parlmice(ncands_pop, 
               n.core = 5,
               n.imp.core = 2)

# output imputed data -----------------------------------------------------

### process and make state-level counts

imp_temp_long<-mice::complete(imp_temp, 
                          action = "long")

state_out<-imp_temp_long %>% 
  filter(age == 0, rptsrc == "medical") %>% 
  group_by(.imp, state, race_ethn, chmal1, ipse) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  tidyr::complete(.imp, state, race_ethn, chmal1, ipse, fill = list(n = 0))

write_csv(state_out, 
          paste("./data/st_ncands_infants", i, ".csv", sep = ""))

}

