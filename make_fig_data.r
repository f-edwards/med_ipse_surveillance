# process_national_ncands.r -----------------------------------------------
# take imputed ncands, subset to infants
# create tables for figures.r

rm(list=ls())
gc()
library(tidyverse)
library(data.table)
library(mice)
library(lubridate)
library(mice)

i<-2010:2019
  
filenames<-paste(
  "~/Projects/ndacan_processing/imputations/ncands_imps",
  i,
  ".rds",
  sep = ""
)

ncands_imps<-list()

for(i in 1:length(filenames)){
  ### read each imputed file, subset to needed vars
  ### subset to infants, preserve only 1st investigation 
  ### per imputation
  ncands_imps[[i]]<-read_rds(filenames[i])
  ncands_imps[[i]]<-mice::complete(ncands_imps[[i]], 
                                   action = "long")
  ncands_imps[[i]]<-ncands_imps[[i]] %>% 
    mutate(st_id = paste(staterr, chid, sep = "")) %>% 
    select(.imp, .id, subyr, rptdt, staterr, age,
           rptsrc, race_ethn, st_id, chmal1, 
           chmal2, chmal3, chmal4) %>% 
    filter(age==0) %>% 
    arrange(desc(rptdt)) %>% 
    group_by(.imp) %>% 
    filter(!(duplicated(st_id)))
}

# unlist

ncands_imps<-bind_rows(ncands_imps) 


### convert reporter to medical, ! medical, create maltype vars
ncands_imps<-ncands_imps %>% 
  mutate(neglect = chmal1 == 2 | chmal2 == 2 | chmal3 == 2 | chmal4 == 2 |
           chmal1 == 3 | chmal2 == 3 | chmal3 == 3 | chmal4 == 3 ,
         phy_abuse = chmal1 == 1 | chmal2 == 1 | chmal3 == 1 | chmal4 == 1,
         sex_abuse = chmal1 == 4 | chmal2 == 4 | chmal3 == 4 | chmal4 == 4,
         other = chmal1 >4 | chmal2 >4 | chmal3 >4 | chmal4 >4 ) %>% 
  mutate(medical_rptsrc = rptsrc==2) %>% 
  select(.imp, st_id, subyr, race_ethn, 
         staterr, medical_rptsrc,
         neglect, phy_abuse, sex_abuse, other)



### create counts, join pop
ncands_imps_count<-ncands_imps %>%
  rename(state = staterr,
         year = subyr) %>%
  group_by(.imp, state, year, race_ethn, medical_rptsrc) %>%
  summarize(phy_abuse = sum(phy_abuse, na.rm=T),
            neglect = sum(neglect, na.rm=T),
            sex_abuse = sum(sex_abuse, na.rm=T),
            other = sum(other, na.rm=T)) %>% 
  ungroup()

##### MAKE FIG 1 and FIG 2 TABLES for figures.r

# figure 1 data -----------------------------------------------------------

pop <- read_fwf("~/Projects/data/us.1990_2019.singleages.adjusted.txt",
                fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                           c("year", "state", "st_fips",
                             "cnty_fips", "reg", "race",
                             "hisp", "sex", "age", "pop"))) %>% 
  filter(year>2009, age == "00") %>% 
  mutate(pop = as.integer(pop),
         age = as.integer(age))%>%
  filter(age==0) %>% 
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AIAN", 
             race==4 ~ "API",
             hisp==1 ~ "Latinx")) %>% 
  group_by(year, race_ethn, ) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

### compute intervals

fig1_dat<-ncands_imps_count %>% 
  pivot_longer(phy_abuse:other,
               values_to = "n",
               names_to = "type") %>% 
  group_by(.imp, year, 
           medical_rptsrc, type) %>% 
  summarize(n = sum(n)) %>% 
  group_by(year,  
           medical_rptsrc, type) %>% 
  summarize(n_mean = mean(n),
            n_min = min(n),
            n_max = max(n)) %>% 
  left_join(pop %>% 
              group_by(year) %>% 
              summarize(pop = sum(pop))) %>% 
  mutate(rate_mean = n_mean / pop * 1e3,
         rate_min = n_min / pop * 1e3,
         rate_max = n_max / pop * 1e3)

### recode for presentation
fig1_dat<-fig1_dat %>% 
  mutate(medical_rptsrc = 
           case_when(medical_rptsrc == T ~ "Medical professionals",
                     medical_rptsrc == F ~ "All others")) %>% 
  mutate(type = case_when(
    type == "neglect" ~ "Neglect",
    type == "phy_abuse" ~ "Physical abuse",
    type == "sex_abuse" ~ "Sexual abuse",
    type == "other" ~ "Other"
  )) %>% 
  mutate(type = factor(type,
                       levels = c("Neglect",
                                  "Physical abuse",
                                  "Sexual abuse",
                                  "Other"))) %>% 
  mutate(medical_rptsrc = factor(medical_rptsrc,
                                 levels = c("Medical professionals",
                                            "All others")))


write_csv(fig1_dat, "./data/fig1_dat.csv")

# FIG 2 -----------------------------------------------------------------

ncands_imps_count<-ncands_imps %>% 
  group_by(subyr, race_ethn,  medical_rptsrc, .imp) %>% 
  summarize(n = n()) %>% 
  summarize(n_max = max(n),
            n_min = min(n),
            n_bar = mean(n))

ncands_imps_count<-ncands_imps_count %>%
  filter(subyr>2009) %>% 
  rename(year = subyr) %>% 
  left_join(pop) %>% 
  mutate(rate_max = n_max / pop * 1e3,
         rate_min = n_min / pop * 1e3,
         rate_bar = n_bar / pop * 1e3)

write_csv(ncands_imps_count, "./data/fig2_dat.csv")
