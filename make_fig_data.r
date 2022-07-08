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
         other = chmal1 >4 | chmal2 >4 | chmal3 >4 | chmal4 >4) %>% 
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

### EXCLUDED STATES
### THESE STATES EITHER A) aren't recorded in 2019 CM report (table 7-5)
### or B) screen out all or nearly all IPSE cases
excluded<-c("FL", "IA", "NY", "PA",
            "CO", "MO", "NC", "SD", 
            "VT", "WI")


### load and process SEER population data

### load pop data for imputation predictors
pop<-read_csv("./data/pop_infant09_19.csv")

### read in summary imputed NCANDS infant medical report data

files<-c("./data/st_ncands_infants1.csv",
         "./data/st_ncands_infants2.csv",
         "./data/st_ncands_infants3.csv",
         "./data/st_ncands_infants4.csv",
         "./data/st_ncands_infants5.csv",
         "./data/st_ncands_infants6.csv",
         "./data/st_ncands_infants7.csv",
         "./data/st_ncands_infants8.csv",
         "./data/st_ncands_infants9.csv",
         "./data/st_ncands_infants10.csv")

### 1:10 file index, 1 is 2010, 10 is 2019
dat_in<-list()
for(i in 1:length(files)){
  temp<-read_csv(files[i])
  temp<-temp %>% 
    mutate(year = i + 2009)
  dat_in[[i]]<-temp
}

dat_in<-bind_rows(dat_in)

### convert chmal1 into labels
dat_in<-dat_in %>% 
  mutate(mal_type = case_when(
    chmal1 == 1 ~ "Physical abuse",
    chmal1 == 2 ~ "Neglect",
    chmal1 == 3 ~ "Neglect",
    chmal1 == 4 ~ "Sexual abuse",
    chmal1 <9 ~ "Other"
  ))

### remove CM identifed 2019 report cases with invalid data
dat_in <- dat_in %>% 
  filter(!state%in%excluded)

#### Table 1: descriptives

### FOR IMPUTATION, EXTRACT THE PPD FOR THE STATE-YEARS WITHOUT OBSERVATIONS
### DONT USE OBSERVED FOR MEASUREMENT ERROR
### OBS/MAX/MIN for OBSERVED REMAIN
### IMP FOR MISSING IMPUTED FROM PPD

pop_index<-dat_in %>% 
  select(state, year) %>% 
  distinct() %>% 
  left_join(pop) %>% 
  group_by(race_ethn) %>% 
  summarize(pop = sum(pop))

#### state-level analysis
state_dat<-dat_in %>% 
  group_by(ipse, race_ethn,  year, state, .imp) %>% 
  summarise(n = sum(n)) %>% 
  summarise(n_mn = mean(n), n_min = min(n), n_max = max(n)) %>% 
  left_join(pop) %>% 
  mutate(r_mn = n_mn / pop * 1e3, 
         r_min = n_min / pop * 1e3,
         r_max = n_max / pop * 1e3)

### look at state_ts for r
full_panel<-state_dat %>% 
  group_by(state) %>% 
  summarise(n = n()) %>% 
  filter(n == max(n))

full_panel<-state_dat %>% 
  ungroup() %>% 
  filter(state%in%full_panel$state,
         state!="WV") #WV time series is clearly discontinuous

full_panel<-full_panel %>% 
  group_by(year, state, ipse) %>% 
  summarize(across(.cols = n_mn:pop, sum)) %>% 
  mutate(r_mn = n_mn / pop * 1e3, 
         r_min = n_min / pop * 1e3,
         r_max = n_max / pop * 1e3) %>% 
  mutate(race_ethn = "Total") %>% 
  ungroup() %>% 
  bind_rows(full_panel)

write_csv(full_panel, "fig3_dat.csv")

### 2019 data
dat_19<-dat_in %>% 
  filter(ipse == T,
         year == 2019) %>% 
  group_by(race_ethn,  year, state, .imp) %>% 
  summarise(n = sum(n)) %>% 
  summarise(n_mn = mean(n), n_min = min(n), n_max = max(n)) %>% 
  left_join(pop) %>% 
  mutate(r_mn = n_mn / pop * 1e3, 
         r_min = n_min / pop * 1e3,
         r_max = n_max / pop * 1e3) %>% 
  bind_rows(dat_in %>% 
              filter(ipse == T,
                     year == 2019) %>% 
              group_by(race_ethn,  year, state, .imp) %>% 
              summarise(n = sum(n)) %>% 
              summarise(n_mn = mean(n), n_min = min(n), n_max = max(n)) %>% 
              left_join(pop) %>% 
              group_by(year, state) %>% 
              summarize(across(.cols = n_mn:pop, sum) %>% 
                          mutate(r_mn = n_mn / pop * 1e3, 
                                 r_min = n_min / pop * 1e3,
                                 r_max = n_max / pop * 1e3,
                                 race_ethn = "Total")))

### format for plotting
dat_19 <- dat_19 %>% 
  mutate(race_ethn = factor(race_ethn, levels = c("Total", "AIAN", "Asian/PI", "Black", "Latinx", "White"))) 

order<-dat_19 %>% 
  filter(race_ethn=="Total") %>% 
  arrange(r_mn) %>% 
  mutate(ord = 1:n()) %>% 
  select(state, ord)

dat_19<-dat_19 %>% 
  mutate(state = factor(state, levels = order$state))

write_csv(dat_19, "./data/fig4_dat.csv")

### 2019 disparity data
dat_19_w<-dat_19 %>% 
  ungroup() %>% 
  filter(race_ethn == "White") %>% 
  rename(wht_mn = r_mn,
         wht_min = r_min,
         wht_max = r_max) %>% 
  select(year, state, wht_mn, wht_min, wht_max)

dat_19_disp<-dat_19 %>% 
  left_join(dat_19_w) %>% 
  mutate(d = r_mn / wht_mn,
         d_min = r_min / wht_max,
         d_max = r_max / wht_min) %>% 
  filter(race_ethn!="Total" & race_ethn!="White")

order<-dat_19_disp %>% 
  filter(race_ethn=="AIAN") %>% 
  arrange(d) %>% 
  mutate(ord = 1:n()) %>% 
  select(state, ord)

dat_19_disp<-dat_19_disp %>% 
  mutate(state = factor(state, levels = order$state)) %>% 
  mutate(grt1 = ifelse(d_min>1, 
                       "Greater than white rate",
                       "Less than or equal to white rate"))

write_csv(dat_19_disp, "./data/fig5_dat.csv")
