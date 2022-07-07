# make_state_data.r -------------------------------------------------------
# project: med_ipse_surveillance ------------------------------------------
# Author: Frank Edwards
# Email:  frank.edwards@rutgers.edu
#
# process imputed state data
#
# log: pre-review cleanup (6/8/22)
#
rm(list=ls())
### make figures for medical ipse surveillance
library(tidyverse)
library(scales)
library(RColorBrewer)

theme_set(theme_bw())

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

#### 50 state figure
temp<-dat_in %>% 
  group_by(ipse,  year, state, .imp) %>% 
  summarise(n = sum(n)) %>% 
  summarise(n_mn = mean(n), n_min = min(n), n_max = max(n)) %>% 
  left_join(pop %>% 
  group_by(year, state) %>% 
  summarize(pop = sum(pop))) %>% 
  mutate(r_mn = n_mn / pop * 1e3, 
         r_min = n_min / pop * 1e3,
         r_max = n_max / pop * 1e3) %>% 
  mutate(ipse = factor(ipse))
# 
# library(geofacet)
# ggplot(temp,
#        aes(x = year, ymin = r_min,
#            ymax = r_max,
#            y = r_mn,
#            fill = ipse,
#            color = ipse)) + 
#   geom_line() + 
#   geom_ribbon() + 
#   facet_geo(~state) + 
#   scale_x_continuous(breaks = c(2015)) + 
#   scale_y_continuous(breaks = c(0, 30, 60)) + 
#   labs(x = "", y = "Rate per 1,000", fill = "IPSE",
#        color = "IPSE") + 
#   theme_minimal()
# 
# ggsave("./vis/IPSE_map.pdf", width = 7, height = 5)

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

write_csv(full_panel, "fig3_dat.png")

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



