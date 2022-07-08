# impute_ncands10_20.r ----------------------------------------------------
# use ncands child file to compute
# 2010 - 2020 imputations for missing age and race data


#### REWRITE THIS, INCLUDE RPTSRC, MALTYPE, OTHERS NEEDED FOR IPSE

rm(list=ls()); gc()
library(lubridate)
library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(mice)

ncands_path<-"~/Projects/ndacan_data/ncands/"
ncands_files<-paste(ncands_path,
                    list.files(ncands_path),
                    sep = "")
#### filter for 2010 - 2020 for imputation
ncands_files<-paste(ncands_path, list.files(ncands_path), sep = "")[9:19]

ncands<-lapply(ncands_files, fread)


for(i in 1:length(ncands_files)){
  ncands[[i]]<-ncands[[i]]%>%
    rename_all(tolower) %>%
    mutate(race_ethn =
             ifelse(chracbl==1,
                    "Black",
                    ifelse(chracai==1, "AIAN",
                           ifelse(chracas==1 | chracnh==1,
                                  "API",
                                  ifelse(cethn==1, "Latinx",
                                         ifelse(chracwh == 1, "White",
                                                NA)))))) %>%
    mutate(stfcid = paste(staterr, afcarsid, sep = "")) %>% 
    select(chid, 
           subyr, 
           staterr,
           rptfips, 
           rptdt,
           chage, 
           rptsrc,
           race_ethn,
           chprior, 
           rptvictim,
           subyr,
           chmal1,
           chmal2,
           chmal3,
           chmal4)
  }

ncands<-bind_rows(ncands)

## recode missings
ncands <- ncands %>% 
  rename(age = chage) %>% 
  mutate(age = ifelse(age==77, 0, age)) %>% 
  mutate(age = ifelse(age>18, 
                      NA,
                      age)) %>% 
  mutate(rptsrc = ifelse(is.na(rptsrc),
                         99,
                         rptsrc))

### assumptions: age>18 is miscoded, rptsrc == NA is not professional reporter


# read pop data for composition predictors --------------------------------

pop<-read_fwf("~/Projects/data/us.1990_2020.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")))

pop<-pop%>%
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AI/AN", 
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Hispanic")) 

pop_st <- pop %>% 
  filter(age<=18) %>% 
  group_by(state, year, st_fips, race_ethn) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  rename(staterr = state) %>% 
  select(-st_fips)

pop_st<-pop_st %>% 
  group_by(staterr, year) %>% 
  mutate(pct_pop = pop/sum(pop)) %>% 
  select(-pop)

pop_st<-pop_st %>% 
  pivot_wider(id_cols = c(staterr, year),
              names_from=race_ethn,
              values_from=pct_pop,
              names_prefix = "pct_") %>% 
  select(-pct_White)%>% 
  rename(pct_aian = `pct_AI/AN`,
         pct_api = `pct_Asian/PI`) %>% 
  ungroup() %>% 
  rename(subyr = year)

# join ncands to pop and format for imputation

ncands_pop<-ncands %>% 
  left_join(pop_st)

ncands_pop<-ncands_pop %>% 
  mutate(staterr = factor(staterr),
         rptfips = as.character(rptfips),
         race_ethn = factor(race_ethn),
         rptvictim = factor(rptvictim),
         chprior = chprior == 1,
         rptsrc = factor(rptsrc),
         chmal1 = as.character(chmal1),
         chmal2 = as.character(chmal2),
         chmal3 = as.character(chmal3),
         chmal4 = as.character(chmal4)) %>% 
  filter(!staterr%in%c("PR", "XX"))

### PAUSE: confirm file structure here

imps<-mice(ncands_pop %>% 
             filter(subyr==2016), m=1, maxit=0)

pred<-imps$predictorMatrix
### turn off ids, chmal vars
pred[1:5,]<-0
pred[,1:5]<-0
pred[11:14,]<-0
pred[,11:14]<-0
# 
meth<-imps$method
# meth$rptsrc<-list("pmm")
# meth$age<-list("pmm")
# meth$race_ethn<-list("polyreg")
# meth$chprior<-list("pmm")


rm(ncands); rm(pop); gc()

years<-unique(ncands_pop$subyr)

### missings on rptsrc (small number) led to some non-imputed race_ethn values

for(i in 1:length(years)){
  imp_dat<-ncands_pop %>% 
    filter(subyr == years[i]) 
  
  imps<-parlmice(imp_dat, 
                 n.imp.core=2,
                 predictorMatrix = pred,
                 method = meth,
                 n.core = 5)
  filename<-paste("./imputations/ncands_imps", years[i], ".rds", sep="")
  saveRDS(imps, file = filename)
}

