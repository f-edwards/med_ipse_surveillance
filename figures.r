# figures.r ---------------------------------------------------------------
# project: med_ipse_surveillance ------------------------------------------
# Author: Frank Edwards
# Email:  frank.edwards@rutgers.edu
#
# Creates figures for med_ipse_surveillance manuscript
#
# log: pre-review cleanup (6/8/22)
#


# load packages -----------------------------------------------------------

library(geofacet)
library(tidyverse)
library(knitr)
library(gridExtra)
library(scales)
library(RColorBrewer)

theme_set(theme_bw())

# process data ------------------------------------------------------------

source("make_state_data.R")

# decsriptive table -------------------------------------------------------

pop_index<-dat_in %>% 
  select(state, year) %>% 
  distinct() %>% 
  left_join(pop) %>% 
  group_by(race_ethn) %>% 
  summarize(pop = sum(pop))

tab1_all<-dat_in %>% 
  group_by(race_ethn, .imp) %>% 
  summarize(n = sum(n)) %>% 
  summarize(n_mn = mean(n),
            n_max = max(n),
            n_min = min(n)) %>% 
  left_join(pop_index) %>% 
  mutate(r_mn = n_mn/pop*1e3,
         r_max = n_max/pop*1e3,
         r_min = n_min/pop*1e3)

tab1_ipse<-dat_in %>% 
  filter(ipse==1) %>% 
  group_by(race_ethn, .imp) %>% 
  summarize(n = sum(n)) %>% 
  summarize(n_mn = mean(n),
            n_max = max(n),
            n_min = min(n)) %>% 
  left_join(pop_index) %>% 
  mutate(r_mn = n_mn/pop*1e3,
         r_max = n_max/pop*1e3,
         r_min = n_min/pop*1e3)

state_years<-dat_in %>% 
  select(state, year) %>% 
  distinct() %>% 
  group_by(state) %>% 
  summarize(n = n())


# figure 1 ----------------------------------------------------------------

ncands_med<-read_csv("./data/fig1_dat.csv")
ncands_med<-ncands_med %>% 
  mutate(medical_rptsrc = factor(medical_rptsrc,
                       levels = c("Medical professionals",
                                  "All others")))

ggplot(ncands_med, 
       aes(x = year, y = rate_mean, 
           ymin = rate_min,
           ymax = rate_max,
           color = type)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~medical_rptsrc) + 
  labs(x = "Year", y = "Rate per 1,000 infants", color = "Alleged type of\nmaltreatment") + 
  theme(legend.title = element_blank()) + 
  scale_x_continuous(breaks=c(2010, 2012, 2014, 2016, 2018)) 

ggsave("./vis/fig1.png", width = 6, height = 4)


### text call outs
ncands_med %>% 
  group_by(medical_rptsrc, year) %>% 
  summarise(n_mean = sum(n_mean),
            n_min = sum(n_min),
            n_max = sum(n_max))

# figure 2 ----------------------------------------------------------------

fig_dat<-read_csv("./data/fig2_dat.csv")

ggplot(fig_dat %>% 
         filter(medical_rptsrc==T), 
       aes(x = year,
           y = rate_bar,
           ymin = rate_min,
           ymax = rate_max,
           color = race_ethn,
           fill = race_ethn)) + 
  geom_point() + 
  geom_line() + 
  geom_ribbon(alpha = 0.5, aes(color = NULL)) + 
  scale_x_continuous(breaks=c(2010, 2012, 2014, 2016, 2018))+ 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Year", y = "Rate per 1,000 infants",
       color = "",
       fill = "") + 
  theme_bw()

ggsave("./vis/fig2.png", width = 6, height = 4)


# figure 3 ----------------------------------------------------------------

ggplot(full_panel %>% 
         filter(race_ethn=="Total") %>% 
         mutate(ipse = ifelse(ipse==1, "IPSE", "Non-IPSE")),
       aes(x = year, y = r_mn,
           ymin = r_min,
           ymax = r_max,
           color = factor(ipse),
           fill = factor(ipse))) + 
  geom_line() + 
  geom_ribbon(aes(color = NULL), alpha = 0.5) + 
  facet_wrap(~state) + 
  scale_x_continuous(breaks=c(2012, 2015, 2018)) + 
  labs(x = "Year", 
       y = "Rate per 1,000 infants", 
       color = "",
       fill = "")

ggsave("./vis/fig3.png", width = 8, height = 4)
  
# figure 4 ----------------------------------------------------------------

ggplot(dat_19,
       aes(x = state, 
           y = r_mn,
           ymin = r_min,
           ymax = r_max)) + 
  geom_point(size = 1) +
  geom_linerange() + 
  facet_wrap(~race_ethn, nrow = 1) + 
  coord_flip() + 
  labs(y = "Rate per 1,000 infants", x= "", color = "")

ggsave("./vis/fig4.png", width = 8, height = 4)

### in-text call outs
dat_19 %>% 
  group_by(race_ethn) %>% 
  summarize(rate = sum(n_mn) / sum(pop) * 1e3)
             
# figure 5 ----------------------------------------------------------------

ggplot(dat_19_disp,
       aes(x = state, 
           y = d,
           ymin = d_min,
           ymax = d_max,
           color = grt1)) + 
  geom_point() +
  geom_linerange() + 
  facet_wrap(~race_ethn, nrow = 1) + 
  coord_flip() + 
  geom_hline(yintercept = 1, lty = 2) + 
  labs(y = "Rate ratio", x= "", color = "") + 
  theme(legend.position = "bottom")

ggsave("./vis/fig5.png", width = 8, height = 4)

