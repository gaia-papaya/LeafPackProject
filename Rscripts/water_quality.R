#dependencies:----
require(tidyverse)

#README:
#This script reads the water quality data from the 'Leaf Pack Project' sheet file on google drive @ "https://docs.google.com/spreadsheets/d/1_8Fph2bfNAaMCpI9q8LHModg78G5v4BVoOnJUHY-knE/edit#gid=0"
#sourced by the controller.R script. 

#data reforamtting----
#read in sheets file from google drive link
wq <- read_excel("Rdata/LPP.xlsx",sheet = 2, na = "NA") %>%
  select(!c(d_mean, d_se, m_mean, m_se, u_mean, u_se))%>% #select out calculated columns
  pivot_wider( names_from ="param", values_from = c("d_rep1", "d_rep2", "d_rep3",
                                                    "m_rep1", "m_rep2", "m_rep3",
                                                    "u_rep1", "u_rep2", "u_rep3")) #pivot wider  by the rep columns
#separate into 3 tables corresp to site, and remove site prefix for easy merging
#DOWNSTREAM
d_wq <- wq %>%
  select(date, str_which(names(wq), "^d")) %>%
  mutate(site = "downstream") %>%
  rename_with(.fn = ~ str_sub(.x, start = 3, end = -1), .cols = !date & !site)
#MIDSTREAM
m_wq <- wq %>%
  select(date, str_which(names(wq), "^m")) %>%
  mutate(site = "midstream") %>%
  rename_with(.fn = ~ str_sub(.x, start = 3, end = -1), .cols = !date & !site)
#UPSTREAM
u_wq <- wq %>%
  select(date, str_which(names(wq), "^u")) %>%
  mutate(site = "upstream") %>%
  rename_with(.fn = ~ str_sub(.x, start = 3, end = -1), .cols = !date & !site)
#join all three tables together row-wise, place date and site at beginning, arrange by date
wq <- bind_rows(d_wq,m_wq,u_wq) %>%
  select(date, site, everything()) %>%
  arrange(date)

#pivot into longer format to condense rep columns into 1
water_quality <- wq %>%
  pivot_longer(cols = ends_with("_P"), values_to = "P",names_to = "rn_P") %>%
  pivot_longer(cols = ends_with("_pH"), values_to = "pH",names_to = "rn_pH") %>%
  pivot_longer(cols = ends_with("_NH3"), values_to = "NH3",names_to = "rn_NH3") %>%
  pivot_longer(cols = ends_with("_DO"), values_to = "DO",names_to = "rn_DO") %>%
  pivot_longer(cols = ends_with("_Temp"), values_to = "Temp",names_to = "rn_Temp")%>%
  pivot_longer(cols = ends_with("_Cond"), values_to = "Cond",names_to = "rn_Cond") %>%
  pivot_longer(cols = ends_with("_Nitrogen"), values_to = "Nitrogen",names_to = "rn_Nitrogen") %>%
  pivot_longer(cols = ends_with("_Flow"), values_to = "Flow",names_to = "rn_Flow") %>%
  select(!starts_with("rn_"))

#NOTE TO SELF: stop calling across w/o argumetns or you might break your code!!!
#summary statsitics table grouped by site
wq_site_summary <- water_quality %>%
  group_by(site) %>%
  mutate(across(everything(), .fns = ~sd(., na.rm = T)/n(), .names = "{.col}_se")) %>%
  summarise(across( .fns = ~mean(., na.rm = TRUE)))

#summary statsitics table grouped by site and date
wq_full_summary <- water_quality %>%
  group_by(site, date) %>%
  mutate(across(.fns = ~sd(., na.rm = T)/n(), .names = "{.col}_se")) %>%
  summarise(across( .fns = ~mean(., na.rm = TRUE)))

#list of water quality objects
wq_list <- list(raw_water_quality = water_quality,
                site_summ_wq = wq_site_summary,
                summ_water_quality = wq_full_summary)

#delete intermediary wq objects
rm(d_wq, m_wq, u_wq, wq, water_quality, wq_site_summary, wq_full_summary)
