#data reforamtting----
#read in sheets file from google drive link
wq <- readxl::read_excel("Rdata/LPP.xlsx",sheet = 2, na = "NA") %>%
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
  group_by(date, site) %>% 
  pivot_longer(cols = ends_with("_P"), values_to = "P",names_to = "rn_P") %>% 
  mutate(P_mean = mean(P, na.rm = T),
         P_se = sd(P, na.rm = T)/sqrt(n())) %>% 
  distinct(P_mean,.keep_all = T) %>% 
  select(!rn_P) %>% 
  pivot_longer(cols = ends_with("_pH"), values_to = "pH",names_to = "rn_pH") %>%
    mutate(pH_mean = mean(pH, na.rm = T),
           pH_se = sd(pH, na.rm = T)/sqrt(n())) %>% 
    distinct(pH_mean,.keep_all = T) %>% 
  select(!rn_pH) %>% 
  pivot_longer(cols = ends_with("_NH3"), values_to = "NH3",names_to = "rn_NH3") %>%
  mutate(NH3_mean = mean(NH3, na.rm = T),
         NH3_se = sd(NH3, na.rm = T)/sqrt(n())) %>% 
  distinct(NH3_mean,.keep_all = T) %>% 
  select(!rn_NH3) %>% 
  pivot_longer(cols = ends_with("_DO"), values_to = "DO",names_to = "rn_DO") %>%
  mutate(DO_mean = mean(DO, na.rm = T),
         DO_se = sd(DO, na.rm = T)/sqrt(n())) %>% 
  distinct(DO_mean,.keep_all = T) %>% 
  select(!rn_DO) %>% 
  pivot_longer(cols = ends_with("_Temp"), values_to = "Temp",names_to = "rn_Temp")%>%
  mutate(Temp_mean = mean(Temp, na.rm = T),
         Temp_se = sd(Temp, na.rm = T)/sqrt(n())) %>% 
  distinct(Temp_mean,.keep_all = T) %>% 
  select(!rn_Temp) %>% 
  pivot_longer(cols = ends_with("_Cond"), values_to = "Cond",names_to = "rn_Cond") %>%
  mutate(Cond_mean = mean(Cond, na.rm = T),
         Cond_se = sd(Cond, na.rm = T)/sqrt(n())) %>% 
  distinct(Cond_mean,.keep_all = T) %>% 
  select(!rn_Cond) %>% 
  pivot_longer(cols = ends_with("_Nitrogen"), values_to = "Nitrogen",names_to = "rn_Nitrogen") %>%
  mutate(Nitrogen_mean = mean(Nitrogen, na.rm = T),
         Nitrogen_se = sd(Nitrogen, na.rm = T)/sqrt(n())) %>% 
  distinct(Nitrogen_mean,.keep_all = T) %>% 
  select(!rn_Nitrogen) %>% 
  pivot_longer(cols = ends_with("_Flow"), values_to = "Flow",names_to = "rn_Flow") %>%
  mutate(Flow_mean = mean(Flow, na.rm = T),
         Flow_se = sd(Flow, na.rm = T)/sqrt(n())) %>% 
  distinct(Flow_mean,.keep_all = T) %>% 
  select(!rn_Flow & ends_with("mean") | ends_with("se")) %>%  
 filter(P_mean <4) #filter out outlier observations

#delete intermediary wq objects
rm(d_wq, m_wq, u_wq, wq)
