#README #README #README #README #README #README #README #README #README:
#Hello! this is the data analysis script for the bag project. YOU MUST SOURCE bag_source.R IN ORDER FOR
#THIS SCRIPT TO WORK!!!
#control panel----
#set to true to remove intermediate objects
REMOVE_INTERMED = TRUE

#reading in data----

#this code is deprecated owo why are you snooping here???

#BIODIVERSITY
data <- read_sheet("https://docs.google.com/spreadsheets/d/1_8Fph2bfNAaMCpI9q8LHModg78G5v4BVoOnJUHY-knE/edit#gid=1699683095",
                 sheet = 1) %>%
  #reformat site names to match wq table
  mutate(site = str_remove(site, "-"), #remove dashes
     site = str_to_lower(site)) %>%
  filter(material != "Cellulose (Treated)") %>%
  #calculate base diversity indices, selecting for strings which start with an underscore 
  mutate(shannon = diversity(select(., str_which(names(.), "^[:upper:]"))), 
         simpson =diversity(select(., str_which(names(.), "^[:upper:]")), index = "simpson"),
     invsimp = diversity(select(., str_which(names(.), "^[:upper:]")), index = "invsimpson"),
   #calc numeric bag quality from string
       bag_quality = eval(as.numeric(str_extract(bag_quality, "[:digit:]"))/5)) %>%
  #calc evenness from shannon
  mutate(even = shannon/log(richness),
         #TODO: FILL OUT CASE_WHEN WITH THE REST OF THE MESH SIZE VALUES (ENSURE CORRECT SPELLING)
         mesh_size = case_when(material == "Biopolymer" ~ 10,
                               material == "Cellulose (Untreated)" ~ 3,
                               material == "Plastic" ~ 12.7,
                               material == "Jute" ~ 17.78,
                               material == "Cotton" ~ 5.08)) %>%  
  #arrange by date
arrange(date)

#JOINING WQ AND BIODIVERSITY
#join water quality to data by date and site
joined <- right_join(x = wq, y = data, by = c("date", "site"), multiple = "all") %>%
select(material,date,site, everything())

#FUNCTIONAL GROUPS 
#vectors of funtional group column names
collector <- c("MidgeFlies", "BlackFlies", "Planaria", "AquaticWorms", "Amphipods", "NetSpinningCaddisflies", "ClamsMussels")
shredder <- c("Sowbugs", "CraneFlies", "Stoneflies", "RiffleBeetles")
scraper <- c("MidgeFlies", "LeftHandedSnails", "RightHandedSnails", "Mayflies", "RiffleBeetles")
predator <- c("Damselflies", "CraneFlies", "Crayfish", "Leeches")

#nested list containing tibbles of taxa counts from respective functional groups
fn_groups <- list(
  #taxa data for collectors
Collector = select(joined, material, date, site, all_of(collector), mesh_size) %>%
  mutate(shannon = diversity(select(., str_which(names(.), "^[:upper:]"))), 
          across(str_which(names(.), '^[:upper:]'), as.logical, .names = "lol_{.col}")) %>%
  mutate(richness = rowSums(.[paste("lol_", collector, sep = "")] == TRUE),
        even = shannon/log(richness),
         #row sum of species counts 
         count_sum = rowSums(select(.,str_which(names(.), '^[:upper:]')))) %>%
  select(!starts_with("lol_")), 
#taxa data for shredders
Shredder = select(joined, material, date, site, all_of(shredder),  mesh_size) %>%
  mutate(shannon = diversity(select(., str_which(names(.), "^[:upper:]"))), 
         across(str_which(names(.), '^[:upper:]'), as.logical, .names = "lol_{.col}")) %>%
  mutate(richness = rowSums(.[paste("lol_", shredder, sep = "")] == TRUE),
         even = shannon/log(richness),
         #row sum of species counts 
         count_sum = rowSums(select(.,str_which(names(.), '^[:upper:]')))) %>%
  select(!starts_with("lol_")),
#taxa data for scrapers
Scraper = select(joined, material, date, site, all_of(scraper),  mesh_size)%>%
  mutate(shannon = diversity(select(., str_which(names(.), "^[:upper:]"))), 
         across(str_which(names(.), '^[:upper:]'), as.logical, .names = "lol_{.col}")) %>%
  mutate(richness = rowSums(.[paste("lol_", scraper, sep = "")] == TRUE),
         even = shannon/log(richness),
         #row sum of species counts 
         count_sum = rowSums(select(.,str_which(names(.), '^[:upper:]')))) %>%
  select(!starts_with("lol_")),
#taxa data for predators
Predator = select(joined, material, date, site, all_of(predator),  mesh_size)%>%
  mutate(shannon = diversity(select(., str_which(names(.), "^[:upper:]"))), 
         across(str_which(names(.), '^[:upper:]'), as.logical, .names = "lol_{.col}")) %>%
  mutate(richness = rowSums(.[paste("lol_", predator, sep = "")] == TRUE),
         even = shannon/log(richness),
         #row sum of species counts 
         count_sum = rowSums(select(.,str_which(names(.), '^[:upper:]')))) %>%
select(!starts_with("lol_")))

#read cummulative fn group sheet
fn_cummul <- read_sheet("https://docs.google.com/spreadsheets/d/1_8Fph2bfNAaMCpI9q8LHModg78G5v4BVoOnJUHY-knE/edit#gid=1167437605",
                        sheet = 3) %>%
  group_by(Material) %>%
  mutate(prop = Count/sum(Count))

#counts of unique taxa by functional groups
fn_count <- joined %>%
mutate(across(str_which(names(.), '^[:upper:]'), as.logical)) %>%
mutate(Collector = rowSums(.[collector] == TRUE),
         Predator = rowSums(.[predator] == TRUE),
         Scraper = rowSums(.[scraper] == TRUE),
         Shredder = rowSums(.[shredder] == TRUE)) %>%
  select(material,date,site, Collector, Predator, Scraper, Shredder,  mesh_size) %>%
  mutate(total = rowSums(.[c("Collector", "Scraper", "Shredder", "Predator")]),
        fn_even = diversity(select(., str_which(names(.), "^[:upper:]")))/log(total)) 


if(REMOVE_INTERMED == TRUE){
  #water quality intermediaries
  rm(d_wq,m_wq,u_wq)
  
  #biodiversity intermediaries
  rm(data, wq)
  
  #functional group intermediaries
  rm(collector, predator, scraper, shredder)
}

#stat analysis:----
#creating aov modelz objects for ANOVA testing
anova_models <- list(shannon = aov(shannon ~ material + as.factor(date)   , data = joined),
                simpson = aov(simpson ~ material + as.factor(date)   , data = joined),
                evenness = aov(even ~ material + as.factor(date)   , data = joined),
              richness = aov(richness ~ material + as.factor(date)   , data = joined),
              fn_even = aov(fn_even ~material + as.factor(date)   , data = fn_count),
              coll_even = aov(even ~ material + as.factor(date)   , data = fn_groups$Collector),
              scr_even = aov(even ~ material + as.factor(date)   , data = fn_groups$Scraper),
              pre_even = aov(even ~ material + as.factor(date)   , data = fn_groups$Predator),
              shr_even = aov(even ~ material + as.factor(date)   , data = fn_groups$Shredder))
wq_models <- list(
  
)

#running  ANOVA test
tests <- anova_models %>%
        {list(shannon = Anova(.$shannon),
              simpson = Anova(.$simpson),
              eveneness = Anova(.$evenness),
              richness = Anova(.$richness),
              fn_even = Anova(.$fn_even),
              coll_even = Anova(.$coll_even),
            scr_even = Anova(.$scr_even),
              pre_even = Anova(.$pre_even),
              shr_even = Anova(.$shr_even))}
#print test vals
print(tests) 


print(tukeys)

#linear regression models
mesh_size_mod <- lm(richness ~ mesh_size, data = joined)
summary(mesh_size_mod)

bag_quality_mod <- lm(richness ~ bag_quality, data = joined)
summary(bag_quality_mod)

#PLOTTING----
#cummulative diversity data
bt_barplot(joined,"shannon") + labs(x="Material", y="Shannon Diversity Index")
bt_barplot(joined, "even")+ labs(x="Material", y="Shannon Eveness", title= "Shannon Eveness by Bag Material")
bt_barplot(joined, "simpson") + labs(x="Material", y="Simpson Diversity Score", title= "Simpson Diversity Score by Bag Material")
bt_barplot(joined, "richness")+ labs(x="Material", y="Richness", title="Richness by Bag Material")

#by functional groups
#collectors
bt_barplot(fn_groups$Collector, "shannon")
bt_barplot(fn_groups$Collector, "richness")
bt_barplot(fn_groups$Collector, "even")

#scrapers
bt_barplot(fn_groups$Scraper, "shannon")
bt_barplot(fn_groups$Scraper, "richness")
bt_barplot(fn_groups$Scraper, "even")

#shredders
bt_barplot(fn_groups$Shredder, "shannon")
bt_barplot(fn_groups$Shredder, "richness")
bt_barplot(fn_groups$Shredder, "even")

#predators
bt_barplot(fn_groups$Predator, "shannon")
bt_barplot(fn_groups$Predator, "richness")
bt_barplot(fn_groups$Predator, "even")


ggplot(joined, aes(x = bag_quality*5, y = richness)) +
  geom_point(aes(color = material)) + geom_smooth(method = "lm", se = F)+labs(x="Bag Quality Score", y="Richness",title="Correlation Between Richness and Bag Quality Score ")

ggplot(joined, aes(x = site, y = mean_Nitrogen)) + geom_point()

ggplot(data = fn_cummul, aes(y = prop, x = Material, fill = FNG)) +
  geom_col() +
  scale_fill_viridis_d()+theme_rafa(base_size=12)+labs(title="Proportions of Functional Groups by Bag Type", y="Proportion")


