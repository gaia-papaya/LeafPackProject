#dependencies----
require(readxl)
require(tidyverse)
require(vegan)

#README:
#this script reads the biodiveristy data from the 'Leaf Pack Project' sheet file on google drive @ "https://docs.google.com/spreadsheets/d/1_8Fph2bfNAaMCpI9q8LHModg78G5v4BVoOnJUHY-knE/edit#gid=0"
#sourced by the controller.R script. 

#data reforamtting----

#data reforamtting----
#read in sheets file from google drive link
bd <- read_excel("Rdata/LPP.xlsx" ,sheet = 1) %>%
  filter(material != "Cellulose (Treated)") %>% #exclude Cellulose treated data from dataset
  #reformat site names,attachment style to consistent standard values
  mutate(site = str_remove(site, "-"), 
         site = str_to_lower(site),
         attachment_style = str_to_lower(attachment_style),
         #calculate diversity indices for columns which start with an upper-case letter
         shannon = diversity(select(., str_which(names(.), "^[:upper:]"))), 
         simpson =diversity(select(., str_which(names(.), "^[:upper:]")), index = "simpson"),
         invsimp = diversity(select(., str_which(names(.), "^[:upper:]")), index = "invsimpson"),
         bag_quality = eval(as.numeric(str_extract(bag_quality, "[:digit:]"))/5)) %>% #convert bag quality to numeric
  mutate(even = shannon/log(richness), #calcluate shannon evenness
         mesh_size = case_when(material == "Biopolymer" ~ 10,
                               material == "Cellulose" ~ 3, #derive mesh size value from material
                               material == "Plastic" ~ 12.7,
                               material == "Jute" ~ 17.78,
                               material == "Cotton" ~ 5.08)) %>%
  arrange(date)   #arrange by date

#functional group catergorization----
#create vectors of functional group member taxa
collector <- c("MidgeFlies", "BlackFlies", "Planaria", "AquaticWorms", "Amphipods", "NetSpinningCaddisflies", "ClamsMussels")
shredder <- c("Sowbugs", "CraneFlies", "Stoneflies", "RiffleBeetles")
scraper <- c("MidgeFlies", "LeftHandedSnails", "RightHandedSnails", "Mayflies", "RiffleBeetles")
predator <- c("Damselflies", "CraneFlies", "Crayfish", "Leeches")

#list of functional group matrices
bd_matrices <- list(
  collector_matrix = as.matrix(bd[collector]),
  shredder_matrix = as.matrix(bd[shredder]),
  scraper_matrix = as.matrix(bd[scraper]),
  predator_matrix = as.matrix(bd[predator])
)

#add functional group diversity as collumn
biodiversity <- bd %>%
  mutate(collector_shannon = diversity(bd_matrices[["collector_matrix"]]),
         shredder_shannon = diversity(bd_matrices[["shredder_matrix"]]),
         scraper_shannon = diversity(bd_matrices[["scraper_matrix"]]),
         predator_shannon = diversity(bd_matrices[["predator_matrix"]]))

#list of biodiversity objects
bd_list <- list(biodiversity = biodiversity,
                bd_matrices = bd_matrices)

#remove intermediary data
rm(bd, collector, shredder, scraper, predator, biodiversity, bd_matrices)