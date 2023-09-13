#dependencies----
require(tidyverse)

#data reformatting----
#read in sheets file from google drive link
bd <- readxl::read_excel("Rdata/LPP.xlsx" ,sheet = 1) %>%
  filter(material != "Cellulose (Treated)") %>% #exclude Cellulose treated data from dataset
  #reformat site names,attachment style to consistent standard values
  mutate(site = str_remove(site, "-"), 
         site = str_to_lower(site),
         attachment_style = str_to_lower(attachment_style),
         #calculate diversity indices for columns which start with an upper-case letter
         shannon = vegan::diversity(select(., str_which(names(.), "^[:upper:]"))), 
         simpson = vegan::diversity(select(., str_which(names(.), "^[:upper:]")), index = "simpson"),
         invsimp = vegan::diversity(select(., str_which(names(.), "^[:upper:]")), index = "invsimpson"),
         bag_quality = eval(as.numeric(str_extract(bag_quality, "[:digit:]"))/5)) %>% #convert bag quality to numeric
  mutate(even = shannon/log(richness), #calculate shannon evenness
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

#add functional group diversity as column
biodiversity <- bd %>%
  mutate(collector_shannon = vegan::diversity(bd_matrices[["collector_matrix"]]),
         shredder_shannon = vegan::diversity(bd_matrices[["shredder_matrix"]]),
         scraper_shannon = vegan::diversity(bd_matrices[["scraper_matrix"]]),
         predator_shannon = vegan::diversity(bd_matrices[["predator_matrix"]]))

#Bray Curtis Dissimilarity calculation----
#by site
BD_BySite <- bd %>%
  group_by(site) %>%
  summarise(across(MidgeFlies:RightHandedSnails, sum)) 
  BC_BySite <- vegan::vegdist(select(BD_BySite, !site)) 
  BC_BySite_tibble <- tibble(
    site_comp = c("down-midstream", "down-upstream", "mid-upstream"),
    bc_score = as.vector(BC_BySite)
  )
  
  #by material
  BD_ByMat <- bd %>%
    group_by(material) %>%
    summarise(across(MidgeFlies:RightHandedSnails, sum)) 
  BC_ByMat <- vegan::vegdist(select(BD_ByMat, !material)) 
  BC_ByMat_tibble <- tibble(
    mat_comp = c("Biopolymer-cellulose", "BioPoly-cotton", "Cell-cotton", "Biopoly-Jute", "Cell-jute", "Cotton-Jute", "Biopolymer-Plastic", "Cell-Plastic", "Cotton-Plastic", "Jute-plastic"),
    bc_score = as.vector(BC_ByMat)
  )
  

#list of biodiversity objects
bd_list <- list(biodiversity = biodiversity,
                bd_matrices = bd_matrices)

#remove intermediary data
rm(bd, collector, shredder, scraper, predator, biodiversity, bd_matrices)