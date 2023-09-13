#join diversity data and water quality data together
LPP_FullData <- right_join(x = wq_list$summ_water_quality, y = bd_list$biodiversity, by = c("date", "site"), multiple = "all")

#DIVERSITY MODELS----
#FORMULA: INDEX ~ MATERIAL + DATE
diversity_mod_objects <- list(#taxa wide anova model objects
  shannon = aov(shannon ~ material + as.factor(date) , data = LPP_FullData),
  simpson = aov(simpson ~ material + as.factor(date) , data = LPP_FullData),
  invsimp = aov(simpson ~ material + as.factor(date) , data = LPP_FullData),
  evenness = aov(even ~ material + as.factor(date) , data = LPP_FullData),
  richness = aov(richness ~ material + as.factor(date), data = LPP_FullData),
  #functional group anova model objects
  coll_shannon = aov(collector_shannon ~ material + as.factor(date)   , data = LPP_FullData),
  scr_shannon = aov(shredder_shannon ~ material + as.factor(date)   , data = LPP_FullData),
  pre_shannon = aov(predator_shannon ~ material + as.factor(date)   , data = LPP_FullData),
  shr_shannon = aov(shredder_shannon ~ material + as.factor(date)   , data = LPP_FullData))

#Type two anova list using car::car::Anova function 
diversity_tests <- diversity_mod_objects %>%
  {list(shannon = car::Anova(.$shannon),
        simpson = car::Anova(.$simpson),
        invsimp = car::Anova(.$invsimp),
        eveneness = car::Anova(.$evenness),
        richness = car::Anova(.$richness),
        coll_shannon = car::Anova(.$coll_shannon),
        scr_shannon = car::Anova(.$scr_shannon),
        pre_shannon = car::Anova(.$pre_shannon),
        shr_shannon = car::Anova(.$shr_shannon))}

#perform tukey tests for all indices
list() -> diversity_tukeys
for(t in 1:length(diversity_tests)){
  
    cat(paste(names(diversity_mod_objects[t]), "had significant (<0.05) results.",
              "\n", "performing Tukey tests...",
              "\n"))
    diversity_tukeys[[t]] <- agricolae::HSD.test(diversity_mod_objects[[t]], "material", group = T)
    names(diversity_tukeys)[t] <- names(diversity_tests)[[t]]

  
}

#WATER QUALITY MODELS----
#FORMULA: PARAMETER ~ DATE + SITE
#wq model objects
wq_mod_objects <- list(
  P = aov(P~as.factor(date)+site, data = wq_list$raw_water_quality),
  pH = aov(pH~as.factor(date)+site, data = wq_list$raw_water_quality),
  NH3 = aov(NH3~as.factor(date)+site, data = wq_list$raw_water_quality),
  DO = aov(DO~as.factor(date)+site, data = wq_list$raw_water_quality),
  Temp = aov(Temp~as.factor(date)+site, data = wq_list$raw_water_quality),
  Cond = aov(Cond~as.factor(date)+site, data = wq_list$raw_water_quality),
  Flow = aov(Flow~as.factor(date)+site, data = wq_list$raw_water_quality)
)

#T2 anova
wq_tests <- wq_mod_objects %>%
  {list(
  P = car::Anova(.$P),
  pH = car::Anova(.$pH),
  NH3 = car::Anova(.$NH3),
  DO = car::Anova(.$DO),
  Temp = car::Anova(.$Temp),
  Cond = car::Anova(.$Cond),
  Flow = car::Anova(.$Flow))
  }


#set seed for repeatability
set.seed(420)

#Nonmetric Multidimensional Scaling (NMDS) of Biodiversity data
taxaCount_NMDS <- LPP_FullData %>%
  ungroup() %>%
  select(MidgeFlies:RightHandedSnails) %>%
vegan::metaMDS(.,distance = "bray", k = 4, plot = T)

