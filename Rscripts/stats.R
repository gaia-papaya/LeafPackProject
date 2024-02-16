#join diversity data and water quality data together
LPP_FullData <- right_join(x = wq_list$summ_water_quality, y = bd_list$biodiversity, by = c("date", "site"), multiple = "all")

#DIVERSITY MODELS----
#FORMULA: INDEX ~ MATERIAL + DATE
diversity_mod_objects <- list(#taxa wide anova model objects
  shannon = aov(shannon ~ material + as.factor(date) , data = LPP_FullData),
  simpson = aov(simpson ~ material + as.factor(date) , data = LPP_FullData),
  invsimp = aov(simpson ~ material + as.factor(date) , data = LPP_FullData),
  even = aov(even ~ material + as.factor(date) , data = LPP_FullData),
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
        even = car::Anova(.$even),
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
#normality tests
wq_norms <- list(P = shapiro.test(wq_list$summ_water_quality$P),
                 Nitrogen = shapiro.test(wq_list$summ_water_quality$Nitrogen),
                 pH = shapiro.test(wq_list$summ_water_quality$pH),
                 NH3 = shapiro.test(wq_list$summ_water_quality$NH3),
                 DO = shapiro.test(wq_list$summ_water_quality$DO),
                 Temp = shapiro.test(wq_list$summ_water_quality$Temp),
                 Cond = shapiro.test(wq_list$summ_water_quality$Cond),
                 FLow = shapiro.test(wq_list$summ_water_quality$Flow))


#ANOVA for parameters which pass shapiro test
#FORMULA: PARAMETER ~ DATE + SITE
#wq model objects
wq_normMod_objects <- list(
  pH = aov(pH~site+date, data = wq_list$summ_water_quality),
  Temp = aov(Temp~site+date, data =  wq_list$summ_water_quality),
  Flow = aov(Flow~site+date, data =  wq_list$summ_water_quality)
)

#T2 anova
wq_ANOVAt2 <- wq_normMod_objects %>%
  {list(
  pH = car::Anova(.$pH),
  Temp = car::Anova(.$Temp),
  Flow = car::Anova(.$Flow))
  }

#independence test for non-normal params
wq_indep_tests <-list(
  P = independence_test(data = wq_list$summ_water_quality, P~ as.factor(site) | as.factor(date)),
  Nitrogen = independence_test(data = wq_list$summ_water_quality, Nitrogen ~ as.factor(site) | as.factor(date)),
   NH3 = independence_test(data = wq_list$summ_water_quality, NH3 ~ as.factor(site) | as.factor(date)),
  DO = independence_test(data = wq_list$summ_water_quality, DO ~ as.factor(site) | as.factor(date)),
  Cond = independence_test(data = wq_list$summ_water_quality, Cond ~ as.factor(site) | as.factor(date))
)

independence_test(Cond ~ as.factor(site) | as.factor(date), data = wq_list$summ_water_quality)

#SPECIES COUNTS NMDS----
#set seed for repeatability
set.seed(420)

#Nonmetric Multidimensional Scaling (NMDS) of Biodiversity data
taxaCount_NMDS <- LPP_FullData %>%
  ungroup() %>%
  select(MidgeFlies:RightHandedSnails) %>%
vegan::metaMDS(.,distance = "bray", k = 5, plot = T)

