#join diversity data and water quality data together
joined <- right_join(x = wq_list$summ_water_quality, y = bd_list$biodiversity, by = c("date", "site"), multiple = "all")

#statistical analyses----
#DIVERSITY
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

#if the p values are below 95% confidence threshhold, perform tukey tests on them
diversity_tukeys <- list()
for(t in 1:length(diversity_tests)){
  if(diversity_tests[[t]]){}
  diversity_tukeys[[t]] <- agricolae::HSD.test(diversity_mod_objects[[t]], "material", group = T)
  names(diversity_tukeys)[t] <- names(diversity_tests)[[t]]
  
}