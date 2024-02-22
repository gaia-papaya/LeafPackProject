#dependencies----
require(coin)

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
wq_norms <- list(P = shapiro.test(sqrt(water_quality$P_mean)),
                 Nitrogen = shapiro.test(sqrt(water_quality$Nitrogen_mean)),
                 pH = shapiro.test(water_quality$pH_mean),
                 NH3 = shapiro.test(water_quality$NH3_mean),
                 DO = shapiro.test(water_quality$DO_mean),
                 Temp = shapiro.test(water_quality$Temp_mean),
                 Cond = shapiro.test(1/water_quality$Cond_mean),
                 FLow = shapiro.test(water_quality$Flow_mean))


#ANOVA for parameters which pass shapiro test
#FORMULA: PARAMETER ~ DATE + SITE
#wq model objects
wq_normMod_objects <- list(
  sqrt_P = aov(sqrt(P_mean)~site+date, data = water_quality),
  sqrt_Nitrogen = aov(sqrt(Nitrogen_mean)~site+date, data = water_quality),
  pH = aov(pH_mean~site+date, data = water_quality),
  Temp = aov(Temp_mean~site+date, data =  water_quality),
  Flow = aov(Flow_mean~site+date, data =  water_quality)
)

#T2 anova
wq_ANOVAt2 <- wq_normMod_objects %>%
  {list(
    sqrt_P = car::Anova(.$sqrt_P),
    sqrt_Nitrogen = car::Anova(.$sqrt_Nitrogen),
  pH = car::Anova(.$pH),
  Temp = car::Anova(.$Temp),
  Flow = car::Anova(.$Flow))
  }

#independence test for non-normal params
wq_kw_tests_site <-list(
   NH3 =  kruskal.test(data =water_quality, NH3_mean ~ as.factor(site)  ),
  DO =  kruskal.test(data = water_quality, DO_mean ~ as.factor(site)  ),
  Cond =  kruskal.test(data = water_quality, Cond_mean ~ as.factor(site)  )
)

wq_kw_tests_date <-list(
  NH3 =  kruskal.test(data =water_quality, NH3_mean ~ as.factor( date)  ),
  DO =  kruskal.test(data = water_quality, DO_mean ~ as.factor( date)  ),
  Cond =  kruskal.test(data = water_quality, Cond_mean ~ as.factor( date)  )
)



#SPECIES COUNTS NMDS----
#set seed for repeatability
set.seed(420)

#Nonmetric Multidimensional Scaling (NMDS) of Biodiversity data
taxaCount_NMDS <- LPP_FullData %>%
  ungroup() %>%
  select(MidgeFlies:RightHandedSnails) %>%
vegan::metaMDS(.,distance = "bray", k = 5, plot = T)

