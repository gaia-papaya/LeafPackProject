#dependencies----
require(car)
require(tidyverse)

#source data processor scripts----
source("Rscripts/biodiversity.R")#script to process biodiversity data
source("Rscripts/water_quality.R")#script to process water quality data
source("Rscripts/plot_source.R") #source code for custom plotting functions

#join together summ_water_quality and biodiversity data
joined <- full_join(x = wq_list$summ_water_quality, y = bd_list$biodiversity, by = c("date", "site"), multiple = "all")

#statistical analyzes:
#diversity model object list 
#FORMULA: INDEX ~ MATERIAL + DATE
diversity_mod_objects <- list(#taxa wide anova model objects
                              shannon = aov(shannon ~ material + as.factor(date) , data = joined),
                              simpson = aov(simpson ~ material + as.factor(date) , data = joined),
                              invsimp = aov(simpson ~ material + as.factor(date) , data = joined),
                              evenness = aov(even ~ material + as.factor(date) , data = joined),
                              richness = aov(richness ~ material + as.factor(date), data = joined),
                              #functional group anova model objects
                              coll_shannon = aov(collector_shannon ~ material + as.factor(date)   , data = joined),
                              scr_shannon = aov(shredder_shannon ~ material + as.factor(date)   , data = joined),
                              pre_shannon = aov(predator_shannon ~ material + as.factor(date)   , data = joined),
                              shr_shannon = aov(shredder_shannon ~ material + as.factor(date)   , data = joined))
                
#Type two anova list using car::Anova function 
diversity_tests <- diversity_mod_objects %>%
  {list(shannon = Anova(.$shannon),
        simpson = Anova(.$simpson),
        invsimp = Anova(.$invsimp),
        eveneness = Anova(.$evenness),
        richness = Anova(.$richness),
        coll_shannon = Anova(.$coll_shannon),
        scr_shannon = Anova(.$scr_shannon),
        pre_shannon = Anova(.$pre_shannon),
        shr_shannon = Anova(.$shr_shannon))}

#if the p values are below 95% confidence threshhold, perform tukey tests on them
diversity_tukeys <- list()
for(t in 1:length(diversity_tests)){
  if(any(diversity_tests[[t]]$`Pr(>F)` < 0.05, na.rm = T) == TRUE)
  {cat(names(diversity_tests[t]), "has P values below 0.05 threshhold, conducting tukey test", sep = " ")
    diversity_tukeys[[t]] <- TukeyHSD(diversity_mod_objects[[t]])
    names(diversity_tukeys)[t] <- names(diversity_tests)[[t]]
  }
  #remove NA elements of tukey list
  diversity_tukeys <- compact(diversity_tukeys)
}

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

#tests 
wq_tests <- list(
  P = Anova(wq_mod_objects$P),
  pH = Anova(wq_mod_objects$pH),
  NH3 = Anova(wq_mod_objects$NH3),
  DO = Anova(wq_mod_objects$DO),
  Temp = Anova(wq_mod_objects$Temp),
  Cond = Anova(wq_mod_objects$Cond),
  Flow = Anova(wq_date_mod_objects$Flow)
)

#tukeys
wq_tukeys <- list()
for(t in 1:length(wq_tests)){
  if(any(wq_tests[[t]]$`Pr(>F)` < 0.05, na.rm = T) == TRUE)
  {cat(names(wq_tests[t]), "has P values below 0.05 threshhold, conducting tukey test", sep = " ")
    wq_tukeys[[t]] <- TukeyHSD(wq_mod_objects[[t]])
    names(wq_tukeys)[t] <- names(wq_tests)[[t]]
  }
  #remove NA elements of tukey list
  wq_date_tukeys <- compact(wq_tukeys)
}

#correlation testing
cor.test(joined$bag_quality, joined$richness)

#eda----
#cummul richness bag type barplots
bt_barplot( "shannon")+ labs(x = "Material", y= "Shannon Diversity")
bt_barplot( "simpson")+ labs(x = "Material",y= "Simpson Diversity")
bt_barplot( "invsimp")+ labs(x = "Material",y= "Inverse-Simpson Diversity")
bt_barplot( "richness")+ labs(x = "Material", y="Richness")
bt_barplot("even")+ labs(x = "Material",y= "Shannon Evenness")

#water quality by date on the y and site as color
wq_ts_BySite("P")
wq_ts_BySite("pH")
wq_ts_BySite("NH3")
wq_ts_BySite("DO")
wq_ts_BySite("Temp")
wq_ts_BySite("Cond")
wq_ts_BySite("Nitrogen")
wq_ts_BySite("Flow") + labs(title = "Flow rate seasonality",
                            x = "Date", y = "Flow (m/s)")

#correl plot

ggplot(filter(joined, is.na(material) == FALSE), aes(x = bag_quality*5, y = richness)) +
  geom_point(aes(color = material)) + geom_smooth(method = "lm", se = F, color = 'black')+
  theme_bw() + theme_rafa(base_size = 15) +
  labs(x="Bag Quality Score", y="Richness",
       title="Correlation Between Richness and Bag Quality Score ",
     caption =  "Pearson's r: 0.4212087 (P <0.01)")

#output csv
wq_list$site_summ_wq %>%
arrange(desc(site)) %>%
write_csv( file = "Rdata/wq_summary_by site.csv")

