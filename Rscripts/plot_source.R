#README #README #README #README #README #README #README #README #README:
#Hello!  this is the source code used to create necessary functions for bag data analysis. in order for
#the bag_project.R script to work, you must source this file first and foremost
#DEPENDENCIES----
require(vegan)
require(ggpubr)
require(tidyverse)
require(googlesheets4)
require(viridisLite)
require(rlang)
require(car)

#STATISTICS----



#PLOTTING----
#set plotting theme function 
theme_rafa <- function(base_size){
  theme_pubr(base_family="sans") %+replace%
    theme(panel.background  = element_blank(),
          plot.background = element_rect(fill="transparent", colour=NA), 
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key = element_rect(fill="transparent", colour=NA),
          legend.text = element_text(size=base_size * 0.7),
          legend.title = element_text(size = base_size * 0.8, 
                                      face = "bold"),
          axis.text = element_text(size = base_size*0.85),
          axis.title = element_text(size = base_size * 1.2),
          plot.title.position = "panel",
          plot.caption.position = "panel",
          plot.title = element_text(
            size = base_size * 1.25,
            face = "bold", 
            hjust = 0.5,
            vjust = 1),
          plot.subtitle = element_text(
            size = base_size * 0.9,
            vjust = 1),
          plot.caption =  element_text(
            size = base_size*0.75,
            hjust = 0.5)
    )
}

#bag type barplot function:
#PARAMS: 
#index(string): a string specifying the desired index
bt_barplot <- function(index){
  #calculate mean and standard error of chosen index
  new_tbl <- joined%>%
  filter(is.na(material) == FALSE)%>% #remove na values from material before calculations
    group_by(material) %>% #group by material
    mutate(index_se = sd(get(index), na.rm = T)/sqrt(n())) %>% # calc SE
    summarise(index = mean(get(index), na.rm = T), index_se = mean(index_se)) #summarise into mean
    
  ggplot(new_tbl, aes(x = reorder(material, -index, sum), y = index)) +
    geom_col(aes(fill = reorder(material, -index, sum))) +
    geom_errorbar(aes(ymin = index - 2*index_se, ymax = index + 2*index_se)) +
    
    theme_bw() +
    scale_fill_viridis_d(begin = 0.1, end = 0.9)+
    theme_rafa(base_size = 15) +
    theme(axis.text.x = element_text(angle = -15, vjust = 0.5))+
    labs(x = "material", title = "Diversity Index by Bag material", y = paste(index),
         fill = "material")
}

#water quality time series plots by site
#PARAMS:
#param(string): a string specifying the water quality parameter
wq_ts_BySite <- function(param){
  #create symbol of parameter name and se for tidy eval in ggplot2 call
  param<- sym(param)
  param_se <- sym(paste(param,"se", sep = "_"))
  ggplot(joined, aes(x = date, y = !!param, color = site)) +
    #geom layers
    geom_point() +
    geom_errorbar(aes(ymin = !!param - 2*!!param_se, ymax = !!param + 2*!!param_se)) + 
    #theme adjustments
    theme_bw() +
    theme_rafa(base_size = 15) +
    scale_color_viridis_d(begin = 0.1, end = 0.9)
}
