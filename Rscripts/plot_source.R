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



#BRAY CURTIS
#trying to read in leaf pack paper from google sheet but downloaded as a csv to more quickly obtain a qorkable data table for r analysis



getwd()
setwd("/Users/reb/Desktop/ARE/LeafPackProject/Rdata")
lpp<-read.csv("LPP.csv")
#it worked but i have to put it in quotes

#going to filter the lpp data frame so that it only has value from downstream and upstream

#DU<-

?mutate

#creating data frame of lpp that will be mutated and chopped up
Dstream<-lpp[lpp$site=="D", ]
#this is all the data for sites with downstream trait

Ustream<-lpp[lpp$site=="U",]
#for upstream

Mstream<-lpp[lpp$site=="M",]

#so i have them seperated so now i can try to do the stat i want which is... i guess bray curtis but i need to have each table converted to be a sum of each organism so maybe i need to do column sums?


#dropping the non numeric columns that I dont need

MS<-subset(Mstream, select = -c(material,date ,bag_quality, site ,n_tol_taxa,n_ss_taxa, richness, n_s_taxa))

US<-subset(Ustream, select = -c(material,date ,bag_quality, site ,n_tol_taxa,n_ss_taxa, richness, n_s_taxa))

DS<-subset(Dstream, select = -c(material,date ,bag_quality, site ,n_tol_taxa,n_ss_taxa, richness, n_s_taxa))


#creating new data frame with the species totals for each column 
#proboem is i think i need to pivot the table so that the species names are columns instead of rows
DSUMS<-as.data.frame(colSums(DS))

#this switches the columns and rows
DSUMS2<-as.data.frame(t(DSUMS))


#now for midstream

MSUMS<-as.data.frame(colSums(MS))

MSUMS2<-as.data.frame(t(MSUMS))


#now for upstream

USUMS<-as.data.frame(colSums(US))

USUMS2<-as.data.frame(t(USUMS))

#Now maybe i should combine them again into a superset? or maybe i can break them down for their comparisons during the bray curtis

Superset<-rbind(USUMS2, MSUMS2,DSUMS2)

#First will be the Upstream Midstream comparison

UMSET<-rbind(USUMS2,MSUMS2)

#next will be upstream and downstream
UDSET<-rbind(USUMS2, DSUMS2)

#next will be midstream and downstream set
MDSET<-rbind(MSUMS2,DSUMS2)


#this does the bray curtis score for this comparison of Up and midstream
bcUM<-sum(apply(UMSET,2, function(x) abs(max(x)-min(x))))/sum(rowSums(UMSET))

#this will do the bray curtis score for upstream and downstream

bcUD<-sum(apply(UDSET,2, function(x)abs(max(x)-min(x))))/sum(rowSums(UDSET))

#this will do the bray curtis score for midtsream and downstream
bcMD<-sum(apply(MDSET,2, function(x)abs(max(x)-min(x))))/sum(rowSums(MDSET))

#here are my results
#now to interpret

BRAYCURTIS<-as.data.frame(rbind(bcUM,bcUD,bcMD))
#the scale is from 0 to 1 where a zero means the 2 sites are basically the same
#this can be seen as a percent so a bcd score of .09 is 9% 
#to get similarity subtract 1-x to get the similarity 
#so a bcd of .09 is a bcs of 91%


BRAYCURTIS






#if we treat each bag type as a seperate site
#we can do bray curtis like above to see similarity between our best preforming bag types
#lets give it a shot
#creating data frame of lpp that will be mutated and chopped up
Plastic<-lpp[lpp$material=="Plastic", ]
#this is all the data for sites with plastic

Biop<-lpp[lpp$material=="Biopolymer",]
#for biopolymer

Jute<-lpp[lpp$material=="Jute",]
#for jute

#for cotton

Cotton<-lpp[lpp$material=="Cotton",]



#for cellulos
Cellulose<-lpp[lpp$material=="Cellulose ",]


#now to filter out the shit i dont need
Pl<-subset(Plastic, select = -c(material,date ,bag_quality, site ,n_tol_taxa,n_ss_taxa, richness, n_s_taxa))

B<-subset(Biop, select = -c(material,date ,bag_quality, site ,n_tol_taxa,n_ss_taxa, richness, n_s_taxa))

J<-subset(Jute, select = -c(material,date ,bag_quality, site ,n_tol_taxa,n_ss_taxa, richness, n_s_taxa))


Cot<-subset(Cotton, select = -c(material,date ,bag_quality, site ,n_tol_taxa,n_ss_taxa, richness, n_s_taxa))

Cell<-subset(Cellulose, select = -c(material,date ,bag_quality, site ,n_tol_taxa,n_ss_taxa, richness, n_s_taxa))








#now to do the sums

#PLASTIC
#creating new data frame with the species totals for each column 
#proboem is i think i need to pivot the table so that the species names are columns instead of rows
PLSUMS<-as.data.frame(colSums(Pl))

#this switches the columns and rows
PL2<-as.data.frame(t(PLSUMS))


#BIOPOLYMER
BIOSUMS<-as.data.frame(colSums(B))

B2<-as.data.frame(t(BIOSUMS))



#JUTE
JSUMS<-as.data.frame(colSums(J))

J2<-as.data.frame(t(JSUMS))



#Cotton
COTSUMS<-as.data.frame(colSums(Cot))

Cot2<-as.data.frame(t(COTSUMS))



#Cellulose

CELLSUMS<-as.data.frame(colSums(Cell))

CELL2<-as.data.frame(t(CELLSUMS))








#NOW FOR THE COMPARISONS using rbind
#need

#Plastic Bio
PB<-rbind(PL2,B2)

#Plastic Jute
PJ<-rbind(PL2,J2)

#Plastic Cell
PCELL<-rbind(PL2,CELL2)

#Plastic Cotton
PCOT<-rbind(PL2, Cot2)







#now for brayc curtis!!!!!

#PLASTIC + BIOPOLYMER
bcPB<-sum(apply(PB,2, function(x) abs(max(x)-min(x))))/sum(rowSums(PB))


#PLASTIC +JUTE
bcPJ<-sum(apply(PJ,2, function(x) abs(max(x)-min(x))))/sum(rowSums(PJ))




#Plastic+Cellulose

bcPCELL<-sum(apply(PCELL,2, function(x) abs(max(x)-min(x))))/sum(rowSums(PCELL))

#PLASTIC+COTTON

bcPCOT<-sum(apply(PCOT,2, function(x) abs(max(x)-min(x))))/sum(rowSums(PCOT))




BRAYCURTIS2<-as.data.frame(rbind(bcPB,bcPJ,bcPCELL,bcPCOT))
BRAYCURTIS2
```

```{r}

bcPB
0.1369451
bcPJ
0.3158981
bcPCELL
0.5930298
bcPCOT
0.3437648


T<-BRAYCURTIS2
str(T)

TT<-setNames(cbind(rownames(T), T, row.names = NULL), 
             c("Comparison", "BC_Score"))




plot<-ggplot(TT, aes(x=TT$Comparison, y=TT$BC_Score)) + 
  geom_bar(stat = "identity")+
  labs(y="Bray Curtis Dissimilarity Score", x="Comparison", title="Plot of Bray Curtis Dissimilarity Scores by Comparison Type")

plot+ ylim(0,1)
```


