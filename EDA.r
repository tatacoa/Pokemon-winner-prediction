#ML2 Project
# Code for descriptive analysis of the data

# Import Libraries
#Libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
#install.packages('corrplot')
library(corrplot)
#install.packages('caret')
library(caret)
#install.packages('ggthemes')
library(ggthemes)
library(RColorBrewer)
#install.packages('fmsb')
library(fmsb) # radarchart
#install.packages('rpart.plot')
library(rpart.plot)
#install.packages('ROCR')
library(ROCR)
#install.packages('tidyverse')
#library(tidyverse)

##### Data Load #####
# Load data
setwd("Pokemon-winner-prediction")
pokemon <- read.csv('data/pokemon.csv')
fights <- read.csv('./data/combats.csv')

##### EXPLORATY DATA ANALYSIS #####
# data exploration for Pokemon Data
dim(pokemon)
names(pokemon)
head(pokemon)
colSums(is.na(pokemon)) # no missing values
summary(pokemon)

# data exploration for Fights Data
dim(fights)
names(fights)
head(pokemon)
colSums(is.na(fights))
wins <- group_by(fights, Winner)
summarise(wins, count = n())

# visualization
# feature correlation
featCorr <- cor(select(pokemon, HP, Attack, Defense, Sp..Atk, Sp..Def, Speed )) # correlation table for features
corrplot(featCorr, method = "color", type = "upper", addCoef.col = "black", tl.col = "black", diag = FALSE)

# pokemon type distribution
par(las=2)
barplot(table(pokemon$Type.1), 
        col = heat.colors(length(unique(pokemon$Type.1))), 
        main = 'Type 1 Pokemon')
barplot(table(pokemon$Type.2), 
        col = heat.colors(length(unique(pokemon$Type.2))), 
        main = 'Type 2 Pokemon')
barplot(table(pokemon$Generation), 
        col = heat.colors(length(unique(pokemon$Generation))),
        main = 'Generation Pokemon')

# plot specific feature distribution by Pokemon type
boxplot(HP ~ Type.1,
        data=pokemon,
        main="Heat points Range by Pokemon Type",
        xlab="Type 1",
        ylab="Health points",
        col="grey",
        border="brown"
)

boxplot(Attack ~ Type.1,
        data=pokemon,
        main="Attack Range by Pokemon Type",
        xlab="Type 1",
        ylab="Attack",
        col="red",
        border="brown"
)

boxplot(Defense ~ Type.1,
        data=pokemon,
        main="Defense Range by Pokemon Type",
        xlab="Type 1",
        ylab="Defense",
        col="orange",
        border="brown"
)

# set color for each pokemon type for plotting // hex codes from http://www.epidemicjohto.com/t882-type-colors-hex-colors
color<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130","#F7D02C","#705746","#735797","#E2BF65","#96D9D6","#6390F0","#7AC74C","#C22E28","#D685AD","#A33EA1","#A8A77A","#A6B91A")

# pokemon characteristics
#res<-data.frame(pokemon %>% dplyr::select(Type.1,HP, Attack, Defense, Sp..Atk, Sp..Def, Speed) %>% dplyr::group_by(Type.1) %>% dplyr::summarise_all(funs(mean)) %>% mutate(sumChars = HP + Attack + Defense + Sp..Atk + Sp..Def + Speed) %>% arrange(-sumChars))
res <- select(pokemon, Type.1, HP, Attack, Defense, Sp..Atk, Sp..Def, Speed) # select particular features
res <- group_by(res, Type.1) # group by pokemon type
res <- summarise_all(res, funs(mean)) # get mean values for the types
res <- mutate(res, sumChars = HP + Attack + Defense + Sp..Atk + Sp..Def + Speed) # sum up all mean values
res <- arrange(res, -sumChars) # sort for values, descending
res$color<-color # apply color scheme
max<- ceiling(apply(res[,2:7], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,6)

par(mfrow=c(3,6))
par(mar=c(1,1,1,1))
for(i in 1:nrow(res)){
  curCol<-(col2rgb(as.character(res$color[i]))%>% as.integer())/255 # convert to rgb
  radarchart(rbind(max,min,res[i,2:7]),
             axistype=2 , 
             pcol=rgb(curCol[1],curCol[2],curCol[3], alpha = 1) ,
             pfcol=rgb(curCol[1],curCol[2],curCol[3],.5) ,
             plwd=2 , cglcol="grey", cglty=1, 
             axislabcol="black", caxislabels=seq(0,2000,5), cglwd=0.8, vlcex=0.8,
             title=as.character(res$Type.1[i]))
}
