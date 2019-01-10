#ML2 Project
# @Autor: Ana Maria Sandoval 


#Kaggle
# https://www.kaggle.com/jonathanbouchet/pokemon-battles/data
# https://www.kaggle.com/nayansolanki2411/world-of-pokemon
# https://www.kaggle.com/mmetter/pokemon-data-analysis-tutorial
# https://www.kaggle.com/rautaki0127/pokemon-data-science-challenge

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
library(ggplot2)

##### DATA IMPORT #####
# Load data
setwd("Pokemon-winner-prediction")
pokemon <- read.csv('data/pokemon.csv')
fights <- read.csv('data/combats.csv')

##### EXPLORATY DATA ANALYSIS #####
# data exploration
dim(pokemon)
names(pokemon)
head(pokemon)
colSums(is.na(pokemon)) # no missing values
summary(pokemon)

dim(fights)
names(fights)
head(pokemon)
colSums(is.na(fights))
wins <- group_by(fights, Winner)
summarise(wins, count = n())

# visualization
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
        main="Health points Range by Pokemon Type",
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

# set color for each pokemon type for plotting
color<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130","#F7D02C","#705746","#735797","#E2BF65","#96D9D6","#6390F0","#7AC74C","#C22E28","#D685AD","#A33EA1","#A8A77A","#A6B91A")

# pokemon characteristics
res<-data.frame(pokemon %>% dplyr::select(Type.1,HP, Attack, Defense, Sp..Atk, Sp..Def, Speed) %>% dplyr::group_by(Type.1) %>% dplyr::summarise_all(funs(mean)) %>% mutate(sumChars = HP + Attack + Defense + Sp..Atk + Sp..Def + Speed) %>% arrange(-sumChars))
res$color<-color
max<- ceiling(apply(res[,2:7], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,6)

par(mfrow=c(3,6))
par(mar=c(1,1,1,1))
for(i in 1:nrow(res)){
  curCol<-(col2rgb(as.character(res$color[i]))%>% as.integer())/255
  radarchart(rbind(max,min,res[i,2:7]),
             axistype=2 , 
             pcol=rgb(curCol[1],curCol[2],curCol[3], alpha = 1) ,
             pfcol=rgb(curCol[1],curCol[2],curCol[3],.5) ,
             plwd=2 , cglcol="grey", cglty=1, 
             axislabcol="black", caxislabels=seq(0,2000,5), cglwd=0.8, vlcex=0.8,
             title=as.character(res$Type.1[i]))
}
names <- pokemon[,c(1,2)]; head(names) # join id with pokemon name
colnames(names)
head(pokemon)

##### DATA MANIPULATION #####

names <- pokemon[,c(1,2)]; head(names) # join id with pokemon name
colnames(names)
head(pokemon)

#Map the figths table from id to pokemon name
fights.name <- data.frame(lapply(fights, function(x) names$Name[match(x,names$X.)]))
head(fights.name)

get_win_table <- function() {
  counts <- group_by(fights.name, Winner)
  count_table <- summarise(counts, count = n())
  return(count_table)
}

get_firsts_table <- function() {
  counts <- group_by(fights.name, First_pokemon)
  count_table <- summarise(counts, count = n())
  return(count_table)
}

get_seconds_table <- function() {
  counts <- group_by(fights.name, Second_pokemon)
  count_table <- summarise(counts, count = n())
  return(count_table)
}

win_table <- get_win_table()
firsts_table <- get_firsts_table()
seconds_table <- get_seconds_table()

win_counts <- sapply(pokemon$Name, function(x) win_table$count[match(x,win_table$Winner)])
first_counts <- sapply(pokemon$Name, function(x) firsts_table$count[match(x,firsts_table$First_pokemon)])
second_counts <- sapply(pokemon$Name, function(x) seconds_table$count[match(x,seconds_table$Second_pokemon)])

pokemon_feats <- cbind(pokemon, win_counts, first_counts, second_counts)
pokemon_feats$losses <- pokemon_feats$first_counts + pokemon_feats$second_counts - pokemon_feats$win_counts

pokemon_feats$win_ratio <- pokemon_feats$win_counts / (pokemon_feats$second_counts + pokemon_feats$first_counts)
head(pokemon_feats)

write.csv(pokemon_feats, file="./features.csv")

for (i in 1:nrow(pokemon_feats)) {
 i_name <- pokemon_feats[i,2]
 
 for (j in 1:nrow(names)) {
   j_name <- names[j,2]
   
   matches <- fights.name[(fights.name$First_pokemon == i_name & fights.name$Second_pokemon == j_name) |
              (fights.name$First_pokemon == j_name & fights.name$Second_pokemon == i_name), ]
   w <- nrow(matches[matches$Winner == i_name,])
   l <- nrow(matches) - w
   
   pokemon_feats[i, paste("wins", j, sep = "_")] <- w
   pokemon_feats[i, paste("losses", j, sep = "_")] <- l
 }
}

write.csv(pokemon_feats, file="./features_with_fight_pairs.csv")
