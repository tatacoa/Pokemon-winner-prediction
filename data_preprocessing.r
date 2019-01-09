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
library(fmsb)
#install.packages('rpart.plot')
library(rpart.plot)
#install.packages('ROCR')
library(ROCR)
library(tidyverse)
library(ggplot2)

# Load data
setwd("/Users/yunussh/playground/Pokemon-winner-prediction")
pokemon <- read.csv('./pokemon.csv')
fights <- read.csv('./combats.csv')

#EDV PLots
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
                              

boxplot(Defense ~ Type.1, data = pokemon, 
        col = rainbow(length(unique(pokemon$Type.1))))
boxplot(Defense ~ Type.1,
        data=pokemon,
        main="Defense Range by Pokemon Type",
        xlab="Type 1",
        ylab="Defense",
        col="orange",
        border="brown"
)
                                  
#nullvalues
colSums(is.na(pokemon))
colSums(is.na(fights))

dim(pokemon)
head(pokemon)
colnames(pokemon)

dim(fights)
head(fights)
colnames(fights)

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
