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
#install.packages('tidyverse')
#library(tidyverse)
library(ggplot2)

# Load data
setwd("~/Documents/Pokemon-winner-prediction")
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
head(names)







