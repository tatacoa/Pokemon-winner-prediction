#ML2 Project
# @Autor: Ana Maria Sandoval 


#Kaggle
# https://www.kaggle.com/jonathanbouchet/pokemon-battles/data
# https://www.kaggle.com/nayansolanki2411/world-of-pokemon
# https://www.kaggle.com/mmetter/pokemon-data-analysis-tutorial
# https://www.kaggle.com/rautaki0127/pokemon-data-science-challenge


##### DATA MANIPULATION #####
levels(pokemon$Name)[levels(pokemon$Name)==""] <- "Primeape" # pokemon name was not defined
#pokemon[63,2] <- factor(63, labels = "Primeape")
names <- pokemon[,c(1,2)]; head(names) # join id with pokemon name
colSums(is.na(names))
colnames(names)
head(pokemon)

#Map the figths table from id to pokemon name
fights.name <- data.frame(lapply(fights, function(x) names$Name[match(x,names$X.)]))
head(fights.name)
sapply(fights.name, function(x) length(unique(x))) # only 784 of 800 pokemon fought

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
win_table$Winner
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
