#ML2 Project
# Data Pre-processing

# set working directory
setwd("~/Documents/Pokemon-winner-prediction")
#Load data
pokemon <- read.csv('./data/pokemon.csv')
fights <- read.csv('./data/combats.csv')

##### DATA MANIPULATION #####
# add missing Pokemon
levels(pokemon$Name)[levels(pokemon$Name)==""] <- "Primeape" # pokemon name was not defined

#create an object with the id and pokemon name
names <- pokemon[,c(1,2)]; head(names) # join id with pokemon name
colSums(is.na(names))
colnames(names)
head(names)

#Map the figths table from id to pokemon name
fights.name <- data.frame(lapply(fights, function(x) names$Name[match(x,names$X.)]))
head(fights.name)
sapply(fights.name, function(x) length(unique(x))) # only 784 of 800 pokemon fought

# Function to get winner table
get_win_table <- function() {
  # group by column Winner
  counts <- group_by(fights.name, Winner)
  count_table <- summarise(counts, count = n())
  return(count_table)
}

# Function to get First_pokemon table
get_firsts_table <- function() {
  # group by column First_pokemon
  counts <- group_by(fights.name, First_pokemon)
  count_table <- summarise(counts, count = n())
  return(count_table)
}

# Function to get Second_pokemon table
get_seconds_table <- function() {
  # group by column Second_pokemon
  counts <- group_by(fights.name, Second_pokemon)
  count_table <- summarise(counts, count = n())
  return(count_table)
}

# Apply the functions 
win_table <- get_win_table()
firsts_table <- get_firsts_table()
seconds_table <- get_seconds_table()

# add combat data with pokemon data
win_counts <- sapply(pokemon$Name, function(x) win_table$count[match(x,win_table$Winner)])
first_counts <- sapply(pokemon$Name, function(x) firsts_table$count[match(x,firsts_table$First_pokemon)])
second_counts <- sapply(pokemon$Name, function(x) seconds_table$count[match(x,seconds_table$Second_pokemon)])

# add new columns to the pokemon dataset
pokemon_feats <- cbind(pokemon, win_counts, first_counts, second_counts)
# compute pokemon losses
pokemon_feats$losses <- pokemon_feats$first_counts + pokemon_feats$second_counts - pokemon_feats$win_counts
# compute pokemon win_ratio
pokemon_feats$win_ratio <- pokemon_feats$win_counts / (pokemon_feats$second_counts + pokemon_feats$first_counts)
head(pokemon_feats)

# save pre-processed file
write.csv(pokemon_feats, file="./features.csv")