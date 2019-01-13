# Hot encoding

# Load Data 
feats <- read.csv('./data/features.csv')
colSums(is.na(feats))
# remove pokemons that never won
feats2 <- feats[-(which(is.na(feats$win_ratio))),]
dim(feats2)
colnames(feats2)
# for the hot encoding remove the pokemon name and Id column
feats2 <- feats2[,c(4:18)]; head(feats2); 
dim(feats2)

# select categorical and numerical features
feature_classes <- sapply(names(feats2),function(x){class(feats[[x]])})
feature_classes
numeric_feats <-names(feats2[feature_classes != "character" &
                      feature_classes != "factor"])
numeric_feats

# get names of categorical features
categorical_feats <- names(feats2[feature_classes == "character" | 
                            feature_classes == "factor"])

categorical_feats
# use caret dummyVars function for hot one encoding for categorical features
dummies <- dummyVars(~.,feats2[categorical_feats])
categorical_1_hot <- predict(dummies,feats2[categorical_feats])
categorical_1_hot[is.na(categorical_1_hot)] <- 0  #for any level that was NA, set to zero

head(dummies)
head(categorical_1_hot)

# join final dataset for modeling
final_data <- cbind(feats2[numeric_feats],categorical_1_hot)
# save file
write.csv(final_data, file="./data/Model_data.csv")
