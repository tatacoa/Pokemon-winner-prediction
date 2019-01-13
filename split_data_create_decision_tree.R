# Load Libraries
library(rpart.plot)
library(dplyr)
library(caret)

# Set path
setwd("~/test_tree")

# Read in
features <- read.csv('data/features/features.csv')

# some pokemon never fought
colSums(is.na(features))

# remove pokemons that never won
feats_clean <- na.omit(features)

# convert category legendary into numerical values
feats_clean$Legendary <- as.integer(as.logical(feats_clean$Legendary))

# drop unnecessary generation column
feats_clean <- select(feats_clean, -c(Generation, win_counts, losses))

#split train test data
trainIndex = createDataPartition(feats_clean$win_ratio,
                                 p=0.6, list=FALSE, times=1)

# 60% training data
train = feats_clean[trainIndex,]
test_val = feats_clean[-trainIndex,]

trainIndex = createDataPartition(test_val$win_ratio,
                                 p=0.5, list=FALSE, times=1)
# 20% validation data
val = test_val[trainIndex,]

# 20% test data
test = test_val[-trainIndex,]

# create Target variable and the train data
y_train <- train$win_ratio
x_train <- train[6:13]

# create Target variable and the validation data
y_val <- val$win_ratio
x_val <- val[6:13]

# create Target variable and the test data
y_test <- test$win_ratio
x_test <- test[6:13]

# Create Function to calculate Root Mean Square Error (RMSE)
rmse <- function(actual, predicted)
{
  error <- actual - predicted
  sqrt(mean(error^2))
}

# Train the decision tree with y_train as target and all the columns from x_train
# Because we are using continious variable as the target value, we have to use a regression tree = anova
tree <- rpart(y_train ~ . , data = x_train, method='anova')
rpart.plot(tree, type=3, digits=3, fallen.leaves = TRUE)

# Find the best cp to prune the tree
tree$cptable
plotcp(tree)

# Prune the tree to get a optimal decision tree
tree_pruned <- prune(tree,cp=0.039)
rsq.rpart(tree_pruned)

#Plot the final decision tree
rpart.plot(tree_pruned, type=3, fallen.leaves = TRUE)

# Predict the 20% validation data to compare accuracy with the other model
p1<- predict(tree, x_val)
rmse(y_val, p1)

# Predict the 20% test data to compare accuracy with the other model
p2<- predict(tree, x_test)
rmse(y_test, p2)