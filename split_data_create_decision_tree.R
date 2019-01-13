# Load Libraries
library(rpart.plot)
library(dplyr)
library(caret)

# Read in
features <- read.csv('data/features.csv')

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
fit <- rpart(y_train ~ . , data = x_train, method='anova')

#Plot the regression tree
rpart.plot(fit, type=3, digits=3, fallen.leaves = TRUE)

#Predict the validation data
p1<- predict(fit, x_val)

#Calculate the RMSE for the prediction and the validation data
rmse(y_val, p1)

# Predict the 20% test data to compare accuracy with the other model
p1<- predict(fit, x_test)

#Calculate the RMSE for the prediction and the test data
rmse(y_test, p1)