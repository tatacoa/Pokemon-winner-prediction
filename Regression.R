# Linear regression

# Load Libraries
#install.packages('glmnet')
library(glmnet)
library(dplyr)
#install.packages('caret')
library(caret)

# Load Data
feats <- read.csv('data/features.csv')

colSums(is.na(feats)) # some pokemon never fought
# remove pokemons that never won
feats_clean <- na.omit(feats)
feats_clean$Legendary <- as.integer(as.logical(feats_clean$Legendary)) # convert category legendary into numerical values
feats_clean <- select(feats_clean, -c(Generation, win_counts, losses)) # drop unnecessary generation column
# typ <- as.integer(feats_clean$Type.1)
#split train test data
trainIndex = createDataPartition(feats_clean$win_ratio,
                                 p=0.6, list=FALSE, times=1)

train = feats_clean[trainIndex,] # 60% training data
test_val = feats_clean[-trainIndex,]

trainIndex = createDataPartition(test_val$win_ratio,
                                 p=0.5, list=FALSE, times=1)
val = test_val[trainIndex,] # 20% validation data
test = test_val[-trainIndex,] # 20% test data


# create Target variable
y_train <- train$win_ratio
y_val <- val$win_ratio
y_test <- test$win_ratio


x_train <- train[6:13]
x_val <- val[6:13]
x_test <- test[6:13]

# Ridge regression
trainControl = trainControl(method="repeatedcv",
                                number=5,
                                repeats=5,
                                verboseIter=FALSE)
lambdas <- seq(1,0,-0.001)
model_ridge <- train(x=x_train,y=y_train,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=trainControl,
                     tuneGrid=expand.grid(alpha=0, # Ridge regression
                                          lambda=lambdas))
model_ridge

# plot the effect of chosen lambda on the RMSE
ggplot(data=model_ridge$results[model_ridge$results$RMSE<=0.10,]) +
  geom_line(aes(x=lambda,y=RMSE))

# quality of model
mean(model_ridge$resample$RMSE)

# Lasso regression
model_lasso <- train(x=x_train,y=y_train,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=trainControl,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))
model_lasso

# quality of model
mean(model_lasso$resample$RMSE)

predLasso <- predict(model_lasso,newdata = x_val)
predRidge <- predict(model_ridge, newdata = x_val)

# function for calculating the metric
rmse <- function(actual, predicted)
{
  error <- actual - predicted
  sqrt(mean(error^2))
}

rmse(y_val, predLasso) # Lasso regression seems to be the better model
rmse(y_val, predRidge)

# extract coefficients for the best performing model
coef <- data.frame(coef.name = dimnames(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda))[[1]], 
                   coef.value = matrix(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda)))

# exclude the (Intercept) term
coef <- coef[-1,]

# print summary of model results
picked_features <- nrow(filter(coef,coef.value!=0))
not_picked_features <- nrow(filter(coef,coef.value==0))

cat("Lasso picked",picked_features,"variables and eliminated the other",
    not_picked_features,"variables\n")

# sort coefficients in ascending order
coef <- arrange(coef,-coef.value)

# extract the top 10 and bottom 10 features
imp_coef <- rbind(head(coef,10),
                  tail(coef,10))

ggplot(imp_coef) +
  geom_bar(aes(x=reorder(coef.name,coef.value),y=coef.value),
           stat="identity") +
  ylim(min(imp_coef$coef.value),max(imp_coef$coef.value)) +
  coord_flip() +
  ggtitle("Coefficents in the Lasso Model") +
  theme(axis.title=element_blank())
