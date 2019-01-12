# Linear regression 


# Load Data 
feats <- read.csv('./data/features.csv')
# Load Libraries
library(ISLR)
install.packages('glmnet')
library(glmnet)
library(dplyr)
library(tidyr)
library(Metrics)
#install.packages('caret')
library(caret)

colSums(is.na(feats))
# remove pokemons that never won
feats2 <- feats[-(which(is.na(feats$win_ratio))),]

#split train test data
trainIndex = createDataPartition(feats2$win_ratio,
                                 p=0.8, list=FALSE, times=1)

train = feats2[trainIndex,]
test = feats2[-trainIndex,]

# create Target variable
y <- train$win_ratio

# Model Ridge
CARET.TRAIN.CTRL = trainControl(method="repeatedcv",
                                number=5,
                                repeats=5,
                                verboseIter=FALSE)

model_ridge <- train(x=train,y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=0, # Ridge regression
                                          lambda=lambdas))








