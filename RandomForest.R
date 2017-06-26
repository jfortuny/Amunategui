# Random Forest
source('~/R Work/Amunategui/DataExplorationUtilities.R')

# Manual Approach using randomForest directly (no caret)

# Titanic from UCI
readLines('http://math.ucdenver.edu/RTutorial/titanic.txt', n=5)
titanicDF <- read.csv('http://math.ucdenver.edu/RTutorial/titanic.txt',
                      sep='\t',
                      header = TRUE,
                      stringsAsFactors = FALSE)
head(titanicDF)
str(titanicDF)

# Conversions and Feature Engineering
titanicDF$Sex <- as.factor(titanicDF$Sex)
titanicDF$PClass <- as.factor(titanicDF$PClass)

titanicDF <- Get_Free_Text_Measures(titanicDF)
titanicDF$Name_1st_word <- NULL
titanicDF <- Binarize_Features(titanicDF, leave_out_one_level = TRUE)
titanicDF <- Impute_Features(titanicDF, use_mean_instead_of_0 = FALSE)

# split data set
set.seed(1234)
random_splits <- runif(nrow(titanicDF))
train_data <- titanicDF[random_splits < .5,]
tune_data <- titanicDF[random_splits >= .5 & random_splits < .8,]
test_data <- titanicDF[random_splits >= .8,]

# tune the model with the tune data to identify optimal mtry
library(randomForest)
set.seed(1234)
outcome_name <- 'Survived'
feature_names <- setdiff(names(train_data), outcome_name)
tnRF <- tuneRF(x=tune_data[,feature_names],
               y = as.factor(tune_data[,outcome_name]),
               mtryStart = 3, stepFactor = 0.5)
best_mtry <- tnRF[tnRF[, 2] == min(tnRF[, 2]), 1][[1]]
best_mtry

# model the data with the train data
rf_model <- randomForest(x=train_data[,feature_names],
                         y=as.factor(train_data[,outcome_name]),
                         importance=TRUE, ntree=100, mtry = best_mtry)
print(importance(rf_model, type=1)[importance(rf_model, type=1)!=0,])

# test the model with the Area Under the Curve score
library(pROC)
predictions <- predict(rf_model,
                       newdata=test_data[,feature_names],
                       type="prob")
print(roc(response = test_data[,outcome_name],
          predictor = predictions[,2]))


# Using caret
