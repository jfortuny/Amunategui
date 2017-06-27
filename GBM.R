# GBM
source('~/R Work/Amunategui/DataExplorationUtilities.R')

library(dplyr)
library(caret)
names(getModelInfo())

require(RCurl)
binData <- getBinaryURL("https://archive.ics.uci.edu/ml/machine-learning-databases/00296/dataset_diabetes.zip",
                        ssl.verifypeer = FALSE)
conObj <- file("dataset_diabetes.zip", open = "wb")
writeBin(binData, conObj)
close(conObj)

files <- unzip("dataset_diabetes.zip")
readLines(files[1], n=5)

library(readr)
diabetes <- data.frame(read_csv(files[1], na='?'))
dim(diabetes)
head(diabetes)

# drop useless variables
diabetes <- subset(diabetes, select = -c(encounter_id, patient_nbr, examide, citoglipton))
# fix the outcome variable to those readmitted under 30 days
diabetes$readmitted <- ifelse(diabetes$readmitted == "<30", 'yes', 'no')

# see what types of classes we have
charcolumns <- names(diabetes[sapply(diabetes, is.character)])
non_numeric_data_dim <- c()
for (colname in charcolumns) {
  non_numeric_data_dim <- rbind(non_numeric_data_dim,
                                c(colname, length(unique(diabetes[,colname]))))
}

non_numeric_data_dim <- data.frame(non_numeric_data_dim) %>%
  mutate(feature_name = as.character(X1), unique_counts = as.numeric(as.character(X2))) %>%
  select(feature_name, unique_counts) %>%
  arrange(desc(unique_counts))

head(non_numeric_data_dim)

diabetes <- Binarize_Features(data_set = diabetes,
                              leave_out_one_level = TRUE,
                              max_level_count = 20,
                              features_to_ignore = "readmitted")
dim(diabetes)
summary(diabetes)
str(diabetes)

diabetes <- Feature_Engineer_Integers(data_set = diabetes,
                                      features_to_ignore = c("admission_type_id",
                                                             "discharge_disposition_id",
                                                             "admission_source_id"))

nzv <- nearZeroVar(diabetes, saveMetrics = TRUE)
length(rownames(nzv[nzv$nzv==FALSE,]))

diabetes <- diabetes[ , rownames(nzv[nzv$nzv==FALSE,])]
dim(diabetes)

# Model using GBM

# prep the variables
outcome_name <- "readmitted"
# clean up all feature names - replace periods with underscores
predictor_names <- setdiff(names(diabetes), outcome_name)

set.seed(1234)
splitIndex <- createDataPartition(diabetes[, outcome_name],
                                  p = 0.75,
                                  list = FALSE,
                                  times = 1)
train_data <- diabetes[splitIndex,]
test_data <- diabetes[-splitIndex,]

objControl <- trainControl(method = "cv",
                           number = 2,
                           returnResamp = "none",
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE)

gbm_caret_model <- train(train_data[,predictor_names],
                         as.factor(train_data[,outcome_name]),
                         method = "gbm",
                         trControl = objControl,
                         metric = "ROC",
                         preProcess = c("center", "scale"))

summary(gbm_caret_model)
print(gbm_caret_model)

predictions <- predict(object = gbm_caret_model,
                       test_data[,predictor_names],
                       type = "raw")
head(predictions)
print(postResample(pred = predictions,
                   obs = as.factor(test_data[,outcome_name])))

prop.table(table(as.factor(diabetes[,outcome_name])))

predictions <- predict(object = gbm_caret_model,
                       test_data[,predictor_names],
                       type = "prob")
head(predictions)
library(pROC)
auc <- roc(ifelse(test_data[,outcome_name]=="yes",1,0), predictions[[2]])
print(auc$auc)
