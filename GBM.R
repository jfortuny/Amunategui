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
diabetes <- subset(diabetes, select = c(encounter_id, patient_nbr, examide, citoglipton))
# fix the outcome variable to those readmitted under 30 days
diabetes$readmitted <- ifelse(diabetes$readmitted == "<30", 'yes', 'no')
