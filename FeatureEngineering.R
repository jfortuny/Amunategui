# Feature Engineering
source('~/R Work/Amunategui/DataExplorationUtilities.R')

# Dates
mix_dataset <- data.frame(
  id=c(10,20,30,40,50),
  gender=c('male','female','female','male','female'),
  some_date=c('2012-01-12','2012-01-12','2012-12-01','2012-05-30','2013-12-12'),
  value=c(12.34, 32.2, 24.3, 83.1, 8.32),
  outcome=c(1,1,0,0,0))
mix_dataset
str(mix_dataset)
mix_dataset$some_date <- as.Date(mix_dataset$some_date)
mix_dataset <- Feature_Engineer_Dates(mix_dataset)
mix_dataset

# Integers
mix_dataset <- data.frame(
  id=c(1,2,3,4,5),
  mood=c(0,20,20,40,50),
  value=c(12.34, 32.2, 24.3, 83.1, 8.32),
  outcome=c(1,1,0,0,0))
mix_dataset
str(mix_dataset)
Feature_Engineer_Integers(mix_dataset, features_to_ignore=c('id'))

# Pipeline check
mix_dataset <- data.frame(
  id=c(1,2,3,4,5),
  gender=c('male','female','female','male','female'),
  some_date=c('2012-01-01','2013-01-01','2014-01-01','2015-01-01','2016-01-01'),
  mood=c(0,20,20,NA,50),
  value=c(12.34, 32.2, 24.3, 83.1, 8.32),
  outcome=c(1,1,0,0,0))
library(readr)
write_csv(mix_dataset, 'mix_dataset.csv')
mix_dataset <- as.data.frame(read_csv('mix_dataset.csv'))
str(mix_dataset)
mix_dataset
# automated pipeline
mix_dataset <- Get_Free_Text_Measures(data_set = mix_dataset)
mix_dataset <- Binarize_Features(data_set = mix_dataset, leave_out_one_level = FALSE)
mix_dataset <- Impute_Features(data_set = mix_dataset)
mix_dataset <- Feature_Engineer_Dates(data_set = mix_dataset)
str(mix_dataset)
mix_dataset <- Feature_Engineer_Integers(data_set = mix_dataset)
mix_dataset <- Feature_Engineer_Numbers(data_set = mix_dataset)
str(mix_dataset)
