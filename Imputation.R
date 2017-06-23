# Imputation
source('~/R Work/Amunategui/DataExplorationUtilities.R')

# data for experimentation
mix_dataset <- data.frame(
  id=c(1,NA,3,4,5),
  mood=c(0,20,20,Inf,50),
  value=c(12.34, 32.2, NaN, 83.1, 8.32),
  outcome=c(1,1,0,0,0))
head(mix_dataset)

# finding NAs
mix_dataset_temp <- mix_dataset
is.na(mix_dataset_temp)

# imputing values
mix_dataset_temp <- Impute_Features(mix_dataset,
                                    use_mean_instead_of_0 = TRUE,
                                    mark_NAs = TRUE)
mix_dataset_temp

# pipeline check
mix_dataset <- data.frame(
  ids=c(1,NA,3,4,5),
  some_dates = c('01/11/2012','04/12/2012','28/02/2013','17/06/2014','08/03/2015'),
  mood=c(0,20,20,Inf,50),
  some_real_numbers = c(12.34, 32.2, NaN, 83.1, 8.32),
  some_text = c('sentence one','sentence two', 'mixing it up', 'sentence four', 'sentence five'))
head(mix_dataset)
str(mix_dataset)
# format date field to be R compliant
mix_dataset$some_dates <- as.Date(mix_dataset$some_dates, format="%d/%m/%Y")
str(mix_dataset$some_dates)
# extra quantitative value out of text entires
mix_dataset <- Get_Free_Text_Measures(data_set = mix_dataset)
head(mix_dataset,2)
# determine whether to keep both the first and second words before binarizing
# binarize categories
mix_dataset <- Binarize_Features(data_set = mix_dataset,
                                 features_to_ignore = c(),
                                 leave_out_one_level = TRUE)
head(mix_dataset,2)
# impute missing data using 0
mix_dataset <- Impute_Features(mix_dataset,
                               use_mean_instead_of_0 = FALSE,
                               features_to_ignore = c('some_dates'))
mix_dataset
