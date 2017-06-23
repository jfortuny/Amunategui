# Factors
source('~/R Work/Amunategui/DataExplorationUtilities.R')

Titanic_dataset <- read.table('http://math.ucdenver.edu/RTutorial/titanic.txt',
                              sep='\t',
                              header=TRUE,
                              stringsAsFactors = FALSE)
head(Titanic_dataset)
str(Titanic_dataset)

Titanic_dataset_temp <- Binarize_Features(data_set = Titanic_dataset,
                                          features_to_ignore = c('Name'))
str(Titanic_dataset_temp)

# pipeline
Titanic_dataset_temp <- Titanic_dataset
head(Titanic_dataset_temp)
# fix date field if any
Titanic_dataset_temp <- Fix_Date_Features(data_set = Titanic_dataset_temp)
head(Titanic_dataset_temp)
# extract quantitative value out of text entires
Titanic_dataset_temp <- Get_Free_Text_Measures(data_set = Titanic_dataset_temp)
head(Titanic_dataset_temp)
# binarize categories
Titanic_dataset_temp$Name_1st_word <- NULL
Titanic_dataset_temp <- Binarize_Features(data_set = Titanic_dataset_temp,
                                          features_to_ignore = c(),
                                          leave_out_one_level = TRUE,
                                          max_level_count = 6)
head(Titanic_dataset_temp)
