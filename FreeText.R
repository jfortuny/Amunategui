# Free text
source('~/R Work/Amunategui/DataExplorationUtilities.R')

Titanic_dataset <- read.table('http://math.ucdenver.edu/RTutorial/titanic.txt',
                              sep='\t',
                              header=TRUE,
                              stringsAsFactors = FALSE)
head(Titanic_dataset)
str(Titanic_dataset)

Titanic_dataset_temp <- Get_Free_Text_Measures(data_set = Titanic_dataset, features_to_ignore = c())
str(Titanic_dataset_temp)
