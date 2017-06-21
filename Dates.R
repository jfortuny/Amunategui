# Dates
source('~/R Work/Amunategui/DataExplorationUtilities.R')

# mix_dataset <- data.frame(
#   id=c(10,20,30,40,50),
#   gender=c('male','female','female','male','female'),
#   some_date=c('01/11/2012','04/12/2012','28/02/2013','17/06/2014','08/03/2015'),
#   value=c(12.34, 32.2, 24.3, 83.1, 8.32),
#   outcome=c(1,1,0,0,0))
# write.csv(mix_dataset, 'mix_dataset.csv', row.names = FALSE)

path_and_file_name <- 'mix_dataset.csv'
readLines(path_and_file_name)

mix_dataset <- read.csv(path_and_file_name, stringsAsFactors = FALSE)
str(mix_dataset)
head(mix_dataset)
#mix_dataset$some_date <- as.Date(mix_dataset$some_date)
Fix_Date_Features(mix_dataset)
