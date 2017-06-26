# Outliers
source('~/R Work/Amunategui/DataExplorationUtilities.R')

head(Identify_Outliers(mtcars, remove_outlying_features=FALSE))

plot(sort(mtcars$wt))
plot(sort(mtcars$mpg))
