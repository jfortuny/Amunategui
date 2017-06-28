# K-Means
source('~/R Work/Amunategui/DataExplorationUtilities.R')
library(dplyr)
library(factoextra)

AutoMpg_data <- read.csv("http://mlr.cs.umass.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
                         na.strings = '?',
                         header=FALSE,
                         sep="",
                         as.is=TRUE,
                         col.names = c("mpg", "cylinders", "displacement", "horsepower",
                                       "weight", "acceleration", "model", "origin", "car_name"),
                         stringsAsFactors = FALSE)
dim(AutoMpg_data)
head(AutoMpg_data)
str(AutoMpg_data)
AutoMpg_data <- Get_Free_Text_Measures(data_set = AutoMpg_data, minimum_unique_threshold=0.5)
AutoMpg_data <- Impute_Features(data_set = AutoMpg_data, use_mean_instead_of_0 = FALSE)

# Model with K-Means - overall clustering
set.seed(1234)
km1 = kmeans(x = select(AutoMpg_data, weight, acceleration), centers = 3)
# Plot results
plot(select(AutoMpg_data, weight, acceleration),
     col =km1$cluster, main="K-Means result with 3 clusters",
     pch=20, cex=2)
# find each cluster's centroids
points(km1$centers, pch=6, col='blue', cex=6)
points(km1$centers, pch=6, col='blue', cex=4)
points(km1$centers, pch=6, col='blue', cex=2)

unique(AutoMpg_data$car_name_1st_word)
# Model with K-Means - clustering by brand name
brand_set <-
  select(AutoMpg_data, weight, acceleration, car_name_1st_word) %>%
  group_by(car_name_1st_word) %>%
  summarize_each(funs(mean)) %>%
  data.frame
row.names(brand_set) <- brand_set$car_name_1st_word
brand_set <- dplyr::select(brand_set,-car_name_1st_word)
set.seed(1234)
km1 <- kmeans(x = brand_set, centers = 3)
# Plot results
plot(
  brand_set,
  col = km1$cluster,
  main = "K-Means result with 3 clusters",
  pch = 20,
  cex = 2
)

set.seed(1234)
km1 <- kmeans(x = brand_set, centers = 3)
print(km1)

fviz_cluster(km1, data = brand_set)

set.seed(1234)
fviz_nbclust(brand_set, kmeans, method = "wss")
