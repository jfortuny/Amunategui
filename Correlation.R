# Correlation
source('~/R Work/Amunategui/DataExplorationUtilities.R')
library(dplyr)
library(reshape2)
library(psych)
library(corrplot)

data_set <- mtcars
d_cor <- as.matrix(cor(data_set))
d_cor
d_Cor_melt <- arrange(melt(d_cor), -(value))
pairwise_corr_matrix <- filter(d_Cor_melt, Var1 != Var2)
pairwise_corr_matrix <- filter(pairwise_corr_matrix, is.na(value)==FALSE)
pairwise_corr_matrix <- pairwise_corr_matrix[seq(1, nrow(pairwise_corr_matrix), by=2),]
plot(pairwise_corr_matrix$value)

data_set <- mtcars
featurenames_copy <- names(data_set)
# strip var names to index for pair wise identification
names(data_set) <- seq(1:ncol(data_set))
cor_data_df <- corr.test(data_set)
# apply var names to correlation matrix over index
rownames(cor_data_df$r) <- featurenames_copy
colnames(cor_data_df$r) <- featurenames_copy
names(cor_data_df)
# matrix of correlations
cor_data_df$r
cor.plot(cor_data_df$r)
corrplot.mixed(cor_data_df$r, lower="circle", upper="color",
               tl.pos="lt", diag="n",
               order="hclust", hclust.method="complete")

dim(Get_Top_Relationships(mtcars))
head(Get_Top_Relationships(mtcars))
