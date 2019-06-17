file_loc <- read.csv("D:\\Programming\\DA\\Lab 4\\domestict20careerbattingrating_mod.csv")[1:3] 
View(file_loc)

str(file_loc)
summary(file_loc)
file_loc[is.na(file_loc)] <- 0
str(file_loc)
View(file_loc)

file_loc_sc <- as.data.frame(file_loc)
dist_mat <- dist(file_loc_sc, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 2)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 2, border = 2:6)
abline(h = 2, col = 'red')

suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)

suppressPackageStartupMessages(library(dplyr))
file_loc_cl <- mutate(file_loc, cluster = cut_avg)
count(file_loc_cl,cluster)
suppressPackageStartupMessages(library(ggplot2))
ggplot(file_loc_cl, aes(x=Matches, y = Innings, color = factor(cluster))) + geom_point()

table(file_loc_cl$cluster)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

df <- read.csv("D:\\Programming\\DA\\Lab 4\\domestict20careerbattingrating_mod.csv")
df[is.na(df)] <- 0
ggplot(df, aes(df$Name, df$Runs)) + geom_point()

fit <- kmeans(df$Runs,3)
plot(df$Name,df$Runs,col=fit$cluster,pch=1)
points(fit$centers,col=1:8,pch=3)

library(cluster)
library(fpc)
plotcluster(df$Runs,fit$cluster)
points(fit$centers,col=1:8,pch=16)
clusplot(df, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

fit <- kmeans(df$High_Score,3)
plot(df$High_Score,col=fit$cluster,pch=15)
points(fit$centers,col=1:8,pch=3)

####################################################

library(tree)
df <- read.csv("D:\\Programming\\DA\\Lab 4\\domestict20careerbattingrating_mod.csv")
df[is.na(df)] <- 0
High = ifelse(df$Runs < 1000, "No", "Yes")
df = data.frame(df, High)
tree.df = tree(High~.-df$Runs, data = df[,c(5,17)])
summary(tree.df)
tree.df 
plot(tree.df)
text(tree.df, pretty = 0)
tree.df
set.seed(101)
train=sample(1:nrow(df), 250)
tree.df = tree(High~.-df$Runs,df[,c(5,17)], subset=train)
plot(tree.df)
text(tree.df, pretty=0)

