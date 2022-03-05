library(tidyverse)
options(warn=-1)
dataset <- read.csv("IPL_trophy_over_years.csv")
x <- dataset$Win.percentage
y <- dataset$Number.of.trophies.won
new_dataset <- spread(dataset,Win.percentage,Number.of.trophies.won)
summary(new_dataset[,c(10:13)])
summary(dataset)
t.test(x,y)
cor.test(dataset$Win.percentage,dataset$Number.of.trophies.won)
cor(dataset[,2:6])
cor.test(dataset$Win.percentage,dataset$Number.of.trophies.won,method = c("spearman"),alt="greater",conf.level=0.99)
cor.test(dataset$Win.percentage,dataset$Number.of.trophies.won,method=c("pearson"),alt="greater",conf.level=0.99)
pairs(dataset[,2:6])

