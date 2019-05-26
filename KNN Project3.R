library(ISLR)
library(tidyverse)
library(caTools)
library(class)

df <- read.table("wpbc.data", sep = ',')

head(df)
str(df)

df <- df %>% 
    select(2:35)

head(df)

df <- df[,-34]
head(df)

stand.df <- scale(df[,-1])
head(stand.df)

final.df <- cbind(stand.df, df[1])
head(final.df)

sample <- sample.split(final.df$V2, SplitRatio = 0.7)
sample.train <- subset(final.df, sample == TRUE)
sample.test <- subset(final.df, sample == FALSE)

set.seed(101)
predicted.df <- knn(sample.train[1:32], sample.test[1:32], sample.train$V2, k = 8)

error.rate  <-  mean(sample.test$V2 != predicted.df)

error.rate

sqrt(198)

table(sample.test$V2, predicted.df)

recall <- 2/(2+12)
recall

predicted.species <-  NULL
error.rate <- NULL

for (i in 1:10) {
    set.seed(101)
    predicted.df <- knn(sample.train[1:32], sample.test[1:32], sample.train$V2, k = i)
    error.rate[i] <-  mean(sample.test$V2 != predicted.df)
}


k.values <- 1:10
error.df <- data.frame(error.rate, k.values)

ggplot(error.df, aes(k.values, error.rate)) + 
    geom_point() +
    geom_line(lty = 'dotted', color = 'red') +
    theme_bw()


