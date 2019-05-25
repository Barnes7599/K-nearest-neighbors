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
