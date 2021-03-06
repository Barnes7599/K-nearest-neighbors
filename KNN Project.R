##### KNN PROJECT ON IRIS DATA SET #####
library(ggplot2)
library(ISLR)

######################
## EXPLORE THE DATA ##
######################

head(iris)
str(iris)


##########################
## STANDARDIZE THE DATA ## 
##########################

standardized.iris <- scale(iris[,-5])
head(standardized.iris)
var(iris[,1])
var(iris[,2])

final.data <- cbind(standardized.iris,iris[5])
head(final.data)

#####################
## TRAIN AND SPLIT ##
#####################

library(caTools)
set.seed(101)
sample.iris <- sample.split(final.data$Species, SplitRatio = 0.7)     
train.iris <- subset(final.data, sample.iris == TRUE)
test.iris <- subset(final.data, sample.iris == FALSE)

###############
## KNN MODEL ##
###############

library(class)

predicted.species <- knn(train.iris[1:4], test.iris[1:4], train.iris$Species, k = 11)
error.rate <- mean(test.iris$Species != predicted.species)

error.rate

#############
## K VALUE ##
#############

sqrt(150) 

#or

predicted.species <-  NULL
error.rate <- NULL

for (i in 1:10) {
    set.seed(101)
    predicted.species <- knn(train.iris[1:4], test.iris[1:4], train.iris$Species, k = i)
    error.rate[i] <-  mean(test.iris$Species != predicted.species)
}


k.values <- 1:10
error.species.df <- data.frame(error.rate, k.values)

ggplot(error.species.df, aes(k.values, error.rate)) + 
    geom_point() +
    geom_line(lty = 'dotted', color = 'red') +
    theme_bw()
