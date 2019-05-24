####
## K Nearest Neighbors (KNN) is a classification algorithm that operates on a very simple principle. Training algorithm (Store all the data) Prediction Algorithm: 1. Calculate the distance from x points in your data. 2. Sort the points in your data by increasing distacnce from x. 3. Predict the majority label of the "k" closest points. If you choose k = 3 then the algorithm will look at the 3 "nearest neighbors" to the new point and predicted the new point on the majority of the nearest neighbors.

#Pros: Very simple, training is trivial, works with any number of classes, easy to add more data, and few parameters: K and the distance metric.

#Cons: High prediction costs (worse for large data set), high dimensional data, categorical vaiables do not work well. 
####

####
## Load the required libraries
library(tidyverse)
# ISLR is where we will get our dataset
library(ISLR)
# We will apply the KNN approach to the Caravan data set, which is part of the ISLR library. This data set includes 85 predictors that measure demographic characteristics for 5,822 individuals. The response variable is Purchase, which indicates whether or not a given individual purchases a Caravan insurance policy. 
####

######################
## DATA EXPLORATION ##
######################
str(Caravan)
summary(Caravan$Purchase)
# In this data set, only 6% of people purchased caravan insurance.

# Check for NAs
any(is.na(Caravan))

###########################
## STANDARDIZE VARIABLES ##
###########################
####
## Because the KNN classifier predicts the class of a given test observation by identifying the observations that are nearest to it, the scale of the variables matters. Any variables that are on a large scale will have a much larger effect on the distance between the observations, and hence on the KNN classifier, than variables that are on a small scale.
##
var(Caravan[,1])
var(Caravan[,2])
## Clearly the scales are different! We are now going to standarize all the X variables except Y (Purchase). The Purchase variable is in column 86 of our dataset, so let’s save it in a separate variable because the knn() function needs it as a separate argument.
purchase <- Caravan[,86]
standardized.Caravan <- scale(Caravan[,-86])
# Lets again check the variance
var(standardized.Caravan[,1])
var(standardized.Caravan[,2])
# We can see that now that all independent variables (X’s) have a mean of 1 and standard deviation of 0.

######################
## TRAIN TEST SPLIT ##
######################

test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]

train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]

###############
## KNN MODEL ##
###############

## Rememeber that we are trying to come up with a model to predict whether someone will purchase or not. We will use the knn() function to do so, and we will focus on 4 of its arguments that we need to specify. The first argument is a data frame that contains the training data set(remember that we don’t have the Y here), the second argument is a data frame that contains the testing data set (again no Y variable), the third argument is the train.purchase column (Y) that we save earlier, and the fourth argument is the k (how many neighbors). Let’s start with k = 1. knn() function returns a vector of predicted Y’s.

library(class)
set.seed(101)
predicted.purchase <- knn(train.data, test.data, train.purchase, k = 9 )
head(predicted.purchase)

missclass.error <-  mean(test.purchase != predicted.purchase)
missclass.error

########################
## CHOOSING A K VALUE ##
########################
## Should we manually change k and see which k gives us the minimal misclassification rate? NO! we have computers, so let’s automate the process with a for() loop. A loop in R repeats the same command as much as you specify. For example, if we want to check for k =1 up to 100, then we have to write 3 x 100 lines of code, but with a for loop, you just need 4 lines of code, and you can repeat those 3 lines up to as many as you want. (Note this may take awhile because you're running the model 20 times!)

predicted.purchase <-  NULL
error.rate <-  NULL

for (i in 1:20) {
    set.seed(101)
    predicted.purchase <- knn(train.data, test.data, train.purchase, k = i)
    error.rate[i] <- mean(test.purchase != predicted.purchase)
}
error.rate

##############################
## VISUALIZE K ELBOW METHOD ##
##############################

k.values <- 1:20
error.df <- data.frame(error.rate, k.values)
error.df

ggplot(error.df, aes(k.values, error.rate)) + 
    geom_point() +
    geom_line(lty = 'dotted', color = 'red')

## Pick the k value at the eblow in our case 9
