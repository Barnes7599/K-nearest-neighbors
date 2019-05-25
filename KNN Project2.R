library(ISLR)
library(tidyverse)
library(caTools)
library(class)

##### KNN PROJECT ON WDBC DATA SET #####
## Goal is to predict whether the cancer is benign or malignant based on the given features. 

wdbc <- read.table("wdbc.data", sep = ',')

######################
## EXPLORE THE DATA ##
######################

head(wdbc)
# We do not need V1 as it is a ID for the patient
wdbc <- wdbc[,-1]
# V2 is Diagnosis: M - Malignant or B - Benign 
head(wdbc)
str(wdbc)

##########################
## STANDARDIZE THE DATA ## 
##########################

standardized.wdbc <- scale(wdbc[,-1])
var(standardized.wdbc[,1])
var(standardized.wdbc[,24])
head(standardized.wdbc)


final.wdbc <- cbind(wdbc[1], standardized.wdbc)
head(final.wdbc)

cancer <- wdbc$V2
#####################
## TRAIN AND SPLIT ##
#####################

sample.stand.wdbc <- sample.split(final.wdbc$V2, SplitRatio = 0.7)
train.wdbc <- subset(final.wdbc, sample.stand.wdbc == TRUE)
test.wdbc <- subset(final.wdbc, sample.stand.wdbc == FALSE)


## VERIFY THE TRAIN AND SPLIT ##
str(train.wdbc)
str(test.wdbc)

###############
## KNN MODEL ##
###############

predicted.wdbc <- knn(train.wdbc[2:31], test.wdbc[2:31], train.wdbc$V2, k = 9)
error.rate <- mean(test.wdbc$V2 != predicted.wdbc)

error.rate


table(test.wdbc$V2, predicted.wdbc)

## We will use the RECALL METRIC to measure the models accuracy, if a patient has a malignant tumor (Actual Positive) goes through the test and predicted as not having a malignant tumor. The cost associated with False Negative is extremely high since the malignant tumor is not detected and the person therefore is not treated. 
recall.metric <- 61/(61 + 3)
recall.metric

#######################
## SELECTING K VALUE ##
#######################

sqrt(569)

# based on taking sqrt(n) we get 23 or you can do the following elbow method

predicted.wdbc <- NULL
error.rate <- NULL

for (i in 1:25) {
    
    predicted.wdbc <- knn(train.wdbc[2:31], test.wdbc[2:31], train.wdbc$V2, k = i)
    error.rate <- error.rate[i] <- mean(test.wdbc$V2 != predicted.wdbc)
}

k.values <-  1:25
error.df <- data.frame(error.rate, k.values)

ggplot(error.df, aes(k.values, error.rate)) +
    geom_point() +
    geom_line(lty = 'dotted', color = 'red') +
    theme_bw()
