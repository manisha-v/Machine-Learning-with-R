library(ISLR) # iris dataset
library(ggplot2) 
library(dplyr) 
library(caTools) # To split data
library(class) # knn function

#Use the ISLR libary to get the iris data set

head(iris)

##################
# STANDARDIZE DATA
##################

#Use scale() to standardize the feature columns of the iris dataset.

df <- data.frame(scale(iris[,-5]))

#Check that the scaling worked by checking the variance of one of the new columns.

var(df[,1])

#Join the standardized data with the response/target/label column.

df$Species <- iris$Species 
head(df)

#########################
# TRAIN AND TEST SPLITS
#########################

#split your standardized data into train and test sets. Use a 70/30 split.

splt <- sample.split(df$Species, SplitRatio = 0.7)
train <- subset(df, splt == T)
test <- subset(df, splt == F)

##############
## KNN MODEL
##############

#Use the knn function(in class library) to predict Species of the test set. Use k=1

test$pred <- knn(train[,1:4], test[,1:4], train$Species, k=1 )
head(test$pred)

#What was your misclassification rate?

mean(test$pred != test$Species)

###################
# CHOOSING K VALUE
###################

#Create a plot of the error (misclassification) rate for k values ranging from 1 to 10.

missclass.error <- NULL
predicted <- NULL

for (i in 1:10) {
  predicted <- knn(train[,1:4], test[, 1:4], train$Species, k = i)
  missclass.error[i] <- mean(predicted != test$Species)
}

error.df <- data.frame(missclass.error, 'k.value' = 1:10)

ggplot(error.df, aes(k.value, missclass.error)) + geom_point() + geom_line(col = 'red')
