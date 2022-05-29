library(ISLR)
library(caTools)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)

df <- College

######
# EDA
######

#Create a scatterplot of Grad.Rate versus Room.Board, colored by the Private column.

ggplot(df, aes(Room.Board, Grad.Rate)) + geom_point(aes(col = Private), size = 3, alpha = 0.5)

#Create a histogram of full time undergrad students, color by Private

ggplot(df, aes(F.Undergrad)) + geom_histogram(aes(fill = Private), col = 'black', bins = 50)

#Create a histogram of Grad.Rate colored by Private. You should see something odd here.

ggplot(df, aes(Grad.Rate)) + geom_histogram(aes(fill = Private), col = 'black', binwidth = 2)

#What college had a Graduation Rate of above 100% ?

filter(df, Grad.Rate > 100)$Grad.Rate

#Change that college's grad rate to 100%

df[df$Grad.Rate>100,]$Grad.Rate <- 100

##################
# TRAIN TEST SPLIT
##################

set.seed(10)
splt <- sample.split(df$Private, SplitRatio = 0.7)
train <- subset(df, splt == T)
test <- subset(df, splt == F)

##################
### DECISION TREE
##################

#Use the rpart library to build a decision tree to predict whether or not a school is Private.

tree <- rpart(Private ~ ., method = 'class', train)
pred <- predict(tree, test)
head(pred)

#Turn these two columns into one column to match the original Yes/No Label for a Private column.

pred <- as.data.frame(pred)
pred$Private <- ifelse(pred$No > 0.5, 'NO', 'Yes')
test$pred1 <- pred$Private

#Now use table() to create a confusion matrix of your tree model.

table(test$Private, test$pred1)

#Use the rpart.plot library and the prp() function to plot out your tree model.

prp(tree)

##################
### RANDOM FOREST
##################

#Now use randomForest() to build out a model to predict Private class. Add importance=TRUE as a 
#parameter in the model. (Use help(randomForest) to find out what this does.

set.seed(10)
rf.model <- randomForest(Private ~ ., train, importance = T)
rf.model

#What was your model's confusion matrix on its own training set? 

rf.model$confusion

#Grab the feature importance with model$importance.

rf.model$importance

#Now use your random forest model to predict on your test set!

test$pred2 <- predict(rf.model, test)
table(test$Private, test$pred2)
