library(neuralnet)
library(ggplot2)
library(caTools)
library()

df <- read.csv('CSV files for ML Projects\\bank_note_data.csv')
head(df)

#Train Test Split

set.seed(201)
splt <- sample.split(df$Class, SplitRatio = 0.7)
train <- subset(df, splt == T)
test <- subset(df, splt == F)

#########
#### NEURAL NET
#########

#Use the neuralnet function to train a neural net, set linear.output=FALSe and choose 10 hidden 
#neurons (hidden=10)

n <- names(df)
form <- as.formula(paste('Class ~ ', paste(n[!n %in% 'Class'], collapse = ' + ')))
nn <- neuralnet(form, train, hidden = 10, linear.output = F)

plot(nn)

#Use compute() to grab predictions using your nn model on the test set.

pred <- compute(nn, test[1:4])

#Check the head of the predicted values. You should notice that they are still probabilities.

head(pred$net.result)

#Apply the round function to the predicted values so you only 0s and 1s as your predicted classes

pred$net.result <- round(pred$net.result)
head(pred$net.result)

#Use table() to create a confusion matrix of your predictions versus the real values

table(pred$net.result, test$Class)
