library(e1071)
library(ggplot2)
library(ggthemes)
library(caTools)

df <- read.csv('CSV files for ML Projects\\loan_data.csv')
str(df)

#Convert the following columns to categorical data 

df$purpose <- factor(df$purpose)
df$credit.policy <- factor(df$credit.policy)
df$delinq.2yrs <- factor(df$delinq.2yrs)
df$pub.rec <- factor(df$pub.rec)
df$inq.last.6mths <- factor(df$inq.last.6mths)
df$not.fully.paid <- factor(df$not.fully.paid)
str(df)

###
# EDA
###

#Create a histogram of fico scores colored by not.fully.paid

ggplot(df, aes(fico)) + geom_histogram(aes(fill = not.fully.paid), col = 'black', bins = 40 ) +
  theme_bw()

#Create a barplot of purpose counts, colored by not.fully.paid. Use position=dodge in the geom_bar

ggplot(df, aes(purpose)) + geom_bar(aes(fill = not.fully.paid), col = 'black', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90))

#Create a scatterplot of fico score versus int.rate. Does the trend make sense? Play around with the 
#color scheme if you want.

ggplot(df, aes(int.rate, fico)) + geom_point(aes(col = not.fully.paid), size = 3, alpha = 0.5) + 
  theme_bw() 

############
## BUILDING THE MODEL
############

#Split your data into training and test sets

set.seed(101)
sample = sample.split(df$not.fully.paid, SplitRatio = 0.7)
train <- subset(df, sample = T)
test <- subset(df, sample = F)

#Now use the svm() (in e1071 library) function to train a model on your training set.

model <- svm(not.fully.paid ~ ., train)
summary(model)

pred <- predict(model, test[1:13] )
table(pred, test$not.fully.paid)

############
## TUNING THE MODEL
############

#You probably got some not so great results! With the model classifying everything into one group!
#Let's tune our model to try to fix this. Use the tune() function to test out different cost and 
#gamma values. tuning can take a long time (since its running a bunch of different models!).

tune.result <- tune(svm, train.x = not.fully.paid ~ ., data = train, kernel = 'radial', 
                   ranges = list(cost = c(1,10), gamma = c(0.01, 0.1)))

## in summary(tune.result) we see best parameters : cost 1 gamma 0.1 

tune.model <- svm(not.fully.paid ~ ., train, cost = 1 , gamma = 0.1)
pred2 <- predict(tune.model, test[1:13])
table( pred2, test$not.fully.paid)

# so first tune the data to find appropriate values for cost and gamma then fit the model.
