library(ggplot2)
library(ggthemes)
library(dplyr)
library(Amelia)
library(caTools)

df <- read.csv('CSV files for ML Projects\\adult_sal.csv')
head(df)
df <- select(df,-X)
head(df)
str(df)

#converting some columns in factors
df$type_employer <- factor(df$type_employer)
df$education <- factor(df$education)
df$marital <- factor(df$marital)
df$type_employer <- factor(df$type_employer)
df$occupation <- factor(df$occupation)
df$relationship <- factor(df$relationship)
df$race <- factor(df$race)
df$sex <- factor(df$sex)
df$country <- factor(df$country)
df$income <- factor(df$income)
str(df)

#DATA CLEANING

#Use table() to check out the frequency of the type_employer column.
table(df$type_employer)

#Combine two smallest groups into a single group called "Unemployed".
df$type_employer <- as.character(df$type_employer)
unemploy <- function(x){
  if(x == 'Never-worked' | x == 'Without-pay'){
    return('Unemployed')
  }
  return(x)
}
df$type_employer <- sapply(df$type_employer, unemploy)
table(df$type_employer)

#Combine State and Local gov jobs into a category called SL-gov and combine self-employed jobs 
#into a category called self-emp.
change_employ <- function(x){
  if(x == 'Local-gov' | x == 'State-gov'){
    return('SL-gov')
  }else if(x == 'Self-emp-inc' | x == 'Self-emp-not-inc'){
    return('Self-emp')
  }
  return(x)
}
df$type_employer <- sapply(df$type_employer, change_employ)
table(df$type_employer)

#Use table() to look at the marital column
table(df$marital)

#Reduce this to three groups: Married, Not-Married, Never-Married  
change_marital <- function(x){
  if(x == 'Married-AF-spouse' | x == 'Married-civ-spouse' | x == 'Married-spouse-absent'){
    return('Married')
  }else if(x == 'Divorced' | x == 'Separated' | x== 'Widowed'){
    return('Not-Married')
  }
  return('Never-Married')
}
df$marital <- sapply(df$marital, change_marital)
table(df$marital)

#Check the country column using table(). Group these countries together however you see fit. 
#group by continents
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
change_country <- function(x){
  if(x %in% Asia){
    return('Asia')
  }else if(x %in% Europe){
    return('Europe')
  }else if(x %in% North.America){
    return('North_America')
  }else if(x %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }
  return('other')
}
df$country <- as.character(df$country)
df$country <- sapply(df$country, change_country)
table(df$country)

# MISSING DATA

#Convert any cell with a '?' or a ' ?' value to a NA value.
df[df == '?'] <- NA

#Using table() on a column with NA values should now not 
#display those NA values, instead you'll just see 0 for ?. Optional: Refactor these columns
table(df$type_employer)
df$type_employer <- factor(df$type_employer)
df$marital <- factor(df$marital)
df$country <- factor(df$country)
df$occupation <- factor(df$occupation)

#Play around with the missmap function from the Amelia package
missmap(df)
missmap(df, y.at = c(1), y.labels = c(''), col = c('yellow','black'))

#you probably also noticed that there is a bunch of y labels with missing values, get rid of them
#Use na.omit() to omit NA data from the adult data frame.
df <- na.omit(df)
str(df)
missmap(df, y.at = c(1), y.labels = c(''), col = c('yellow','black'))

# EDA

#Use ggplot2 to create a histogram of ages, colored by income.
ggplot(df, aes(age)) + geom_histogram(aes(fill = income), col = 'black', binwidth = 1) + theme_bw()

#Plot a histogram of hours worked per week
ggplot(df, aes(hr_per_week)) + geom_histogram(fill = 'cyan', col = 'black') + theme_bw()

#Rename the country column to region column to better reflect the factor levels.
df <- rename(df, 'region' = country)

#Create a barplot of region with the fill color defined by income class. 
ggplot(df, aes(region)) + geom_bar(aes(fill = income)) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))

# BUILDING THE DATA

#Split the data into a train and test set
set.seed(101)
splt <- sample.split(df$income, SplitRatio = 0.7)
train <- subset(df, splt == T)
test <- subset(df, splt == F)

model <- glm(income ~., train, family = binomial(logit))
summary(model)

#Use new.model <- step(your.model.name) to use the step() function to create a new model.
new.model <- step(model)
summary(new.model)

test$predicted <- predict(new.model, test, type = 'response')
test$predicted <- ifelse(test$predicted>0.5, '>50K', '=<50K')
#confusion table
table(test$income, test$predicted)

#accuracy
(6372+1423)/(6372+548+872+1423)
#recall
6372/(6372+548)
#precision
6372/(6372+872)
