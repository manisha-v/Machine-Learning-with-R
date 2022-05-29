library(cluster) # clusplot()
# kmeans function is in stats library
library(ggplot2)
library(ggthemes)

df1 <- read.csv('CSV files for ML Projects\\winequality-red.csv', sep = ';')
df2 <- read.csv('CSV files for ML Projects\\winequality-white.csv', sep = ';')

#add a label column to both df1 and df2 indicating a label 'red' or 'white'

df1$label <- 'red'
df2$label <- 'white'

#Combine df1 and df2 into a single data frame called wine

df <- rbind(df1,df2)
str(df)

####
## EDA
####

#Create a Histogram of residual sugar from the wine data. Color by red and white wines.

ggplot(df, aes(residual.sugar)) + geom_histogram(aes(fill = factor(label)), col = 'black', 
                                  bins = 50) + scale_fill_manual(values = c('red', 'white'))

#Create a Histogram of citric.acid from the wine data. Color by red and white wines.

ggplot(df, aes(citric.acid)) + geom_histogram(aes(fill = factor(label)), col = 'black', 
                                bins = 50) + scale_fill_manual(values = c('red', 'white'))

#Create a Histogram of alcohol from the wine data. Color by red and white wines.

ggplot(df, aes(alcohol)) + geom_histogram(aes(fill = factor(label)), col = 'black', 
                                bins = 50) + scale_fill_manual(values = c('red', 'white'))

#Create a scatterplot of residual.sugar versus citric.acid, color by red and white wine.

ggplot(df, aes(citric.acid, residual.sugar)) + geom_point(aes(col = factor(label)), alpha = 0.5,
                     size = 3) + scale_colour_manual(values = c('red', 'white')) + theme_dark()

#Create a scatterplot of volatile.acidity versus residual.sugar, color by red and white wine.

ggplot(df, aes(volatile.acidity, residual.sugar)) + geom_point(aes(col = factor(label)), alpha = 0.5,
                     size = 3) + scale_colour_manual(values = c('red', 'white')) + theme_dark()

#Grab the wine data without the label and call it clus.data

clus.data <- df[,-13]
head(clus.data)

###########
###### BUILDING THE CLUSTERS
###########

#Call the kmeans function on clus.data and assign the results to wine.cluster.

set.seed(10)
wine.clus <- kmeans(clus.data, 2, nstart = 15)

#Print out the wine.cluster Cluster Means and explore the information.

wine.clus$centers

#############
##### EVALUATING THE CLUSTERS
#############

#Use the table() function to compare your cluster results to the real 
#results. Which is easier to correctly group, red or white wines?

table(df$label, wine.clus$cluster)
#red wine is easier to correctly group
