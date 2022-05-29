library(ggplot2)
library(ggthemes)

df <- read.csv('bikeshare.csv')
print(head(df))

#Create a scatter plot of count vs temp. Set a good alpha value
print(ggplot(df, aes(temp, count)) + geom_point(size = 3, aes(col = temp), alpha = 0.2) + 
        theme_clean())

#Plot count versus datetime as a scatterplot with a color gradient based on temperature.
#You'll need to convert the datetime column into POSIXct before plotting.
df$datetime <- as.POSIXct(df$datetime)
print(ggplot(df, aes(datetime, count)) + geom_point(size = 3, aes(col = temp), alpha = 0.5) + 
        theme_clean() + scale_color_gradient(low = '#00FFFF', high = '#F59B42'))

#What is the correlation between temp and count?
print(cor(df[, c("temp", "count")]))

# Create a boxplot, with the y axis indicating count and the x axis begin a box for each season.
df$season <- factor(df$season)
print(ggplot(df, aes(season, count)) + geom_boxplot(aes(col = season)) + theme_clean())

#Create an "hour" column that takes the hour from the datetime column. 
df$hour <- format(df$datetime,"%H")

#Now create a scatterplot of count versus hour, with color scale based on temp. Only use bike 
#data where workingday==1.
print(ggplot(df[df$workingday==1,], aes(hour, count)) + geom_point(aes(col = temp), size = 3, 
                    alpha = 0.5, position=position_jitter(w=1, h=0)) + scale_color_gradientn(
                    colors = c('blue', 'green', 'yellow', 'red')) + theme_clean())

#Now create the same plot for non working days:
print(ggplot(df[df$workingday==0,], aes(hour, count)) + geom_point(aes(col = temp), size = 3, 
                    alpha = 0.5, position=position_jitter(w=1, h=0)) + scale_color_gradientn(
                      colors = c('blue', 'green', 'yellow', 'red')) + theme_clean())
#Use lm() to build a model that predicts count based solely on the temp feature.
lm.temp <- lm(count ~ temp, df)
print(summary(lm.temp))

#How many bike rentals would we predict if the temperature was 25 degrees Celsius? Calculate this two ways:
#1. Using the values we just got above
lm.temp$coefficients[1] + 25*lm.temp$coefficients[2]
#2.Using the predict() function
print(predict(lm.temp, data.frame(temp = 25)))

#Use sapply() and as.numeric to change the hour column to a column of numeric values.
df$hour <- sapply(df$hour, as.numeric)

#Finally build a model that attempts to predict count based off of the following features.
#season, holiday, workingday, weather, temp, humidity, windspeed, hour
model <- lm(count ~ . - casual - registered - datetime - atemp, df)
print(summary(model))
