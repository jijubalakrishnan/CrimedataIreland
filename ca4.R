# ARIMA Analysis on crime data ireland
getwd()
data <- read.csv("irelandcrime.csv")
View(data)
# The data is ranging from 2003 first quarter to 2019 first quarter
# There are 67 columns which have quarterly crime values for different crimes
# By finding the columns sum we can find the overall crime in each these quarterly years
x <- data[c(6:72)]
x <- colSums(x)
# Which was the period when there was maximum crime in ireland
which.max(x)
# which was the year when there was minimum crime in ireland
which.min(x)
# The mean crime rate in Ireland
mean(x)
# The median crime rate in Ireland
median(x)
y <-c(1:67)
#plotting the crime rate
plot(y,x)
# Arima model prdiction is implimented on the above data
# For this Tsstudio package is installed
install.packages("TSstudio")
library(TSstudio)
predictiondata <- data.frame(y,x)
names(predictiondata)[1] <- "year"
names(predictiondata)[2] <- "crimes"
# converting the dataframe into a time series object
timeseriesdata <- ts(predictiondata$crimes,frequency =4,start=c(2003,1))
timeseriesdata
# It is required to split the data to test and train
split_predictiondata <- ts_split(ts.obj = timeseriesdata, sample.out = 23)
# Checking the type of the timeseries data
class(timeseriesdata)
# Training and testing
training <- split_predictiondata$train
testing <- split_predictiondata$test
paste("crime time series:" , length(timeseriesdata))
paste("length of training data:" , length(split_predictiondata$train))
paste("length of testing data:" , length(split_predictiondata$test))
library(forecast)
#Implimenting the ARIMA model
arimamodel <- auto.arima(training)
arimamodel
# Testing the model
test_model <- Arima(testing, model=arimamodel)
accuracy(test_model)
# Predicting the result using the model
prediction <- predict(arimamodel, n.ahead = 3 * 12)
prediction$pred
# Thus the crime rate ahead of three years is predicted using Arima model
# Comparing the model with actual data
# Used for Jupyter notebook only
options(repr.plot.width=16, repr.plot.height=10)

# Show both side-by-side for comparison
opar <- par(no.readonly=TRUE)

plot(prediction$pred, 
     lty=1, 
     pch="o", 
     col = "Blue", 
     main = "Comparison of Crimerate prediction and testing model")

# Plot second dataset
lines(testing, 
      col="red", 
      pch="*", 
      lty=2)

legend(1,19, legend=c("Prediction","Testing"), col=c("red","blue"),
       pch=c("o","*"),lty=c(1,2), ncol=1)

par(opar)

# # Predicting a particular type of crime in a particular region
# # At a future point
# # As an example endangering traffic offence in donegal region is predicted here
# # For this data has to be filtered accordingly from the intial dataset
# data
# library(sqldf)
# donegalprediction <- sqldf("select *from data where OFFENCE='Endangering traffic offences'")
# names(donegalprediction)[2] <- "locality"
# donegalprediction <-sqldf("select *from donegalprediction where locality='DONEGAL'")
# # The data is filtered, however the numbers are too small
# # The ARIMA model is applied on this as well
# # For code reusability, the donegalprediction dataframe is loaded into
# # earlier timeseries data prediction
# x <- donegalprediction[c(6:72)]
# x <- colSums(x)
# # Which was the period when there was maximum crime in ireland
# which.max(x)
# # which was the year when there was minimum crime in ireland
# which.min(x)
# # The mean crime rate in Ireland
# mean(x)
# # The median crime rate in Ireland
# median(x)
# y <-c(1:67)
# #plotting the crime rate
# plot(y,x)
# # Arima model prdiction is implimented on the above data
# # For this Tsstudio package is installed
# install.packages("TSstudio")
# library(TSstudio)
# predictiondata <- data.frame(y,x)
# names(predictiondata)[1] <- "year"
# names(predictiondata)[2] <- "crimes"
# # converting the dataframe into a time series object
# timeseriesdata <- ts(predictiondata$crimes,frequency =4,start=c(2003,1))
# timeseriesdata
# split_predictiondata <- ts_split(ts.obj = timeseriesdata, sample.out = 23)
# # Checking the type of the timeseries data
# class(timeseriesdata)
# # Training and testing
# training <- split_predictiondata$train
# testing <- split_predictiondata$test
# paste("crime time series:" , length(timeseriesdata))
# paste("length of training data:" , length(split_predictiondata$train))
# paste("length of testing data:" , length(split_predictiondata$test))
# library(forecast)
# #Implimenting the ARIMA model
# arimamodel <- auto.arima(training)
# arimamodel
# # Testing the model
# test_model <- Arima(testing, model=arimamodel)
# accuracy(test_model)
# # Predicting the result using the model
# prediction <- predict(arimamodel, n.ahead = 3 * 12)
# prediction$pred