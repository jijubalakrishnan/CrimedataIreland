# The crime dataset for this analysis is in csv fie format
# The dataset is imported into R.
# importing the library for performing sql operations
# importing ohter libraires required for this analysis
library(tidyverse)
install.packages("sqldf")
install.packages("ggplot2")
library("sqldf")
library(dplyr)
library("ggpubr")
library(tidyverse) ## For data wrangling and visualization 
library(lubridate) ## To work with dates
library(ggpubr)    ## Extra visualizations and themes
library(patchwork) ## Patch visualizations together
library(hrbrthemes)## extra themes and formatting
library(scales)    ## For formatting numeric variables
library(tidytext)  ## Reordering within facets in ggplot2
library(pier)      ## Make interactive piecharts in R
library(ggalt)     ## Extra visualizations
getwd()
install.packages("ggrepel")
# The file can be stored inside the working directory
# Thus the file can be read directly
data <- read.csv("irelandcrime.csv")
getwd()
data2 <- read.csv("unemployment.csv")
str(data)
# The dataframe has got 1624 observations in 72 variables
# These variables include region,garda office,offense name,offense code,
# Type of offence and quarterly crime numbers from 2003 to 2019
# An intial plotting of this data gives an overall idea about
# overall crime scenario in republic of ireland.
# viewing the head of the data

head(data)
# Viewing the glimpse of the data
glimpse(data)
# Viewing the summary of data
summary(data)
nlevels(data$REGION)
#Numerical data in the dataframe
numeric_variable_list <- sapply(data,is.numeric)
numerical_data <- data[numeric_variable_list]
# Finding the different types of offences recorded in the dataframe
levels(data$OFFENCE)
# There are 58 types of offences recorded in this dataframe
# plotting the overall trend
# The quarterly data is filterd out and stored in a new dataframe
plotcrime <- data[c(6:72)]
plot(x=c(1:67),y=colSums(plotcrime),xlab="quarterly",ylab = "totalcrime")
x <- c(1:67)
y <- colSums(plotcrime)
mod <- lm(y~x)
pre <- predict(mod)
lines(pre)
plot(x=c(1:67),y=colSums(plotcrime),xlab="quarterly",ylab = "totalcrime")
x <- c(1:67)
y <- colSums(plotcrime)
mod <- lm(y~I(x^3))
pre <- predict(mod)
lines(pre,col="blue")
          # Formal test of normality
          normality_test <- shapiro.test(y)
          normality_test$p.value
# Since P value is very much less than 0.05 it is not normally distributed.

crime_by_type <- sqldf("select OFFENCE,sum(x2015Q1),sum(x2015Q2),sum(x2015Q3),sum(x2015Q4),sum(x2016Q1),sum(x2016Q2),sum(x2016Q3),sum(x2016Q4),sum(x2017Q1),sum(x2017Q2),sum(x2017Q3),sum(x2017Q4),sum(x2018Q1),sum(x2018Q2),sum(x2018Q3),sum(x2018Q4),sum(x2019Q1),sum(x2019Q2),sum(x2019Q3) from data group by OFFENCE")
type_plot <- data.frame(crime_by_type$OFFENCE,rowSums(crime_by_type[,-c(1)]))
names(type_plot)[1] <- "Offence"
names(type_plot)[2] <- "crimes"
# ordering by number of crimes
type_plot <- type_plot[order(type_plot$crimes,decreasing = TRUE),]
type_plot <- type_plot[1:10,]

type_plot %>%
  ggplot(aes(x=Offence, y=crimes,main="high offence list")) + 
  geom_bar(stat = "identity",fill = "darkred" ) +
  coord_flip()
# this plot gives generl trend of crimes in ireland
# on qurterly years
# The data has to be filterd by region
# grouping the dataframe by region
grouped_data <- group_by(data,REGION)
#calculating the regionwise crime from recent years
regionwisecrime <- sqldf("select REGION, sum(x2015Q1),sum(x2015Q2),sum(x2015Q3),sum(x2015Q4),sum(x2016Q1),sum(x2016Q2),sum(x2016Q3),sum(x2016Q4),sum(x2017Q1),sum(x2017Q2),sum(x2017Q3),sum(x2017Q4),sum(x2018Q1),sum(x2018Q2),sum(x2018Q3),sum(x2018Q4),sum(x2019Q1),sum(x2019Q2),sum(x2019Q3) from data group by REGION")
rowSums(regionwisecrime[,-c(1)])
#storing the regionwise crime data
modeldata1 <- data.frame(regionwisecrime$REGION,rowSums(regionwisecrime[,-c(1)]))
#renaming the columns
names(modeldata1)[1] <- "REGION"
names(modeldata1)[2] <- "Totalcrime"
modeldata1 %>%
  ggplot(aes(x=REGION, y=Totalcrime)) + 
  geom_bar(stat = "identity") +
  coord_flip()
names(data)
# Filtering a particular type of crime for analysis and prediction
filter<- data$TYPE.OF.OFFENCE=="SEXUAL OFFENCES"
prediction_model <- data[filter,]
prediction_model <-sqldf("select sum(x2015Q1),sum(x2015Q2),sum(x2015Q3),sum(x2015Q4),sum(x2016Q1),sum(x2016Q2),sum(x2016Q3),sum(x2016Q4),sum(x2017Q1),sum(x2017Q2),sum(x2017Q3),sum(x2017Q4),sum(x2018Q1),sum(x2018Q2),sum(x2018Q3),sum(x2018Q4),sum(x2019Q1),sum(x2019Q2),sum(x2019Q3) from data ")
# plotting the prediction over time
years <- c(2015,2016,2017,2018)
crime_number <- c(prediction_model$`sum(x2015Q1)`+prediction_model$`sum(x2015Q2)`+prediction_model$`sum(x2015Q3)`+prediction_model$`sum(x2015Q4)`,prediction_model$`sum(x2016Q1)`+prediction_model$`sum(x2016Q2)`+prediction_model$`sum(x2016Q3)`+prediction_model$`sum(x2016Q4)`+prediction_model$`sum(x2016Q4)`,prediction_model$`sum(x2017Q1)`+prediction_model$`sum(x2017Q2)`+prediction_model$`sum(x2017Q3)`+prediction_model$`sum(x2015Q4)`+prediction_model$`sum(x2017Q4)`,prediction_model$`sum(x2018Q1)`+prediction_model$`sum(x2018Q2)`+prediction_model$`sum(x2018Q3)`+prediction_model$`sum(x2018Q4)`+prediction_model$`sum(x2018Q4)`)
my_data <- data.frame(years,crime_number)
ggscatter(my_data, x = "years", y = "crime_number", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Years", ylab = "Cases",col="red")
# The overall crime rate in the case of SEXUAL OFFENCES is consistent in Ireland from the period 2015 to 2018
# Comparing the crime rate over time based on its type
# preparation of dataset for statistical testing 
stat_test_data <- data[c(6:45)]
stat_test_data2 <- data.frame(colSums(stat_test_data))
stat_test_data <- stat_test_data2
names(stat_test_data)[1] <- "crimes"
unemployment_rate <- data2$National.Unemployment.Rate.SA....
# extracting the required number of columns from second dataframe
# rowwise filtering is done
data2[22:66,2]
unemployment_rate <- data2[22:61,2]
# This value has to be appended to the first dataframe
stat_test_data <-data.frame(stat_test_data,unemployment_rate)
# Since the relavant data is extracted, correlation tests can be conducted
v1 <- stat_test_data$unemployment_rate
v2 <- stat_test_data$crimes
corvalues <- c(cor(v1,v2 = "kendall", use = "complete.obs"),
               cor(v1,v2,method = "spearman", use = "complete.obs"),
               cor(v1,v2,method = "pearson", use = "complete.obs"))
corvalues
tests <- c("kendall","spearman","pearson")
names(corvalues)<- tests
corvalues
which.max(stat_test_data$crimes)
# scattering unemployment rate vs crime rate
ggplot(stat_test_data, aes(x = unemployment_rate , y = crimes )) +
  geom_point() +
  stat_smooth()
wilcox <- wilcox.test(v1,v2)
wilcox
