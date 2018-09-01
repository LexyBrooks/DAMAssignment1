
#note that correct working directory is setwd("/Users/alex/Projects/DAMAT1")
#load libraries 
#library(tidyverse)
#library(modelr)
library(readr)
library(ggplot2)
library(dplyr)
#library(ggExtra)
#library(dygraphs)


### AT1 Spring 2018: Part A
## Data Cleaning and Exploratory Data Analysis on "transactions.csv" dataset
#I will explore the data using predictor variables like date, location, industry and customer_id.

#import dataset using read.csv
transactions <- read_csv("transactions.csv")


# 1. a. Cleaning the data

#It was noted that readr did not identify the date column as a data type of date. In order to give the date colum a correct datatype of date, the parse_date function is used to parse the values in the date colum as dates.

#To ensure that any other artefacts are cleaned up, a new 'clean' csv is written and read back in (overwriting the original transaction variable) and the column types are explicitly set.

# Use the date parser to get the date data into an understandable format
transactions$date <- parse_date(transactions$date, format="%d/%m/%y")

transactions <- as.data.frame(transactions)

# Write a new 'clean' csv with correct date formats and remove any artefacts from opening in Excel
write_csv(transactions, "transactions_clean.csv")



# Read in the clean data, overwriting transactions and explicitly declaring the data types for each column
transactions <- read_csv("transactions_clean.csv",
                         col_types = cols(
                           date = col_date(format = ""),
                           customer_id = col_character(),
                           industry = col_integer(),
                           location = col_integer(),
                           monthly_amount = col_number()
                           )
                         )
# Verify transactions data is using the correct data types
spec(transactions)
# View the data structure
transactions <- as.data.frame(transactions)

#1. b. Description of 2 insights just from EDA

##show histograms of industry and location variables

par(mfrow=c(1,2))
hist(transactions$industry, main = "Histogram of number of transactions by Industry", xlab="Industry", ylab="Number of transactions", xlim = c(0,10), ylim=c(0,50000), las=0)
hist(transactions$location, main = "Histogram of number of transactions by Location", xlab="Location", ylab="Number of transactions", xlim = c(0,10), ylim=c(0,50000), las=0)

# More visual exploration
#It looks like locations one and two account for the greatest majority of monthly transactions and there's something about industry code 6 that's very distinct from the other industries.

# Let's mutate the dataset to create a new column called 'monthly_amount_above_mean'
transactions <- transactions %>%
  # creating a new variable to classify transaction column
  mutate(monthly_amount_above_mean = ifelse(monthly_amount > mean(monthly_amount), TRUE, FALSE))


# plot a bar chart by monthly amounts by date
qplot(x = date, fill = monthly_amount_above_mean, data = transactions, geom = "bar", main = "Number of transactions by month, showing proportion above the mean")


#This reveals most monthly amounts are below the mean and the monthly amounts grow steadily rather than steeply. Let's now look at it by industry and location, too

# plot a bar chart by monthly amounts by location
qplot(x = location, fill = monthly_amount_above_mean, data = transactions, geom = "bar", main = "Number of transactions by location, showing proportion above the mean")

#So what about by industry?

# plot a bar chart by monthly amounts by industry
qplot(x = industry, fill = monthly_amount_above_mean, data = transactions, geom = "bar", main = "Number of transactions by industry, showing proportion above the mean")

##Some insights
#Location 1 and 2 have the highest volumes of monthly_amounts, as well as the most above-average monthly amounts.

#Industry 6 has the least amount of sales, but when they have them, they are all above the mean of all sales amounts.

#Industries 3, 5 and 9 look to have a greater proportion of their transactions above the mean average monthly amount

#Let's aggregate the data and see if we can get more insights

# Aggregate the data, grouping by date, industry and location, and calculating the mean monthly_amount
aggregated_transactions <- transactions %>%
  group_by(date, industry, location) %>%
  summarize(monthly_amount = mean(monthly_amount, na.rm = TRUE))

# Let's also create a column for the month number and another one for year number
aggregated_transactions <- aggregated_transactions %>%
  mutate(month_number = format(as.Date(date), "%m"))
#set the data type of month_number to integer
aggregated_transactions$month_number <- as.integer(aggregated_transactions$month_number)

#transform(aggregated_transactions, month_number = as.integer(month_number), year_number = as.integer(year_number))

# View the data
aggregated_transactions

#let's plot the location variable against the average monthly amount , ylim=c(0,1000000)
#industry_boxplot <- boxplot(mean_monthly_amount ~ industry, data = aggregated_transactions) +
#      geom_point()


#let's try to make a time series plot
#library(ggfortify)
#autoplot(~ monthly_amount) + labs(title="Monthly Sales Amount")


#let's visualise some of the first import of the data as a scatterplot
#ggplot(transactions, aes(x=industry, y=monthly_amount, color=location)) + geom_point() + geom_smooth()


#let's visualise the second data import transactions as a scatterplot to see if there's a difference
#ggplot(transactions, aes(x=industry, y=monthly_amount, color=location)) + geom_point() + geom_smooth()

#No difference! These plots are identical, even though we are using two slightly different column structures of the same data.

# More exploration using GGPLOT2 & Dp

#That's a slightly ridiculous plot, but it shows there is something going up with the industry number 6:

#Take the column "industry" and make a histogram of it
qplot(transactions$industry, geom="histogram", binwidth=0.5) 

#The histogram shows the industry variable is front weighted, I THINK to a poisson distribution. Still not super helpful

#Take a different column "location" and make a histogram of it
#qplot(transactions$location, geom="histogram", binwidth=0.5) 


#get the sum of all monthly amounts by location
barplot <- ggplot(transactions, aes(x=monthly_amount, y=location)) + geom_bar(stat = "identity")

#another scatterplot
ggplot(transactions) + geom_point(aes(x=location, y=monthly_amount, color=industry)) + geom_smooth(aes(x=location, y=monthly_amount, color=industry)) # Specifying the aesthetics inside the geoms.

aggregated_transactions <- aggregated_transactions[c("monthly_amount", "date", "industry", "location", "month_number")]
aggregated_transactions <- as.data.frame(aggregated_transactions)

#2. a. ii. Create a line plot of the variable monthly_amount, for industry = 1 and location = 1
ggplot(data=filter(aggregated_transactions, industry == 1 & location == 1), aes(x=date, y=monthly_amount, group=1)) + geom_line(color="red") + geom_point() + labs(title="Line Plot of mean monthly amount for Industry = 1 and Location = 1", x="Date", y="Mean Monthly Amount")

testData <- filter(aggregated_transactions, industry == 1 & location == 1)
testingSet <- filter(testData, month_number == 12)
trainingSet <- filter(testData, month_number != 12)

summary(aggregated_transactions)

par(mfrow = c(2,2))
counter = 0
for (variable in colnames(aggregated_transactions[, -1])) {
  plot(aggregated_transactions[, variable], aggregated_transactions$monthly_amount, main = variable, ylab = "monthly amount", xlab = variable)
  counter = counter + 1
  if (counter %% 4 == 0) {
    readline(prompt = "Hit ENTER to show more plots")
  }
}

ggplot(trainingSet, aes(date, monthly_amount)) + geom_point() +
  geom_line(data=trainingSet, aes(x = date, y = monthly_amount), size=1, color="red")

# Train a linear regression model on all the predictors (using '.' in the formula does this)
#data.lm = lm(formula = monthly_amount ~ month_number, data = dataSet)
# Analyse the model output, take note of the p-values and Adjusted R-squared
#data.lm
# plot the regression diagnostics.  Note the red line on two left plots is not straight
#plot(data.lm)

#trainingSet <- filter(dataSet, month_number != 12)
#testingSet <- filter(dataSet, month_number == 12)

myLinearModel <- lm(formula = monthly_amount ~ date, data=trainingSet)

summary(myLinearModel)

prediction = predict(myLinearModel, testingSet)

testingSet$prediction = prediction

delta <- prediction - testingSet$monthly_amount

delta

testingSet$prediction

ggplot(testingSet, aes(date, monthly_amount)) + geom_point() +
  geom_line(data=testingSet, aes(x = date, y = monthly_amount), size=1, color="red")


predictedData <- predict(myLinearModel, newdata=testingSet)

predictedData

delta <- predictedData - testingSet$monthly_amount

delta

simple.fit
summary(simple.fit)



#rows <- nrow(dataSet)
#f <- 0.5
#upper_bound <- floor(f * rows)
#sampleDataSet <- dataSet[sample(rows), ]
#trainingSet <- sampleDataSet[1:upper_bound, ]
trainingSet <- filter(dataSet, month_number != "12")

#testingSet <- sampleDataSet[(upper_bound+1):rows, ]
testingSet <- filter(dataSet, month_number == "12")

length(testingSet)

plot(trainingSet$date,trainingSet$monthly_amount, main="test",
xlab="date", ylab="monthly amount")




myLM <- lm(monthly_amount ~ date, data = testingSet)
plot(trainingSet$date,trainingSet$monthly_amount)
abline(myLM)

myLinearModel

View(testingSet)
myLinearModel <- lm(monthly_amount ~ date, data = trainingSet)

myLinearModel

predictedData <- predict(myLinearModel, newdata=testingSet)

predictedData

delta <- predictedData - testingSet$monthly_amount

delta

t.test(delta, conf.level = 0.95)

plot(delta)


