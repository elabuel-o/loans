
##----------------------------------------------------------------------------
## Title: Loans data analysis project (data setup)
## Author: Armando Enriquez Z.
## Date: November 23rd, 2014
## Purpose: Cleaning and preparing data for analysis
##----------------------------------------------------------------------------

## Loading data frame and random subsetting

### original data set of 161233 observations
loansData <- read.csv("LoanStats3c.csv", header = TRUE, skip = 1)

### random sample of the original data set
set.seed(123) ## for reproducibility
loans <- loansData[sample(1:nrow(loansData), 4529, replace = FALSE), ] 
names(loans)
rm(loansData)

### there are useless variables: let us drop them
drops <- c("url", "desc", "emp_title", "title")
loans <- loans[, !(names(loans) %in% drops)]

### variables that are coded as factors ---> numeric and/or character
loans$int_rate <- as.numeric(gsub("%", "", loans$int_rate))
loans$revol_util <- as.numeric(gsub("%", "", loans$revol_util))
loans$state <- loans$addr_state
loans$state <- as.character(loans$state)

## Visualizations (first)

library(ggplot2)
library(maps)

### a choropleth map
states <- map_data("state") ## geographic data 

### the loans data set contains only state abbreviations
### let's change abbreviations to full names in order to merge the data frames
state.names <- unlist(sapply(loans$state, function(x) 
        if(length(state.name[grep(x, state.abb)]) == 0) "District of Columbia" 
                             else state.name[grep(x, state.abb)]))

loans$state <- tolower(state.names)
colnames(loans)[49] <- "region" ## now both data frames have the same var name



## Exploratory graphics
library(ggplot2)

### scatter plots
ggplot(loansData, aes(x = request, y = funded, 
                      colour = home)) + geom_point()

ggplot(loansData, aes(x = request, y = Amount.Funded.By.Investors, 
                      colour = Loan.Length)) + geom_point()

ggplot(loansData, aes(x = ficoHigh, y = interestNum, 
                      colour = months)) + geom_point() + 
  stat_smooth(method = lm, level = 0.99)

### histograms and density plots
ggplot(loansData, aes(x = interestNum, fill = months)) + 
  geom_histogram(binwidth = 0.6, position = "identity", alpha = 0.4)

ggplot(loansData, aes(x = interestNum, fill = months)) + 
  geom_line(stat = "density")
