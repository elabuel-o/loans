
##----------------------------------------------------------------------------
## Title: Loans data analysis project (data setup)
## Author: Armando Enriquez Z.
## Date: November 23rd, 2014
## Purpose: Cleaning and preparing data for analysis
##----------------------------------------------------------------------------

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

##-----------------------------------------------------------------------------
## Map Visualization (first)

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

### determining number of loans and merging to new data frame with geo data

### merging data frames
stateLoans <- data.frame(table(loans$region)) ## counting frequencies
colnames(stateLoans) <- c("region", "numLoans")
result <- merge(stateLoans, states, by = "region")
result <- result[order(result$order), ]

### map itself (preliminar)
ggplot(result, aes(x = long, y = lat, group = group, fill = numLoans)) + 
        geom_polygon(colour = "black") + coord_map("polyconic")

### there's a problem: there are no loans in Idaho, Nebraska, North Dakota,
### Iowa and Maine. Let's fix the problem

### Idaho
idaho <- map_data("state")[grep("idaho", map_data("state")[, 5]), ]
idaho$numLoans <- 1 ## useful for log transformations
result <- rbind(result, idaho)
result <- result[order(result$order), ]

### Nebraska
nebraska <- map_data("state")[grep("nebraska", map_data("state")[, 5]), ]
nebraska$numLoans <- 1
result <- rbind(result, nebraska)
result <- result[order(result$order), ]

### North Dakota
nd <- map_data("state")[grep("north dakota", map_data("state")[, 5]), ]
nd$numLoans <- 1
result <- rbind(result, nd)
result <- result[order(result$order), ]

### Iowa 
iowa <- map_data("state")[grep("iowa", map_data("state")[, 5]), ]
iowa$numLoans <- 1
result <- rbind(result, iowa)
result <- result[order(result$order), ]

### Maine
maine <- map_data("state")[grep("maine", map_data("state")[, 5]), ]
maine$numLoans <- 1
result <- rbind(result, maine)
result <- result[order(result$order), ]

### the map (again)
library(tikzDevice)
tikz(file = "USmap.tex", standAlone = TRUE)
ggplot(result, aes(x = long, y = lat, group = group, fill = numLoans)) +
        geom_polygon(colour = "black") + 
        scale_fill_gradient(low = "gray85", high = "black", trans = "log") +
        coord_map("polyconic") + xlab("") + ylab("") + 
        labs(fill = "Número de créditos\notorgados (logs)")
dev.off()

##-----------------------------------------------------------------------------
## Exploratory graphics

### scatter plots

ggplot(loans, aes(x = funded_amnt_inv, y = int_rate, 
                      colour = term)) + geom_point()

ggplot(loansData, aes(x = ficoHigh, y = interestNum, 
                      colour = months)) + geom_point() + 
  stat_smooth(method = lm, level = 0.99)

### histograms and density plots
ggplot(loansData, aes(x = interestNum, fill = months)) + 
  geom_histogram(binwidth = 0.6, position = "identity", alpha = 0.4)

ggplot(loansData, aes(x = interestNum, fill = months)) + 
  geom_line(stat = "density")


