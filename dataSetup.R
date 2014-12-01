
##----------------------------------------------------------------------------
## Title: Loans data analysis project (data setup)
## Author: Armando Enriquez Z.
## Date: November 23rd, 2014
## Purpose: Cleaning and preparing data for analysis
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Loading data frame and random subsetting
load("loansData.rda")
names(loansData) <- c("request", "funded", "interest", "months", "purpose",
                      "debtIncome", "state", "home", "income", "fico", 
                      "creditLines", "creditBal", "inquiries", "employ")

### variables that are coded as factors ---> numeric and/or character
loansData$interest <- as.numeric(gsub("%", "", loansData$interest))
loansData$debtIncome <- as.numeric(gsub("%", "", loansData$debtIncome))
loansData$state <- as.character(loansData$state)
loansData$fico <- as.numeric(substr(loansData$fico, 1, 3))

##-----------------------------------------------------------------------------
## Map Visualization (first)
library(ggplot2)
library(maps)

states <- map_data("state")

### the loansData dataframe contains only state abbreviations
### the states dataframe contains the full state names
### let's change abbreviations to full names in order to merge the data frames
state.names <- unlist(sapply(loansData$state, function(x) 
        if(length(state.name[grep(x, state.abb)]) == 0) "District of Columbia" 
                             else state.name[grep(x, state.abb)]))

loansData$state <- tolower(state.names)
colnames(loansData)[7] <- "region" ## same names in both data frames

### determining number of loans and merging to new data frame with geo data

### merging data frames
stateLoans <- data.frame(table(loansData$region)) ## counting frequencies
colnames(stateLoans) <- c("region", "numLoans")
result <- merge(stateLoans, states)
result <- result[order(result$order), ]

### the map itself (preliminar)
ggplot(result, aes(x = long, y = lat, group = group, fill = numLoans)) + 
        geom_polygon(colour = "black") + coord_map("polyconic")

### there's a problem: there are no loans in Idaho, Nebraska, North Dakota,
### Tennessee and Maine. Let's fix the problem

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

### Tennessee 
ten <- map_data("state")[grep("tennessee", map_data("state")[, 5]), ]
ten$numLoans <- 1
result <- rbind(result, ten)
result <- result[order(result$order), ]

### Maine
maine <- map_data("state")[grep("maine", map_data("state")[, 5]), ]
maine$numLoans <- 1
result <- rbind(result, maine)
result <- result[order(result$order), ]

## Removing auxiliary objects
remove(idaho, maine, nd, nebraska, stateLoans, states, ten, state.names)

## Log of the numLoans
result$logLoans <- log(result$numLoans)
loansData$Plazo <- loansData$months

### the map (again)
library(tikzDevice)
tikz(file = "USmap.tex", standAlone = TRUE)
ggplot(result, aes(x = long, y = lat, group = group, fill = logLoans)) +
        geom_polygon(colour = "black") + 
        scale_fill_gradient(low = "gray85", high = "black") +
        coord_map("polyconic") + xlab("") + ylab("") + 
        labs(fill = "Número de créditos\n(logs)")
dev.off()

##-----------------------------------------------------------------------------
## Exploratory graphics

### scatter plots
ggplot(loansData, aes(x = fico, y = interest, 
                      colour = Plazo)) + 
        geom_point(position = position_jitter(width = 0.7, height = 0.3)) +
        stat_smooth(method = lm, level = 0.99, fullrange = TRUE) +
        xlab("Calificación Crediticia") + ylab("Tasa de Interés")

ggplot(loansData, aes(x = debtIncome, y = interest)) + 
        geom_point(position = position_jitter(width = 0.7, height = 0.3)) + 
        stat_smooth(method = lm, level = 0.99, fullrange = TRUE) + 
        xlab("Relación deuda/ingreso") + ylab("Tasa de Interés")

### histograms and density plots
ggplot(loansData, aes(x = interest, fill = Plazo)) + 
  geom_histogram(binwidth = 0.73, position = "identity", alpha = 0.4) + 
        xlab("Tasas de Interés por plazo") + ylab("Frecuencia")

ggplot(loansData, aes(x = interest, y = ..density..)) + 
        geom_histogram(binwidth = 0.73, fill = "cornsilk", 
                       colour = "gray40", size = 0.7) + 
        geom_density(size = 0.7) + facet_grid(Plazo ~ .)

### boxplots

### reordering by median value
loansData$ordMedian <- with(loansData, reorder(home, funded, median))

### the boxplot itself
ggplot(loansData, aes(x = ordMedian, y = funded)) + 
        geom_boxplot() +
        geom_point(position = position_jitter(width = 0.25, height = 0.6), 
                   alpha = 0.5) + coord_flip() + 
        xlab("Cantidad otorgada por préstamo") + 
        ylab("Propiedad de la vivienda")

## reordering by median value (purpose)
loansData$ordMedian2 <- with(loansData, reorder(purpose, interest, median))

## the boxplot itself
ggplot(loansData, aes(x = ordMedian2, y = interest)) + 
        geom_boxplot() + 
        geom_point(position = position_jitter(width = 0.5, height = 0.5), 
                   alpha = 0.4) + coord_flip() +
        xlab("Tasa de Interés") + ylab("Propósito del crédito")

ggplot(loansData, aes(x = ordMedian, y = interest)) + 
        geom_boxplot() + 
        geom_point(position = position_jitter(width = 0.25, height = 0.6), 
                   alpha = 0.5) + coord_flip()