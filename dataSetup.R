
##----------------------------------------------------------------------------
## Title: Loans data analysis project (data setup)
## Author: Armando Enriquez Z.
## Date: November 23rd, 2014
## Purpose: Cleaning and preparing data for analysis
##----------------------------------------------------------------------------

## Loading data frame

load("loansData.rda")
str(loansData)

###############################################################################
## Dataset setup
names(loansData) <- c("request", "funded", "interest", "months", "purpose",
                      "debtIncome", "state", "home", "income", "fico", 
                      "creditLines", "creditBal", "inquiries", "employ")

### substring character and convert to numeric
### variables: interest, debtIncome and fico
library(gsubfn)

### substring w/ regular expressions:
interest <- loansData[, 3] ## subsetting (variable number 3)
interest <- as.vector(interest)
interest <- strapplyc(interest, "(.*)%", simplify = TRUE) ## regexp
interest <- as.numeric(interest)
loansData$interestNum <- interest

debtIncome <- loansData[, 6] ## subsetting (variable number 6)
debtIncome <- as.vector(debtIncome)
debtIncome <- strapplyc(debtIncome, "(.*)%", simplify = TRUE) ## regexp
debtIncome <- as.numeric(interest)
loansData$debtIncomeNum <- debtIncome

### for FICO rate, we will take two variables (low and high rate)
fico <- loansData[, 10] ## subsetting (variable number 10)
fico <- as.vector(fico)
fico.low <- substr(fico, 1, 3)
fico.high <- substr(fico, 5, 7)
ficoLow <- as.numeric(fico.low)
ficoHigh <- as.numeric(fico.high)
loansData$ficoLow <- ficoLow
loansData$ficoHigh <- ficoHigh

###############################################################################
## Exploratory graphics
library(ggplot2)

ggplot(loansData, aes(x = request, y = funded, 
                      colour = home)) + geom_point()

ggplot(loansData, aes(x = request, y = Amount.Funded.By.Investors, 
                      colour = Loan.Length)) + geom_point()

