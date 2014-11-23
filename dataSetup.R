
## Loading data frame

load("loansData.rda")
str(loansData)

###############################################################################
## Exploratory graphics
library(ggplot2)

ggplot(loansData, aes(x = Amount.Requested, y = Amount.Funded.By.Investors)) + 
         geom_point()

ggplot(loansData, aes(x = Amount.Requested, y = Amount.Funded.By.Investors, 
                      colour = Home.Ownership)) + geom_point()

ggplot(loansData, aes(x = Amount.Requested, y = Amount.Funded.By.Investors, 
                      colour = Loan.Length)) + geom_point()

###############################################################################
## Dataset setup
names(loansData) <- c("request", "funded", "interest", "months", "purpose",
                      "debtIncome", "state", "home", "income", "fico", 
                      "creditLines", "creditBal", "inquiries", "employ")

### substring character and convert to numeric
### Variables: interest, debtIncome and fico
library(gsubfn)

## substring w/ regular expressions:
interest <- loansData[, 3]
interest <- strapplyc(interest, "(.*)%", simplify = TRUE)
interest <- as.vector(interest)
interest <- as.numeric(interest)
str(interest)