
## Regression models of Loans data (Lending Club)
lm1 <- lm(interest ~ fico + log(income) +  months, data = loansData)
summary(lm1)
stats1 <- cbind(Estimate = coef(lm1), confint(lm1)) ## stata style matrix

lm2 <- update(lm1, ~. + fico:months + log(income):months)
stats2 <- cbind(Estimate = coef(lm2), confint(lm2)) ## stata style matrix

lm3 <- update(lm1, ~. -months + debtIncome + purpose)
summary(lm3)

lm4 <- update(lm3, ~. -purpose + home)
summary(lm4)

library(car)
head(Prestige)

## Create a linear regression model

lm1 <- lm(prestige ~ education + log2(income) + women, data = Prestige)
summary(lm1) ## all are numeric variables

## Let us try with numeric and factor predictions (without interactions)

lm2 <- update(lm1, ~. - women + type)
summary(lm2)

## The "adjusted means" have a fancy interpretation
library(effects)
adj.means <- effect("type", lm2)

## Let us try the same model lm2, but with interactions
lm3 <- update(lm2, ~ . + log2(income):type + education:type)
summary(lm3)

