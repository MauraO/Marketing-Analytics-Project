### Set Working Directory HERE
setwd("C:/Users/Bhoomija/Dropbox/Teaching/Marketing Analytics/Datasets")
### Put in your own working directory address here

### Load packages HERE
library(tidyverse)
library(rmarkdown)

### Loading in data HERE
consumerDB = read_csv("~/Desktop/BUS-256/consumerDataFrame.csv")
summary(consumerDB)

## Creating new Variables, for. eg. - Price
consumerDB$price = consumerDB$dollars/consumerDB$units
### or alternatively
consumerDB = consumerDB %>% mutate(price = dollars/units)

##############################################################################
###
### HYPOTHESIS TESTING
###
###############################################################################

#### We can check if there is an association between price-levels and stores.
### If there is no association, i.e., all the stores have a "similar" distribution of 
### price-levels, the chi-squared test will fail to reject the null.
## Creating price-levels
consumerDB = consumerDB %>%
  mutate(category=cut(price, breaks=c(-Inf, 1, 2, Inf), 
                      labels=c("low","middle","high")))
## Creating two-way table of frequencies -- pivot table
table(consumerDB$STOREIdentifier, consumerDB$category)
## Using the frequency table for a chi-squared test
chisq.test(table(consumerDB$STOREIdentifier, consumerDB$category))

#### Test of correlation and Regression
### Correlation between units and dollars
cor(consumerDB$units, consumerDB$dollars)

### If we want to test if this correlation is different from 
### 0, we test via the cor.test() function
cor.test(consumerDB$units, consumerDB$dollars)

##############################################################################
###
### REGRESSIONS
###
###############################################################################

### We now want to run a regression of units and price, 
### i.e., a demand curve specification
consumerDB$price = consumerDB$dollars/consumerDB$units
demand_lm = lm(units~price, data = consumerDB)
## demand_lm is a "lm" object. One can retrieve the regression
## output with the summary() function.
summary(demand_lm)

### "demand_lm" is an R-object of type "lm". 
### names() function shows the names of the differenct components
### of the linear model within demand_lm
names(demand_lm)

### coefficients() or demand_lm$coefficients prints out the 
### coefficients of the demand model
coefficients(demand_lm)
demand_lm$coefficients

### "residuals" is the error term "epsilon" in the regression.
### We have a residual associated with each data observation
length(demand_lm$residuals)
summary(demand_lm$residuals)

### Similarly, "fitted.value" is the expected value of Y given the X
### That is, if the regression model is Y = b0 + b1X + e, 
### fitted value of Y = b0 + b1X
length(demand_lm$fitted.values)
summary(demand_lm$fitted.values)

### "call" tells us the model specification
demand_lm$call

### We can get the residual standard error, i.e., the dispersion
### of the residual epsilons. Remember in our regression model, 
### Y = b0 + b1X + epsilon,  the error term epsilon ~ N(0,sigma^2)
### This sigma is the dispersion in the residuals, and is estimated with 
### b0 and b1 in the regression linear model
sd(demand_lm$residuals)    ### This gives us the estimate for sigma
### This result is also present in the summary(demand_lm) table
### under "residual standard error".

lm(formula = units ~ price +STOREIdentifier, data=consumerDB)

summary(lm(units~price + STOREIdentifier, data=consumerDB))

store_lm = lm(units~price + factor(STOREIdentifier), data=consumerDB) #creating dummy variables for stores, doesn't
#create category for store 1 because it's the base category
summary(store_lm)

store_lm_1 = lm(units~price + factor(STOREIdentifier) -1, data=consumerDB) #if you want to remove store 1
summary(store_lm_1)
