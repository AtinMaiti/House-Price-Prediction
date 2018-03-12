# Setting the Work Directory
setwd('E:/House Price Prediction/House-Price-Prediction')

#read csv from working directory
data <- read.csv('kc_house_data.csv') 

#loading Libraries for data manupulation and EDA
library(dplyr)
library(DataExplorer)

#attach data into the environment
attach(data)

#Display some summary statistics
summary(data)


# Plot Data dictonary and correlation matrix
plot_str(data)
plot_correlation(data)

#Listing all the Independent Variables with significant correlation
# sqft_living15
# lat
# sqft_above
# grade
# view
# waterfront
# floors
# sqft_living
# bedrooms
# bathrooms


#Before testing if the OLS assumptions stands true lets try to run a kitchen sink model.
KSmodel1<- lm(formula = price ~  sqft_living15 + lat + sqft_above + grade + view + waterfront + floors + waterfront + sqft_living + bedrooms + bathrooms, data = data)

summary(model1)
#Multiple R-squared = 0.6577

par(mfrow=c(2,2))
plot(KSmodel1)
# Both heteroskedactic and not normal

# Test for OLS assumptions
# Price is our Dependent Variable
# Since OLS model follows central limit theorum, distribution of the dependent and independent variable must follow a normal distribution, slightly skewed is also fine as long as the number of samples or observation is more than 30
hist(price)
hist(log(price))

#First test: Multicolinearity

# Variance Inflation Factor (remove variables with value more than 5)
library(car)
vif(KSmodel1)

#sqft_living and sqft_above has value more than 5 so we will remove it

#Kitchen Sink Model 2
KSmodel2 <- lm(formula = price ~  sqft_living15 + lat + grade + view + waterfront + floors + bedrooms + bathrooms, data = data)
summary(KSmodel2)

#Second Test: We know that there might be auto-correlation, but there was not so much data to provide evidence Real estate price increases year over year due to inflation. 
#Third Test: All the factors are dependent with grade, so have to exclude this variable.

#Before we jump into further assumptions we would like to see that if all the IV have normal distribution if not we will apply tranformations.
#ignoring bedrooms, bathrooms, waterfront, view, and floors as they are discrete variables
par(mfrow = c(2,3))

hist(sqft_living15)
hist(lat)
hist(grade)

## List of Hypothesis, I believe in parsimonious models:
# 1. bathrooms with bedrooms might have more predicting power than bathrooms and bedrooms.
# 2. The house which has been viewed and has waterfront will have higher price.


#Let's just plot a scatterplot for price vs sqft_living15
plot(log(price) ~ log(sqft_living15))
abline(lm(log(price) ~ log(sqft_living15), data= data), col ='red')


#parsimonious model
Model1<-lm(log(price) ~ log(sqft_living15), data= data)
summary(Model1)

# Homoskedastic and Normal, So this is a good model but doesnot explains the non linearity towards the extreme ends.
par(mfrow = c(2,2))
plot(Model1)

#Model that fits our hypothesis
Model2<- lm(log(price) ~ log(sqft_living15) + bedrooms*bathrooms + view*as.factor(waterfront), data= data)

summary(Model2)

