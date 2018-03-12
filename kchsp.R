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


#Before testing if the OLS assumption stands true lets try to run a kitchen sink model.
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
KSmodel2 <- lm(formula = price ~  sqft_living15 + lat + grade + view + waterfront + floors + waterfront + bedrooms + bathrooms, data = data)
summary(KSmodel2)
vif(KSmodel2)

#Before we jump into further assumptions we would like to see that if all the IV have normal distribution if not we will apply tranformations.
#ignoring bedrooms, bathrooms, waterfront, view, and floors as they are discrete variables
par(mfrow = c(1,3))
hist(log(sqft_living15))
hist(lat)
hist(grade)

## List of Hypothesis, I believe in parsimonious model:
1.


#Second Test 
hist(log(sqft_living))
plot(log(price) ~ log(sqft_living))

hist(yr_built)

plot(k)

mean(price)

boxplot(price)

VIF(data)

#m1 <- lm(log(price) ~ log(sqft_living) + bedrooms*bathrooms + view + waterfront + yr_built)
m <- lm(log(price) ~ log(sqft_living))  

summary(m)

plot(m)

interaction.plot(bedrooms, bathrooms , price)
