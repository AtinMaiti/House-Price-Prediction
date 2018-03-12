# Setting the Work Directory
setwd('E:/House Price Prediction')

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


## Test for OLS assumptions
#Price is our Dependent Variable.

# Since OLS model follows central limit theorum, distribution of the dependent and independent variable must follow a normal distribution, slightly skewed is also fine as long as the number of samples or observation is more than 30


hist(price)
hist(log(price))



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
