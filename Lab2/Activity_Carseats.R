# ISLR Carseats dataset

# Activity 1
# Univariate analysis
# Calculate descriptive statistic using describe()
describe(Carseats)
describe(Carseats,Sales,CompPrice,Income) # Select column by name
describe(Carseats,Sales:Income) # Select all column from Sales until Income
describe(Carseats,-(Sales:Income)) # Select all column except Sales until Income

# Test of normality on numeric variables using normality()
# Normality tests are used to determine if a data set is well-modeled by a normal distribution 
# and to compute how likely it is for a random variable underlying the data set to be normally distributed.
# Column selection same as describe()
normality(Carseats)
normality(Carseats,Sales,CompPrice,Income) # Select column by name
normality(Carseats,Sales:Income) # Select all column from Sales until Income
normality(Carseats,-(Sales:Income)) # Select all column except Sales until Income

# Visualization of normality of numerical variables using plot_normality()
plot_normality(Carseats,Sales,CompPrice)

# Perform bivariate/multivariate analysis.
# Calculation of correlation coefficient using correlate()
correlate(Carseats,Sales)

# Visualization of the correlation matrix using plot.correlate()
# Method 1
plot_correlate(Carseats, Sales)

# Method 2
# %>% is called the forward pipe operator in R. 
# It provides a mechanism for chaining commands with a new forward-pipe operator, %>%. 
# This operator will forward a value, or the result of an expression, into the next function call/expression.
Carseats %>% 
  correlate() %>%
  plot()
  
# Perform EDA based on target variable
# Set a target variable from a data set
# US is categorical variable
cat <- target_by(Carseats,US)

# The relationship between the target variable and the variable of interest (predictor) is briefly analyzed.
# target variable is categorical variable and predictors are numeric variable.
cat_Sales <- relate(cat, Sales)
cat_Sales
summary(cat_Sales)
plot(cat_Sales)

# target variable is categorical variable and predictors are categorical variable.
cat_ShelveLoc <- relate(cat, ShelveLoc)
cat_ShelveLoc
summary(cat_ShelveLoc)
plot(cat_ShelveLoc)

# Price is numeric variable
num <- target_by(Carseats,Sales)

# target variable is numeric variable and predictors are categorical variable.
num_ShelveLoc <- relate(num, ShelveLoc)
num_ShelveLoc
summary(num_ShelveLoc)
plot(num_ShelveLoc)

# target variable is numeric variable and predictors are numeric variable.
num_Price <- relate(num, Price)
num_Price
summary(num_Price)
plot(num_Price)
