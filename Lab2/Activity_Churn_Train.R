# Churn_Train datasets

# Data Preparation
# Import Churn_Train datasets
churn = read.csv("Churn_Train.csv", header = TRUE)

# Check for duplicated entries
duplicated(churn)
anyDuplicated(churn)
# Look for its location
which(duplicated(churn))

# Check for missing values
is.na(churn)
anyNA(churn)
# Look for its location
which(is.na(churn))
colSums(is.na(churn))

# Filling missing values
# Select either mean or median as the value for missing values
# Method 1
# churn$Total.Charges <- replace_na(churn$Total.Charges, mean(churn$Total.Charges, na.rm = TRUE))

# Method 2 
churn <- churn %>%
  mutate(Total.Charges=replace(Total.Charges, is.na(Total.Charges), median(Total.Charges, na.rm = TRUE)))

# Check for missing values
anyNA(churn)


# **********************************************************************************
# Activity 1

# Univariate analysis
# Calculate descriptive statistic using describe()
describe(churn)

# Test of normality on numeric variables using normality()
# Normality tests are used to determine if a data set is well-modeled by a normal distribution 
# and to compute how likely it is for a random variable underlying the data set to be normally distributed.
# Column selection same as describe()
normality(churn)

# Visualization of normality of numerical variables using plot_normality()
plot_normality(churn)

# Perform bivariate/multivariate analysis.
# Calculation of correlation coefficient using correlate()
correlate(churn)

# Visualization of the correlation matrix using plot.correlate()
churn %>% 
  correlate() %>%
  plot()

# Perform EDA based on target variable
# Set a target variable from a data set
# Payment.Method is categorical variable
cat <- target_by(churn,Payment.Method)

# The relationship between the target variable and the variable of interest (predictor) is briefly analyzed.
# target variable is categorical variable and predictors are numeric variable.
cat_Tenure <- relate(cat, Tenure)
cat_Tenure
summary(cat_Tenure)
plot(cat_Tenure)

# target variable is categorical variable and predictors are categorical variable.
cat_Gender <- relate(cat, Gender)
cat_Gender
summary(cat_Gender)
plot(cat_Gender)

# Price is numeric variable
num <- target_by(churn,Monthly.Charges)

# target variable is numeric variable and predictors are numeric variable.
num_Tenure <- relate(num, Tenure)
num_Tenure
summary(num_Tenure)
plot(num_Tenure)


# **********************************************************************************
# Activity 2

# Create a dynamic report using eda_web_report() for Churn dataset
churn %>%
  eda_web_report(output_file="EDA.html", theme="blue")

# Create a EDA report using eda_paged_report() for static report
churn %>%
  eda_paged_report(output_file="EDA.pdf", theme="blue")
