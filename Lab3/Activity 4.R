# Activity 4 - Normalize data with scaling methods

# Feature Scaling is an essential step prior to modeling while solving prediction problems in Data Science. 
# Machine Learning algorithms work well with data that belongs to a smaller and standard scale. 
# Normalization techniques enable us to reduce the scale of the variables and 
# thus it affects the statistical distribution of the data in a positive manner.

# Import library
library(titanic)
install.packages("caret")
library(caret)

# Normalize data using Log Transformation
log_scale = log(as.data.frame(titanic_train$Fare))

# Normalize data using Min-Max Scaling
# With Min-Max Scaling, we scale the data values between a range of 0 to 1 only. 
# Due to this, the effect of outliers on the data values suppresses to a certain extent. 
# Moreover, it helps us have a smaller value of the standard deviation of the data scale.

process <- preProcess(as.data.frame(titanic_train$Fare), method=c("range"))
norm_scale <- predict(process, as.data.frame(titanic_train$Fare))

# Normalize data using standard scaling in R.
scale_data <- as.data.frame(scale(titanic_train$Fare))


