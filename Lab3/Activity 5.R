# Activity 5 - Feature Encoding

# There are several powerful machine learning algorithms in R. 
# However, to make the best use of these algorithms, it is imperative that we transform the data into the desired format.

# Import library
library(titanic)
library(caret)


# 1. Label Encoding
# Label encoding is the process of replacing the different levels of a categorical variable with dummy numbers.
gender_encode <- ifelse(titanic_train$Sex == "male",1,0)
table(gender_encode)


# 2. One-Hot Encoding
# One-hot (dummy) encoding is applied to the features, 
# creating a binary column for each category level and returning a sparse matrix. 
# In each dummy variable, the label "1" will represent the existence of the level in the variable,
# while the label "0" will represent its non-existence.

# Create new data frame with only variables Fare, Sex, Embarked
new_dat = data.frame(titanic_train$Fare,titanic_train$Sex,titanic_train$Embarked)

summary(new_dat)

# Call caret library and transform the categorical variables using predict() function.

# Use the dummyVars() function to create a full set of dummy variables. 
# The dummyVars() method works on the categorical variables. 
# It is to be noted that the second line contains the argument fullrank=T, 
# which will create n-1 columns for a categorical variable with n unique levels.

dmy <- dummyVars(" ~ .", data = new_dat, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = new_dat))

glimpse(dat_transformed)


# 3. Encoding Continuous (or Numeric) Variables
summary(new_dat$titanic_train.Fare)

# Create a vector of cut-off points based on 1st Quarter value and 3rd Quarter values
bins <- c(-Inf, 7.91, 31.00, Inf)

# Gives the respective names to these cut-off points.
bin_names <- c("Low", "Mid50", "High")

# Uses the cut() function to break the vector using the cut-off points.
new_dat$new_Fare <- cut(new_dat$titanic_train.Fare, breaks = bins, labels = bin_names)

# Compare the original Fare variable with the binned new_Fare
summary(new_dat$titanic_train.Fare)
summary(new_dat$new_Fare)
