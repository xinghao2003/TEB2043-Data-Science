# Data Pre-processing for Churn_Train.csv

library(dplyr) # library for using %>%
library(ggplot2)
library(cowplot) # library for plot_grid
library(mice) # library for impute
library(missForest) # library for impute
library(caret)

churn <- read.csv("Churn_Train.csv", header = TRUE)

# Test with md.pattern() function and observe the visual representation of missing values.
md.pattern(churn, rotate.names = TRUE)
# As the result, there are 9 missing values under Total.Charges column

# Look for the best method to fill in the missing values

# Imputed by zero, mean, median
value_imputed <- data.frame(
  original = churn$Total.Charges,
  # definitely not considering using zero, it got no meaning, and also causing min value changed
  imputed_zero = replace(churn$Total.Charges, is.na(churn$Total.Charges), 0),
  # 
  imputed_mean = replace(churn$Total.Charges, is.na(churn$Total.Charges), mean(churn$Total.Charges, na.rm=TRUE)),
  # median will be selected to fill in the missing value, its summary data is slightly closer to original compare to mean
  imputed_median = replace(churn$Total.Charges, is.na(churn$Total.Charges), median(churn$Total.Charges, na.rm=TRUE))
)

# Create histograms after imputation
h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()

h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()

h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()

h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()

# Perform MICE imputation methods. 
# summary data is slightly deviated from original when compared to mean and median
# beside mice is slower compare to mean and median, but still way more faster than miss forest
mice_imputed <- data.frame(
  # pmm: Predictive mean matching.
  imputed_pmm = complete(mice(churn, method = "pmm"))$Total.Charges,
  # cart: Classification and regression trees.
  imputed_cart = complete(mice(churn, method = "cart"))$Total.Charges,
  # lasso.norm: Lasso linear regression.
  # obviously out, didn't even match the original distribution
  imputed_lasso = complete(mice(churn, method = "lasso.norm"))$Total.Charges
)

# Create histograms after imputation

h5 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM-imputed distribution") +
  theme_classic()

h6 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART-imputed distribution") +
  theme_classic()

h7 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("LASSO.NORM-imputed distribution") +
  theme_classic()

# Impute missing values using miss forest method
missForest_imputed <- data.frame(
  # too slow...
  imputed_missForest = missForest(churn[,c(3,6,19,20)])$ximp$Total.Charges
)

# Visualize the distribution
# Create histograms after imputation

h8 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Miss Forest-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, h5, h6, h7, h8, nrow = 4, ncol = 2)

# Combine all type of imputed value together
collections <- data.frame(
  value_imputed,
  mice_imputed,
  missForest_imputed
)
# Compare different method of imputation
summary(collections)

# Graph will be included here, compare each method for imputation.
# Result: Imputed using median is the best for this data set.
# Explain why median is the best.

churn$Total.Charges[is.na(churn$Total.Charges)] <- median(churn$Total.Charges, na.rm=TRUE)
md.pattern(churn, rotate.names = TRUE)


# Data Normalization for variables Tenure, Monthly.Charges, Total.Charges
process <- preProcess(as.data.frame(churn$Tenure), method=c("range"))
tenure_normalized <- predict(process, as.data.frame(churn$Tenure))

process <- preProcess(as.data.frame(churn$Monthly.Charges), method=c("range"))
monthlyCharges_normalized <- predict(process, as.data.frame(churn$Monthly.Charges))

process <- preProcess(as.data.frame(churn$Total.Charges), method=c("range"))
totalCharges_normalized <- predict(process, as.data.frame(churn$Total.Charges))


# Label Encoding for variables Gender, Partner, Dependents, Phone.Service, Paperless.Billing, Churn
# These variables is selected to be label encoded because it's only contain two type of values.
gender_encode <- ifelse(churn$Gender == "Male",1,0)
table(gender_encode)

partner_encode <- ifelse(churn$Partner == "Yes",1,0)
table(partner_encode)

dependents_encode <- ifelse(churn$Dependents == "Yes",1,0)
table(dependents_encode)

phoneService_encode <- ifelse(churn$Phone.Service == "Yes",1,0)
table(phoneService_encode)

paperlessBilling_encode <- ifelse(churn$Paperless.Billing == "Yes",1,0)
table(paperlessBilling_encode)

churn_encode <- ifelse(churn$Churn == "Yes",1,0)
table(churn_encode)

# One hot encoding for variables Multiple.Lines, Internet.Services, Online.Security, Online.Backup, Device.Protection, Tech.Support, Streaming.TV, Streaming.Movies, Contract, Payment.Method
temp_dat = data.frame(churn$Multiple.Lines,churn$Internet.Service,churn$Online.Security, churn$Online.Backup, churn$Device.Protection, churn$Tech.Support, churn$Streaming.TV, churn$Streaming.Movies, churn$Contract, churn$Payment.Method)

dmy <- dummyVars(" ~ .", data = temp_dat)
dat_onehot <- data.frame(predict(dmy, newdata = temp_dat))

glimpse(dat_onehot)

# Output of data pre-processing, a processed churn data set
processed_churn <- data.frame(
  CustomerID = churn$CustomerID,
  Tenure = tenure_normalized$`churn$Tenure`,
  Monthly.Billing = monthlyCharges_normalized$`churn$Monthly.Charges`,
  Total.Billing = totalCharges_normalized$`churn$Total.Charges`,
  Senior.Citizen = churn$Senior.Citizen,
  Gender = gender_encode,
  Partner = partner_encode,
  Dependents = dependents_encode,
  Phone.Service = phoneService_encode,
  Paperless.Billing = paperlessBilling_encode,
  dat_onehot,
  Churn = churn_encode
  )

View(processed_churn)
