# Activity 2 - Impute Missing Values in R with MICE

# MICE stands for Multivariate Imputation via Chained Equations, 
# and it’s one of the most common packages for R users. 
# It assumes the missing values are missing at random (MAR). 
# The basic idea behind the algorithm is to treat each variable that has missing 
# values as a dependent variable in regression and treat the others as independent (predictors).

# Import library for functions
library(titanic)
library(dplyr) # library for using %>%
library(ggplot2)
library(cowplot) # library for plot_grid

# Install and import the mice package
install.packages("mice")
library(mice)

titanic_numeric <- titanic_train %>%
  select(Survived, Pclass, SibSp, Parch, Age)

# Test with md.pattern() function and observe the visual representation of missing values.
md.pattern(titanic_numeric)

# Perform MICE imputation methods. 
mice_imputed <- data.frame(
  original = titanic_train$Age,
  # pmm: Predictive mean matching.
  imputed_pmm = complete(mice(titanic_numeric, method = "pmm"))$Age,
  # cart: Classification and regression trees.
  imputed_cart = complete(mice(titanic_numeric, method = "cart"))$Age,
  # lasso.norm: Lasso linear regression.
  imputed_lasso = complete(mice(titanic_numeric, method = "lasso.norm"))$Age
)
mice_imputed

# Visualize all imputed data using a grid of histograms
# Create histograms after imputation
h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()

h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM-imputed distribution") +
  theme_classic()

h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART-imputed distribution") +
  theme_classic()

h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("LASSO.NORM-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# Conclusion, CART method imputation distribution most similar to original distribution
