# Activity 3

# The Miss Forest imputation technique is based on the Random Forest algorithm. 
# It’s a non-parametric imputation method, which means it doesn’t make explicit assumptions about the 
# function form, but instead tries to estimate the function in a way that’s closest to the data points.

# Import library for functions
library(titanic)
library(dplyr) # library for using %>%
library(ggplot2)
library(cowplot) # library for plot_grid

# Install and import missForest library.
install.packages("missForest")
library(missForest)

titanic_numeric <- titanic_train %>%
  select(Survived, Pclass, SibSp, Parch, Age)

# Impute missing values in Age attribute
missForest_imputed <- data.frame(
  original = titanic_numeric$Age,
  imputed_missForest = missForest(titanic_numeric)$ximp$Age
)

# Visualize the distribution
# Create histograms after imputation
h1 <- ggplot(missForest_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()

h2 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Miss Forest-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, nrow = 1, ncol = 2)