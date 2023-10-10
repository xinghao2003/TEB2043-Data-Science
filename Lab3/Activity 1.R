# Activity 1 - Basic Imputation Methods

# Replace the column’s missing value with zero.
dt<-data.frame(Product = c('A','B','C','D','E'),Price=c(612,447,NA,374,831))
dt$Price[is.na(dt$Price)] <- 0
dt

# Replace the column’s missing value with the mean.
dt<-data.frame(Product = c('A','B','C','D','E'),Price=c(612,447,NA,374,831))
dt$Price[is.na(dt$Price)] <- mean(dt$Price,na.rm=TRUE)
dt

# Replace the column’s missing value with the median.
dt<-data.frame(Product = c('A','B','C','D','E'),Price=c(612,447,NA,374,831))
dt$Price[is.na(dt$Price)] <- median(dt$Price,na.rm=TRUE)
dt

# Retrieve data from titanic library. Install the titanic package and call the library.
install.packages("titanic")
library(titanic)

summary(titanic_train)

# View the data set.
titanic_train$Age

library(ggplot2)
library(dplyr)
library(cowplot)

# View the Age distribution using histogram
ggplot(titanic_train, aes(Age)) +
  geom_histogram(color = "#000000", fill = "#009900") +
  ggtitle("Variable distribution") +
  theme_dark() +
  theme(plot.title = element_text(size = 18))

# Perform simple value imputation and view the data
value_imputed <- data.frame(
  original = titanic_train$Age,
  imputed_zero = replace(titanic_train$Age, is.na(titanic_train$Age), 0),
  imputed_mean = replace(titanic_train$Age, is.na(titanic_train$Age), mean(titanic_train$Age, na.rm=TRUE)),
  imputed_median = replace(titanic_train$Age, is.na(titanic_train$Age), median(titanic_train$Age, na.rm=TRUE))
)
value_imputed

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

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)


# Explanation
# All imputation methods severely impact the distribution. 
# There are a lot of missing values, so setting a single constant value doesn’t make much sense.
# Zero imputation is the worst, as it’s highly unlikely for close to 200 passengers to have the age of zero.
