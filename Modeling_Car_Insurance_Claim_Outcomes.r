# Import required libraries
library(readr)
library(dplyr)
library(glue)
library(yardstick)

# Read in dataset
cars = read_csv('car_insurance.csv')

# View data types
str(cars)

# Missing values per column
colSums(is.na(cars))

# Distribution of credit_score
summary(cars$credit_score)

# Distribution of annual_mileage
summary(cars$annual_mileage)

# Fill missing values with the mean
cars$credit_score[is.na(cars$credit_score)] <- mean(cars$credit_score, na.rm = TRUE)
cars$annual_mileage[is.na(cars$annual_mileage)] <- mean(cars$annual_mileage, na.rm = TRUE)

# Create a dataframe to store features
features_df <- data.frame(features = c(names(subset(cars, select = -c(id, outcome)))))

# Empty vector to store accuracies
accuracies <- c()

# Loop through features
for (col in features_df$features) {
    # Create a model
    model <- glm(glue('outcome ~ {col}'), data = cars, family = 'binomial')
    # Get prediction values for the model
    predictions <- round(fitted(model))
    # Calculate accuracy
    accuracy <- length(which(predictions == cars$outcome)) / length(cars$outcome)
	# Add accuracy to features_df
	features_df[which(features_df$feature == col), "accuracy"] = accuracy
}

# Find the feature with the largest accuracy
best_feature <- features_df$features[which.max(features_df$accuracy)]
best_accuracy <- max(features_df$accuracy)

# Create best_feature_df
best_feature_df <- data.frame(best_feature, best_accuracy)

# Run in a new cell to check your solution
best_feature_df