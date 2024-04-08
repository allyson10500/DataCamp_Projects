print('solution')

# Imports
library(dplyr)
library(rsample)
library(tidymodels)
library(lubridate)
library(caret)
install.packages("glmnet")
library(glmnet)

# Get the number of rental days
df_rental <- read.csv("rental_info.csv")
df_rental$rental_date <- ymd_hms(df_rental$rental_date)
df_rental$return_date <- ymd_hms(df_rental$return_date)
df_rental <- df_rental %>%
  mutate(rental_length = as.numeric(difftime(return_date, rental_date, units = "hours")))
df_rental$rental_length_days <- df_rental$rental_length / 24  

# Add variables from the special features column
df_rental$deleted_scenes = as.numeric(grepl("Deleted Scenes", df_rental$special_features))
df_rental$behind_the_scenes = as.numeric(grepl("Behind the Scenes", df_rental$special_features))

# Keep relevant columns
X = df_rental
X$return_date = NULL
X$rental_date = NULL
X$rental_length = NULL
X$special_features = NULL

# Perform a train-test split
set.seed(9)
split <- initial_split(X, prop = 0.8) 
X_train <- training(split)
X_test <- testing(split)
y_train <- as.numeric(X_train$rental_length_days)
y_test <- as.numeric(X_test$rental_length_days)
X_train$rental_length_days = NULL
X_test$rental_length_days = NULL

# Center and scale the training and testing sets: this standardization makes the model less sensitive to the scale of features
preProcValues <- preProcess(X_train, method = c("center", "scale"))
X_train <- predict(preProcValues, X_train)
X_test <- predict(preProcValues, X_test)

# Perform feature selection: here we are using the Lasso model to identify the features to be subsequently used in other regression models
lasso_model <- glmnet(as.matrix(X_train), y_train, alpha = 1, lambda = 0.3)
# Extract coefficients at the specified lambda value
non_zero_coef <- coef(lasso_model, s = 0.3)[,1]
# Exclude the intercept
non_zero_coef <- non_zero_coef[2:length(non_zero_coef)]
# Select non-zero coefficients
features_selected <- names(non_zero_coef[non_zero_coef != 0])
X_train_selected <- X_train[, features_selected, drop = FALSE]
X_test_selected <- X_test[, features_selected, drop = FALSE]

# Try a couple of models and choose the best MSE score
# Linear Regression
lm_model <- lm(y_train ~ ., data = as.data.frame(X_train_selected))
predictions <- predict(lm_model, newdata = X_test)
mse_lr <- mean((predictions - y_test) ^ 2)
print(paste("Mean Squared Error:", mse_lr))
# Decision Tree: train the tree model using cross-validation to select the best tree complexity
dt_model <- train(x = as.matrix(X_train), y = y_train, method = "rpart",
                  trControl = trainControl(method = "cv", number = 10),
                  tuneLength = 10)
dt_pred <- predict(dt_model, newdata = X_test)
mse_dt <- mean((dt_pred - y_test)^2)
best_mse = mse_dt
best_model = dt_model