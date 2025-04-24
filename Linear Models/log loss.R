#---------------------------------------Load necessary libraries---------------------------------------------------------#

libs <- c("tidyverse", "dplyr", "gridExtra",  "ggplot2", "ggfortify", "broom", "ggcorrplot",
          
          "corrr", "corrplot", "MASS", "caret", "e1071", "glmnet")

for (ilib in libs) {
  
  if (!(ilib %in% installed.packages())) {
    
    install.packages(ilib)
    
  }
  
  library(ilib, character.only = TRUE)
  
}

theme_set(theme_classic())

# Example: Read data (replace with your actual file)

#churn_df <- read.csv("D:\\OneDrive\\OneDrive - aphrc.org\\SU-2024-2025\\Linear-models\\Materials\\Linear-Models\\Week-5-Linear-Models-in-Classification-Tasks (logistic rgression)\\ChurnData.csv")
churn_df <- read.csv("C:/Users/PC/Downloads/ADEH-MSC/Module 3/Module-3/Linear Models/ChurnData.csv")

names(churn_df)
# Select features and convert target variable to factor

churn_df <- churn_df %>% 
  
  dplyr::select(tenure, age, address, income, ed, employ, equip, callcard, wireless, churn)

churn_df$churn <- as.factor(churn_df$churn)

# Define predictors and target

X <- churn_df %>% dplyr::select(tenure, age, address, income, ed, employ, equip)

y <- churn_df$churn

# Standardize predictors

X_scaled <- scale(X)

# Combine scaled predictors with target for splitting

data <- as.data.frame(cbind(X_scaled, churn = y))

# Train/test split

set.seed(4) # Reproducible 

train_index <- createDataPartition(data$churn, p = 0.8, list = FALSE)

train_data <- data[train_index, ]

test_data <- data[-train_index, ]

# Fit logistic regression with glmnet (regularized)

x_train <- as.matrix(train_data[, -ncol(train_data)])

y_train <- as.numeric(as.character(train_data$churn))

x_test <- as.matrix(test_data[, -ncol(test_data)])

y_test <- as.numeric(as.character(test_data$churn))

# Fit logistic regression with L2 regularization (ridge)

model <- glmnet(x_train, y_train, family = "binomial", alpha = 0, lambda = 100)


# Predict class labels and probabilities

yhat <- predict(model, newx = x_test, type = "class")

yhat_prob <- predict(model, newx = x_test, type = "response")

# View predictions

print(head(yhat))

print(head(yhat_prob))

# Evaluate using log loss

log_loss <- function(actual, predicted_probs) {
  
  epsilon <- 1e-15
  
  predicted_probs <- pmin(pmax(predicted_probs, epsilon), 1 - epsilon)
  
  -mean(actual * log(predicted_probs) + (1 - actual) * log(1 - predicted_probs))
  
}

logLossValue <- log_loss(y_test, yhat_prob)

print(paste("Log Loss:", logLossValue))

# Try different lambda (regularization strength)

model2 <- glmnet(x_train, y_train, family = "binomial", alpha = 0, lambda = 1)

yhat_prob2 <- predict(model2, newx = x_test, type = "response")

logLossValue2 <- log_loss(y_test, yhat_prob2)

print(paste("New Log Loss with lambda=1:", logLossValue2))

