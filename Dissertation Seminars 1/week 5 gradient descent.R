libs <- c("tidyverse","dplyr", "gridExtra", "ggplot2", "ggfortify","broom","datarium")
for (ilib in libs){
  if(!(ilib %in% installed.packages())){
    install.packages(ilib)
  }
  library(ilib, character.only = TRUE)
}

theme_set(theme_classic())


# Load an Excel file

# Load the CSV file
data <- read.csv("C:/Users/PC/Downloads/ADEH-MSC/Module 3/Module-3/Linear Models/data.csv")



#---------------------------------------Load necessary libraries---------------------------------------------------------#

libs <- c("tidyverse", "dplyr", "gridExtra",  "ggplot2", "ggfortify", "broom", "ggcorrplot",
          "corrr", "corrplot", "MASS")

for (ilib in libs) {
  if (!(ilib %in% installed.packages())) {
    install.packages(ilib)
  }
  library(ilib, character.only = TRUE)
}

theme_set(theme_classic())

#----------------Gradient descent


# Load the dataset
data <- read.csv("C:/Users/PC/Downloads/ADEH-MSC/Module 3/Module-3/Linear Models/data.csv")

X <- data[,1]
Y <- data[,2]



# Plot the data
plot(X, Y, main="Scatter Plot of Data", xlab="X", ylab="Y", col="blue", pch=16)

# Initialize parameters
m <- 0  # Gradient (slope)
c <- 0  # Intercept
epochs <- 1000000  # Number of iterations
L <- 0.00038  # Learning rate
n <- length(X)  # Number of data points








# Gradient Descent Algorithm
for (i in 1:epochs) {
  Y_pred <- m * X + c  # Predicted Y values
  D_m <- (-2/n) * sum(X * (Y - Y_pred))  # Partial derivative wrt m
  D_c <- (-2/n) * sum(Y - Y_pred)  # Partial derivative wrt c
  m <- m - L * D_m  # Update slope
  c <- c - L * D_c  # Update intercept
  if (m <= 0) {  # Stop when minimum error is reached
    break
  }
}

# Print final parameters
print(paste("Final m:", m, "Final c:", c))

# Plot the regression line
plot(X, Y, main="Gradient Descent Linear Regression", xlab="X", ylab="Y", col="blue", pch=16)
abline(a=c, b=m, col="red", lwd=2)
