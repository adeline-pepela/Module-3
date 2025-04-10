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

