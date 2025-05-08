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
data <- read.csv("C:/Users/PC/Downloads/ADEH-MSC/Module 3/Module-3/Linear Models/FuelConsumption.csv")

# Check the structure of the data
str(data)



# View the first few rows of the data
head(data)

#inspect the data
sample_n(data, 3)
dim(data)
#build a simple simple linear model (CO2EMISSIONS = beta_0 + beta_1(FUELCONSUMPTION_CITY))
model <- lm(CO2EMISSIONS ~ FUELCONSUMPTION_CITY, data=data)
summary(model)

ggplot(data, aes(x = FUELCONSUMPTION_CITY)) + geom_histogram(binwidth = 1, fill = "blue", color = "black")
  labs(title = "Histograms of FUELCONSUMPTION_CITY", x = "FUELCONSUMPTION_CITY", y = "count")

ggplot(data, aes(x = FUELCONSUMPTION_CITY)) + geom_density(fill = "blue", alpha = 0.5)+
  labs(title = "Histogram of FUELCONSUMPTION_CITY", x = "FUELCONSUMPTION_CITY", y = "count")

#----linearity
#----scatter plot 

ggplot(data, aes(x = FUELCONSUMPTION_CITY, y = CO2EMISSIONS)) +geom_point(color = "blue", size = 1.0)+
  labs(title = "scatter diagram", x = "FUELCONSUMPTION_CITY", y = "CO2EMISSIONS")

#------plot
plot(model, 1)
plot(model, 2)
plot(model, 3)
plot(model, 4)


model2 <- lm(log(CO2EMISSIONS) ~ FUELCONSUMPTION_CITY, data)
plot(model2, 3)

plot(model, 5)

plot(model, 4, id.n = 5)

#check the assumptions using the autoplot
par(mfrow = c(2, 2))
autoplot(model)
