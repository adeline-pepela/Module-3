
# Retail Store Efficiency Analysis using DEA in R

# Load Benchmarking package
library(Benchmarking)

# Read retail data
retail_data <- read.csv("retail_data.csv")

# Print raw data
print("Retail Store Data:")
print(retail_data)

# Prepare input and output matrices
inputs <- as.matrix(retail_data[, c("MarketingSpend", "Employees")])
outputs <- as.matrix(retail_data[, c("Revenue", "Customers")])

# DEA using CCR model (constant returns to scale), input-oriented
dea_ccr <- dea(X = inputs, Y = outputs, RTS = "crs", ORIENTATION = "in")
retail_data$CCR_Efficiency <- dea_ccr$eff

# DEA using BCC model (variable returns to scale), input-oriented
dea_bcc <- dea(X = inputs, Y = outputs, RTS = "vrs", ORIENTATION = "in")
retail_data$BCC_Efficiency <- dea_bcc$eff

# Print efficiency results
print("DEA Efficiency Scores (CCR and BCC):")
print(retail_data[, c("DMU", "CCR_Efficiency", "BCC_Efficiency")])

# Plot CCR Efficiency
barplot(retail_data$CCR_Efficiency, names.arg=retail_data$DMU,
        col="steelblue", ylim=c(0,1.1), main="CCR Efficiency Scores - Retail",
        ylab="Efficiency Score", las=2)

# Plot BCC Efficiency
barplot(retail_data$BCC_Efficiency, names.arg=retail_data$DMU,
        col="darkgreen", ylim=c(0,1.1), main="BCC Efficiency Scores - Retail",
        ylab="Efficiency Score", las=2)
