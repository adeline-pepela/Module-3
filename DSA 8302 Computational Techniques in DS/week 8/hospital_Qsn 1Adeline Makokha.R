
# Hospital Department Efficiency Analysis using DEA in R

# Install Benchmarking package if not already installed
# install.packages("Benchmarking")
library(Benchmarking)

# Read hospital data

hospital_data <- read.csv("C:/Users/PC/Downloads/ADEH-MSC/Module 3/Module-3/DSA 8302 Computational Techniques in DS/week 8/hospital_data.csv")


# Print raw data
print("Hospital Department Data:")
print(hospital_data)

# Prepare input and output matrices
inputs <- as.matrix(hospital_data[, c("Doctors", "Nurses", "Beds")])
outputs <- as.matrix(hospital_data[, c("Patients", "SurvivalRate")])

# DEA using CCR model (constant returns to scale), input-oriented
dea_ccr <- dea(X = inputs, Y = outputs, RTS = "crs", ORIENTATION = "in")
hospital_data$CCR_Efficiency <- dea_ccr$eff

# DEA using BCC model (variable returns to scale), input-oriented
dea_bcc <- dea(X = inputs, Y = outputs, RTS = "vrs", ORIENTATION = "in")
hospital_data$BCC_Efficiency <- dea_bcc$eff

# Print efficiency results
print("DEA Efficiency Scores (CCR and BCC):")
print(hospital_data[, c("DMU", "CCR_Efficiency", "BCC_Efficiency")])

# Plot CCR Efficiency
barplot(hospital_data$CCR_Efficiency, names.arg=hospital_data$DMU,
        col="steelblue", ylim=c(0,1.1), main="CCR Efficiency Scores",
        ylab="Efficiency Score", las=2)

# Plot BCC Efficiency
barplot(hospital_data$BCC_Efficiency, names.arg=hospital_data$DMU,
        col="darkgreen", ylim=c(0,1.1), main="BCC Efficiency Scores",
        ylab="Efficiency Score", las=2)

