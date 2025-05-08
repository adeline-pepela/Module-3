
# School Performance Evaluation using DEA in R

# Load Benchmarking package
library(Benchmarking)

# Read school data
school_data <- read.csv("school_data.csv")

# Print raw data
print("School Performance Data:")
print(school_data)

# Prepare input and output matrices
inputs <- as.matrix(school_data[, c("Teachers", "Budget")])
outputs <- as.matrix(school_data[, c("GraduationRate", "TestScore")])

# DEA using CCR model (constant returns to scale), input-oriented
dea_ccr <- dea(X = inputs, Y = outputs, RTS = "crs", ORIENTATION = "in")
school_data$CCR_Efficiency <- dea_ccr$eff

# DEA using BCC model (variable returns to scale), input-oriented
dea_bcc <- dea(X = inputs, Y = outputs, RTS = "vrs", ORIENTATION = "in")
school_data$BCC_Efficiency <- dea_bcc$eff

# Print efficiency results
print("DEA Efficiency Scores (CCR and BCC):")
print(school_data[, c("DMU", "CCR_Efficiency", "BCC_Efficiency")])

# Plot CCR Efficiency
barplot(school_data$CCR_Efficiency, names.arg=school_data$DMU,
        col="steelblue", ylim=c(0,1.1), main="CCR Efficiency Scores - Schools",
        ylab="Efficiency Score", las=2)

# Plot BCC Efficiency
barplot(school_data$BCC_Efficiency, names.arg=school_data$DMU,
        col="darkgreen", ylim=c(0,1.1), main="BCC Efficiency Scores - Schools",
        ylab="Efficiency Score", las=2)
