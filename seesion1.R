# Load necessary library
library(dplyr)

# Automatic Replication for Normal Distribution
set.seed(2024)

# Simulate means for normal distribution
Means_normal <- replicate(n=10^5, expr = {
  X <- rnorm(n=10, mean = 2, sd = 2)
  mean(X)
})

# Calculate bias, relative bias, and MCSD for normal distribution
bias_normal <- mean(Means_normal - 2) # Bias (since the mean of the normal distribution is 2)
rel_bias_normal <- mean(Means_normal - 2) / 2 # Relative bias
rel_bias_percent_normal <- mean(Means_normal - 2) / 2 * 100 # Relative bias in %

mcsd_normal <- sd(Means_normal) # Monte Carlo Standard Deviation
theoretical_mcsd_10_normal <- 2 / sqrt(10) # Theoretical MCSD for n=10
theoretical_mcsd_1000_normal <- 2 / sqrt(1000) # Theoretical MCSD for n=1000

# Automatic Replication for Chi-Squared Distribution with 2 Degrees of Freedom
set.seed(2024)

# Simulate means for chi-squared distribution
Means_chisq <- replicate(n=10^5, expr = {
  X <- rchisq(n=10, df=2)
  mean(X)
})

# Calculate bias, relative bias, and MCSD for chi-squared distribution
bias_chisq <- mean(Means_chisq - 2) # Bias (since the mean of chi-squared distribution with 2 df is 2)
rel_bias_chisq <- mean(Means_chisq - 2) / 2 # Relative bias
rel_bias_percent_chisq <- mean(Means_chisq - 2) / 2 * 100 # Relative bias in %

mcsd_chisq <- sd(Means_chisq) # Monte Carlo Standard Deviation
theoretical_mcsd_10_chisq <- sqrt(2 * 2 / 10) # Theoretical MCSD for n=10
theoretical_mcsd_1000_chisq <- sqrt(2 * 2 / 1000) # Theoretical MCSD for n=1000

# Combine results into a data frame
results <- data.frame(
  Distribution = c("Normal", "Chi-Squared"),
  Bias = c(bias_normal, bias_chisq),
  Relative_Bias = c(rel_bias_normal, rel_bias_chisq),
  Relative_Bias_Percent = c(rel_bias_percent_normal, rel_bias_percent_chisq),
  MCSD = c(mcsd_normal, mcsd_chisq),
  Theoretical_MCSD_10 = c(theoretical_mcsd_10_normal, theoretical_mcsd_10_chisq),
  Theoretical_MCSD_1000 = c(theoretical_mcsd_1000_normal, theoretical_mcsd_1000_chisq)
)

# Standard Error (SE) and Mean Squared Error (MSE) calculations
se_normal <- sd(Means_normal) / sqrt(length(Means_normal))
se_chisq <- sd(Means_chisq) / sqrt(length(Means_chisq))

mse_normal <- mean((Means_normal - 2)^2) # MSE for normal distribution
mse_chisq <- mean((Means_chisq - 2)^2) # MSE for chi-squared distribution

# Add SE and MSE to results data frame
results$SE <- c(se_normal, se_chisq)
results$MSE <- c(mse_normal, mse_chisq)

View(results)
