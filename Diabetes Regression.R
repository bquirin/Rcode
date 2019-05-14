# Load in dataset 
diabetes = read.table(file = "http://www4.stat.ncsu.edu/%7Eboos/var.select/diabetes.tab.txt",
                       header = TRUE)
# Use only one feature 
X = diabetes$BMI
Y = diabetes$Y

# Standardis BMI 
X = X - mean(X)

# Plot
plot(X, Y, pch = 20, col = "black", cex = 1.2, main = "Diabetes dataset",xlab= "BMI", ylab = "Diabetes")
points(X, Y, pch = 20, col = "yellow", cex = 0.6)


# Sample dataset (Record 30-45) 

start_idx = 30 
n_class = 15 

X = X[start_idx:(start_idx + n_class)]
Y = Y[start_idx:(start_idx + n_class)]

plot(X, Y, pch = 20, col = "black", cex = 1.2, main = "Sample from Dataset",xlab= "BMI", ylab = "Diabetes")
points(X, Y, pch = 20, col = "orange", cex = 0.6)

# Build DataFrame
data = data.frame(Y, X, X * Y, Y * Y, X * X)
colnames(data) = c("Y", "X", "XY", "YY", "XX")
print(data)

# Number of rows/ records
n =  nrow(data)

# Measure Correlation to identify if a linear relation is reasonable
corr_coeff = (n * sum(data["XY"]) - sum(data["X"]) * sum(data["Y"]))/(sqrt(n * sum(data["XX"]) -
                                                                             (sum(data["X"]))^2) * sqrt(n * sum(data["YY"]) - (sum(data["Y"]))^2))
cat("Correlation = ", corr_coeff)
cat("Squared correlation coefficient = ", corr_coeff^2)

# Linear Regression 
X_bar = mean(data[, "X"])
Y_bar = mean(data[, "Y"])

beta_hat = (sum(data["XY"]) - n * X_bar * Y_bar)/(sum(data["XX"]) - n * X_bar * X_bar)
alpha_hat = Y_bar - beta_hat * X_bar

# Linear Regression model coeffecients
cat("Coefficients: \n", "Beta: ", beta_hat, "\nAlpha: ", alpha_hat)

# Create Best fit line 
Y_hat = alpha_hat + beta_hat * data[, "X"]

plot(data[, "X"], data[, "Y"], col = "black", pch = 20, cex = 1.2, main = "Linear Regression",xlab= "BMI", ylab = "Diabetes")
points(data[, "X"], data[, "Y"], col = "yellow", pch = 20, cex = 0.6)
lines(data[, "X"], Y_hat, col = "blue", lwd = 3)

# Print predicted values
output = cbind(Y_hat, alpha_hat, beta_hat, X)
colnames(output) = c("Predicted Y =", "alpha +", "beta", "X")
print(output)

# Analyse residuals
res = data[, "Y"] - Y_hat
plot(data[, "X"], data[, "Y"], col = "black", pch = 20, cex = 1.2, main = "Linear Regression + Residuals",xlab= "BMI", ylab = "Diabetes")
points(data[, "X"], data[, "Y"], col = "yellow", pch = 20, cex = 0.6)
lines(data[, "X"], Y_hat, col = "blue", lwd = 3)
for (i in 1:n) {
  lines(c(data[i, "X"], data[i, "X"]), c(Y_hat[i], data[i, "Y"]), col = "red",
        lwd = 3)
}

plot(data[, "X"], res, col = "black", pch = 20, cex = 1.2, main = "Residuals", xlab = "BMI")
points(data[, "X"], res, col = "yellow", pch = 20, cex = 0.6)
lines(range(X), c(0, 0), col = "blue", lwd = 3)
for (i in 1:n) {
  lines(c(data[i, "X"], data[i, "X"]), c(res[i], 0), col = "red", lwd = 3)
}

output = cbind(Y, Y_hat, res)
colnames(output) = c("Y", "Predicted Y", "Residual")
print(output)


# Estimate model variance
sigma2_hat = 1/(n - 2) * sum(res^2)
cat("Estimated Variance = ", sigma2_hat)

# Sum of Squares
SStotal = sum((data[, "Y"] - Y_bar)^2)
SSexplained = sum((Y_hat - Y_bar)^2)
SSresid = sum(res^2)
cat("SS Total = ", SStotal)
cat("\nSS Explained = ", SSexplained)
cat("SS Residual = ", SSresid)
cat("\nSS Explained + SS Residual = ", SSexplained + SSresid)
R_squared = SSexplained/SStotal
cat("\n\nDetermination coefficient: ", R_squared)

# Thus the regression mode explains appoximately 60% of the variance



