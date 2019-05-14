#Load the diabetes dataset
diabetes = read.table(file = "http://www4.stat.ncsu.edu/%7Eboos/var.select/diabetes.tab.txt", header = TRUE)

X = diabetes$BMI
Y = diabetes$Y 


## standardis BMI 
X = X -mean(X)

plot(X, Y, pch = 20, col = "black", cex = 1.2, main = "Whole dataset")
points(X, Y, pch = 20, col = "yellow", cex = 0.6)

start_idx = 30 
n_class = 15 

X = X[start_idx:(start_idx + n_class)]
Y = Y[start_idx:(start_idx + n_class)]

plot(X, Y, pch = 20, col = "black", cex = 1.2, main = "Data for this seminar")
points(X, Y, pch = 20, col = "yellow", cex = 0.6)

data = data.frame(Y, X, X * Y, Y * Y, X * X)
colnames(data) = c("Y", "X", "XY", "YY", "XX")
print(data)

n <- nrow(data)

corr_coeff = (n * sum(data["XY"]) - sum(data["X"]) * sum(data["Y"]))/(sqrt(n * sum(data["XX"]) - (sum(data["X"]))^2) * sqrt(n * sum(data["YY"]) - (sum(data["Y"]))^2))

cat("Correlation = ", corr_coeff)

cat("Squared correlation coefficient = ", corr_coeff^2)

