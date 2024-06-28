library(car)
library(readxl)
library(MASS)
library(MuMIn)


NBA2324 <- read_excel("BaseNew.xlsx")

View(NBA2324)

test_set <- read_excel("Test.xlsx")
View(test_set)

boxcox_transform <- function(x, lambda) {
  if (lambda == 0) {
    return(log(x))
  } else {
    return((x^lambda - 1) / lambda)
  }
}

# Extract the relevant variables from the training dataset
salary = NBA2324$Salary
Age = NBA2324$Age
PPG = NBA2324$PTS
AST = NBA2324$AST
TRB = NBA2324$TRB
PER = NBA2324$PER
WS = NBA2324$WS

data <- data.frame(salary, Age, PPG, AST, TRB, PER, WS)

# Apply the Box-Cox transformation to the response variable
boxcox_result <- boxcox(salary ~ Age + PPG + AST + TRB + PER + WS, data = data, plotit = TRUE)

# Find the optimal lambda
lambda_optimal <- boxcox_result$x[which.max(boxcox_result$y)]
lambda_optimal

# Transform the response variable in the training set
data$salary_transformed <- boxcox_transform(data$salary, lambda_optimal)

# Fit the reduced model
reduced_model <- lm(salary_transformed ~ Age + PPG + AST + TRB + PER + WS, data = data)
summary(reduced_model)

# Perform ANOVA on the reduced model
anova_test <- anova(reduced_model)
print(anova_test)

# Calculate AICc and BIC
raic <- AICc(reduced_model)
rbic <- BIC(reduced_model)
cat("Red AIC:", raic, "\n")
cat("Red BIC:", rbic, "\n")

# Calculate VIF values
vif_values <- vif(reduced_model)
print(vif_values)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(reduced_model)
qqnorm(residuals(reduced_model))
qqline(residuals(reduced_model))

# Ensure the test set contains the relevant predictor variables and the salary variable
test_set <- test_set[, c("Salary", "Age", "PPG", "AST", "TRB", "PER", "WS")]

# Transform the salary in the test set using the same lambda
test_set$salary_transformed <- boxcox_transform(test_set$Salary, lambda_optimal)

# Make predictions on the test set
predictions <- predict(reduced_model, newdata = test_set)

# Calculate Mean Squared Error (MSE) on the test set
mse <- mean((test_set$salary_transformed - predictions)^2)
cat("Mean Squared Error:", mse, "\n")

# Calculate R-squared on the test set
ss_total <- sum((test_set$salary_transformed - mean(test_set$salary_transformed))^2)
ss_res <- sum((test_set$salary_transformed - predictions)^2)
r_squared <- 1 - (ss_res / ss_total)
cat("R-squared:", r_squared, "\n")

par(mfrow = c(2, 2))
plot(reduced_model)
qqnorm(residuals(reduced_model))
qqline(residuals(reduced_model))

# Diagnostic plots for the test set
par(mfrow = c(2, 2))

# Plot 1: Predicted vs Actual
plot(test_set$salary_transformed, predictions, xlab = "Actual Salary (Transformed)", ylab = "Predicted Salary (Transformed)", main = "Predicted vs Actual")
abline(0, 1, col = "red")

# Plot 2: Residuals vs Fitted
plot(predictions, test_set$salary_transformed - predictions, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Plot 3: QQ plot of residuals
qqnorm(test_set$salary_transformed - predictions, main = "QQ Plot of Residuals")
qqline(test_set$salary_transformed - predictions, col = "red")

