# Install and load necessary packages
#install.packages("brms")
library(brms)

# Generate some example data
set.seed(123)
n <- 100
x <- seq(0, 10, length.out = n)
y <- 3+ 2* x^2 - 3 * x + rnorm(n, mean = 0, sd = 5)

# Define the model formula
formula <- bf(y ~ poly(x, 2))

# Fit the Bayesian polynomial regression model
model <- brm(formula, data = data.frame(x = x, y = y), cores = 4)

# Summary of the model
summary(model)

# Plot the posterior distributions of the coefficients
plot(model)

# Predictions
new_x <- seq(0, 10, length.out = 100)
predictions <- predict(model, newdata = data.frame(x = new_x), allow_new_levels = TRUE)

# Plot the predicted values
plot(x, y, main = "Bayesian Polynomial Regression", xlab = "x", ylab = "y", col = "blue")

# Add lines representing the predicted values
lines(new_x, predictions[,1], col = "red", lwd = 2)
