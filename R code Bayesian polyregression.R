# Bayesian Polynomial Regression with Unsupservised Degree Determination

# Example data
x <- seq(0, 10, by = 0.5)
y <- x^2 - 3*x + rnorm(length(x), mean = 0, sd = 5)

# Bayesian Model Fitting
library(brms)
set.seed(123)

# Define Bayesian polynomial regression model with varying degree
formula <- bf(y ~ poly(x, degree, raw = TRUE))
prior <- c(prior(normal(0, 10), class = b),  # Prior for intercept
           prior(normal(0, 10), class = Intercept),  # Prior for slopes
           prior(normal(0, 10), class = sd))  # Prior for residual standard deviation
fit <- brm(formula, data = data.frame(x = x, y = y), prior = prior)

# Model Selection based on WAIC (Widely Applicable Information Criterion)
model_waic <- loo::waic(fit)
best_degree <- which.min(model_waic$waic)

# Refit the model with the selected degree
formula_best <- bf(y ~ poly(x, best_degree, raw = TRUE))
fit_best <- brm(formula_best, data = data.frame(x = x, y = y), prior = prior)

# Plotting
plot(x, y, main = "Bayesian Polynomial Regression with Unsupervised Degree", xlab = "x", ylab = "y")
lines(x, posterior_predict(fit_best, newdata = data.frame(x = x))$fit, col = "red", lwd = 2)
