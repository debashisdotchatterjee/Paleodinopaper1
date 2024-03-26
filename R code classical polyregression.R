require(ggplot2)
require(xtable)
# Example data
#x <- seq(0, 10, by = 0.5)

# Load required library
library(ggplot2)

# Define the landmark coordinates
landmark_data <- data.frame(
  x = c(491, 287, 81, 322, 510, 489, 460, 433, 402, 65, 99, 152, 152, 187, 249, 248, 231, 330, 325, 319, 539, 251, 27, 308, 287, 317, 508, 62, 504, 418, 331, 241, 171, 78),
  y = c(470, 582, 422, 78, 400, 350, 247, 437, 391, 348, 304, 210, 405, 363, 548, 509, 438, 546, 509, 439, 499, 615, 450, 80, 298, 79, 432, 377, 382, 417, 530, 530, 384, 325)
)

# Plot the landmark coordinates
ggplot(landmark_data, aes(x = x, y = y)) +
  geom_point(size = 2, color = "blue") +  # Adjust point size and color
  labs(x = "X", y = "Y", title = "Dinosaur Foot Landmark Coordinates") +  # Axis labels and title
  theme_minimal()  # Minimal theme

##############
####Centroid Size Plot 
###############

# Compute Euclidean distance between two points
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# Compute distances between all pairs of landmarks
distances <- outer(landmark_data$x, landmark_data$x, function(x1, x2) {
  outer(landmark_data$y, landmark_data$y, function(y1, y2) {
    euclidean_distance(x1, y1, x2, y2)
  })
})

# Remove diagonal elements (distance to self)
distances[lower.tri(distances)] <- NA

# Compute centroid size (mean of all pairwise distances)
centroid_size <- mean(distances, na.rm = TRUE)
print(paste("Centroid size:", centroid_size))

# Plot the landmark coordinates with centroid size
ggplot(landmark_data, aes(x = x, y = y)) +
  geom_point(size = 2, color = "blue") +  # Adjust point size and color
  geom_text(aes(label = sprintf("%.2f", centroid_size)), x = max(landmark_data$x), y = min(landmark_data$y), color = "red", hjust = 1, vjust = 0) +
  labs(x = "X", y = "Y", title = "Dinosaur Foot Landmark Coordinates") +  # Axis labels and title
  theme_minimal()  # Minimal theme

####################
# Load required library
library(ggplot2)

# Define the landmark coordinates
landmark_data <- data.frame(
  x = c(491, 287, 81, 322, 510, 489, 460, 433, 402, 65, 99, 152, 152, 187, 249, 248, 231, 330, 325, 319, 539, 251, 27, 308, 287, 317, 508, 62, 504, 418, 331, 241, 171, 78),
  y = c(470, 582, 422, 78, 400, 350, 247, 437, 391, 348, 304, 210, 405, 363, 548, 509, 438, 546, 509, 439, 499, 615, 450, 80, 298, 79, 432, 377, 382, 417, 530, 530, 384, 325)
)

# Compute centroid size
centroid_x <- mean(landmark_data$x)
centroid_y <- mean(landmark_data$y)
centroid <- c(centroid_x, centroid_y)

# Compute distances from each landmark to the centroid
distances <- sqrt((landmark_data$x - centroid_x)^2 + (landmark_data$y - centroid_y)^2)

# Compute centroid size
centroid_size <- sqrt(mean(distances^2))

# Plot the landmark coordinates with the centroid
ggplot(landmark_data, aes(x = x, y = y)) +
  geom_point(size = 2, color = "blue") +  # Adjust point size and color
  geom_point(aes(x = centroid_x, y = centroid_y), color = "red", size = 3, shape = 4) +  # Plot centroid
  geom_text(aes(x = centroid_x, y = centroid_y, label = paste("Centroid Size:", round(centroid_size, 2))), vjust = 1.5, color = "red") +  # Label centroid size
  labs(x = "X", y = "Y", title = "Dinosaur Foot Landmark Coordinates") +  # Axis labels and title
  theme_minimal()  # Minimal theme

#######################
#y <- x^2 - 3*x + rnorm(length(x), mean = 0, sd = 5)
library(readxl)
ex_CS_FLCM <- read_excel("C:/Users/DELL/Dropbox/dinosaur foot sanjukta 2024/datasets/ex_CS_FLCM.xlsx");
dinosaur_data=ex_CS_FLCM
maindata<- dinosaur_data[, c(ncol(dinosaur_data)-1, ncol(dinosaur_data))]
x=maindata[,1]
y=maindata[,2]

# Load necessary libraries
library(ggplot2)
library(xtable)

# Assuming your dataset is named 'dinosaur_data'
# Replace 'dinosaur_data' with the actual name of your dataset

# Extracting the last two columns
maindata <- dinosaur_data[, c(ncol(dinosaur_data)-1, ncol(dinosaur_data))]

# Renaming columns for convenience
colnames(maindata) <- c("Centroid_Size", "Footprint_Length_to_Center_of_Mass")


#################
# Filter data for Centroid_Size > 50
filtered_data <- maindata[maindata$Centroid_Size > 50, ]

# Check if there are any data points remaining after filtering
if (nrow(filtered_data) == 0) {
  stop("No data points found for Centroid_Size > 50!")
}

# Perform linear regression using lm function
model <- lm(Footprint_Length_to_Center_of_Mass ~ Centroid_Size, data = filtered_data)

# Print model summary
summary(model)

###############
# Perform linear regression analysis
filtered_data <- maindata[maindata$Centroid_Size > 50, ]

if (nrow(filtered_data) == 0) {
  stop("No data points found for Centroid_Size > 50!")
}

#model <- lm(Footprint_Length_to_ Center_of_Mass ~ Centroid_Size, data = filtered_data)
model <- lm(Footprint_Length_to_Center_of_Mass ~ Centroid_Size, data = filtered_data)

# Extract R-squared fit value
r_squared <- summary(model)$r.squared

# ggplot for the relationship
library(ggplot2)
ggplot(filtered_data, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  labs(title = paste0("CS vs. FL-CM (Centroid Size > 50)", "\nR-squared:", round(r_squared, 2)),
       x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL-CM)") +
  geom_smooth(method = lm, se = FALSE)  # Add regression line

# Print R-squared fit value separately
cat("R-squared Fit Value:", round(r_squared, 2), "\n")

##############
# Perform linear regression
lm_model <- lm(Footprint_Length_to_Center_of_Mass ~ Centroid_Size, data = maindata)

# Summary of the regression model
summary(lm_model)
summary_output <- summary(lm_model)

# Convert summary output to LaTeX format
latex_output <- xtable(summary_output)

# Print LaTeX code
print(latex_output, include.rownames = TRUE)
# Plotting the data points and regression line
ggplot(maindata, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL_CM)", 
       title = "Linear Regression Analysis of Dinosaur Footprints") +
  theme_minimal()

############

# Perform linear regression
lm_model <- lm(Footprint_Length_to_Center_of_Mass ~ Centroid_Size, data = maindata)

# Extract the correlation coefficient
correlation <- cor(maindata$Centroid_Size, maindata$Footprint_Length_to_Center_of_Mass)

# Plotting the data points and regression line
ggplot(maindata, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text(aes(x = max(Centroid_Size), y = min(Footprint_Length_to_Center_of_Mass), 
                label = paste("Correlation =", round(correlation, 2))),
            hjust = 1, vjust = 0, size = 4, color = "black") +
  labs(x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL_CM)", 
       title = "Linear Regression Analysis of Dinosaur Footprints") +
  theme_minimal()
###################
#Heteroskedasticity
###################
# Plotting residuals against predicted values
plot(lm_model$fitted.values, residuals(lm_model),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted Values Plot")
abline(h = 0, col = "red", lty = 2)

######
# Plotting residuals against predicted values
residuals_vs_fitted <- data.frame(Fitted_Values = lm_model$fitted.values,
                                  Residuals = residuals(lm_model))
ggplot(residuals_vs_fitted, aes(x = Fitted_Values, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Fitted Values", y = "Residuals",
       title = "Residuals vs Fitted Values Plot")

# Load necessary library
library(lmtest)

# Breusch-Pagan test for heteroskedasticity
bptest(lm_model)
#xtable(bptest(lm_model))
# Breusch-Pagan test for heteroskedasticity
bp_test <- bptest(lm_model)
# Load necessary library
library(lmtest)

# White test for heteroskedasticity
bptest(lm_model, ~ fitted(lm_model) + I(fitted(lm_model)^2))

#######
#Addressing Heteroskedasticity in our context
# Example: Logarithmic transformation of the dependent variable
maindata$log_FL_CM <- log(maindata$Footprint_Length_to_Center_of_Mass)
# Example: Implementing WLS regression
# Define weights inversely proportional to the variance of the residuals
weights <- 1 / residuals(lm_model)^2
# Fit WLS regression model
wls_model <- lm(Footprint_Length_to_Center_of_Mass ~ Centroid_Size, data = maindata, weights = weights)
# Plotting the data points and WLS regression line
ggplot(maindata, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, weights = weights, se = FALSE, color = "blue") +
  labs(x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL_CM)", 
       title = "Weighted Least Squares Regression Analysis of Dinosaur Footprints") +
  theme_minimal()

#######
# Create a data frame for plotting
plot_data <- data.frame(Centroid_Size = maindata$Centroid_Size,
                        Footprint_Length_to_Center_of_Mass = maindata$Footprint_Length_to_Center_of_Mass,
                        Fitted_Values = fitted(wls_model),
                        Residuals = residuals(wls_model))

# Calculate correlation coefficient
correlation_wls <- cor(maindata$Centroid_Size, maindata$Footprint_Length_to_Center_of_Mass)

# Plotting the data points, regression line, and correlation coefficient
ggplot(plot_data, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  geom_line(aes(y = Fitted_Values), color = "blue", size = 1) +
  geom_text(aes(x = max(Centroid_Size), y = min(Footprint_Length_to_Center_of_Mass),
                label = paste("Correlation =", round(correlation_wls, 2))),
            hjust = 1, vjust = 0, size = 4, color = "black") +
  labs(x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL_CM)", 
       title = "Weighted Least Squares Regression Analysis",
       caption = "Correlation coefficient displayed on the plot") +
  theme_minimal()

xtable(wls_model)
#########

# Example: Using robust standard error estimation (Huber-White/sandwich estimator)
# Load necessary library
library(sandwich)
library(lmtest)
# Obtain robust standard errors
robust_se <- sqrt(diag(vcovHC(lm_model, type = "HC1")))
#xtable(robust_se)

# Obtain robust standard errors
robust_se <- sqrt(diag(vcovHC(lm_model, type = "HC1")))

# Plotting the data points and regression line with robust standard errors
ggplot(maindata, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Ordinary least squares regression line
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed", formula = y ~ x, aes(group = 1), size = 1) +  # Robust standard errors
  labs(x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL_CM)", 
       title = "Robust Standard Error Estimation", 
       caption = "95% Confidence Interval (dashed lines represent robust standard errors)") +
  theme_minimal()
###########

# Plotting the data points and regression line with robust standard errors
# Plotting the data points and regression line with robust standard errors
ggplot(maindata, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Ordinary least squares regression line
  geom_smooth(method = "lm", se = TRUE, fill = "lightblue", color = "red", linetype = "dashed", formula = y ~ x, aes(group = 1), size = 1) +  # Robust standard errors
  labs(x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL_CM)", 
       title = "Robust Standard Error Estimation", 
       caption = "95% Confidence Interval (shaded area)") +
  theme_minimal()

#######################POLYNOMIAL REGRESSIOn

# Load necessary library
library(ggplot2)

# Function to calculate mean squared error
mse <- function(true, pred) {
  mean((true - pred)^2)
}

# Define a range of polynomial degrees to consider
max_degree <- 10
degrees <- 1:max_degree

# Initialize vectors to store cross-validated error
cv_errors <- numeric(length(degrees))

# Perform cross-validation for each polynomial degree
for (i in 1:length(degrees)) {
  # Fit polynomial regression model
  poly_model <- lm(Footprint_Length_to_Center_of_Mass ~ poly(Centroid_Size, degrees[i]), data = maindata)
  
  # Calculate cross-validated error using 5-fold cross-validation
  cv_errors[i] <- mean(sapply(split(maindata, cut(seq(nrow(maindata)), breaks = 5)), function(indices) {
    test_data <- maindata[indices, ]
    train_data <- maindata[-indices, ]
    pred <- predict(poly_model, newdata = test_data)
    mse(test_data$Footprint_Length_to_Center_of_Mass, pred)
  }))
}

# Determine the degree with the lowest cross-validated error
optimal_degree <- degrees[which.min(cv_errors)]

# Fit the final polynomial regression model with the optimal degree
final_poly_model <- lm(Footprint_Length_to_Center_of_Mass ~ poly(Centroid_Size, optimal_degree), data = maindata)

# Plot cross-validated error versus polynomial degree
ggplot(data.frame(Degree = degrees, CV_Error = cv_errors), aes(x = Degree, y = CV_Error)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  geom_vline(xintercept = optimal_degree, linetype = "dashed") +
  labs(x = "Polynomial Degree", y = "Cross-Validated Error",
       title = "Cross-Validated Error vs. Polynomial Degree",
       caption = paste("Optimal Degree:", optimal_degree)) +
  theme_minimal()

# Plot the final polynomial regression model
newdata <- data.frame(Centroid_Size = seq(min(maindata$Centroid_Size), max(maindata$Centroid_Size), length.out = 100))
pred <- predict(final_poly_model, newdata = newdata)
ggplot(maindata, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  geom_line(data = newdata, aes(y = pred), color = "blue") +
  labs(x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL_CM)",
       title = paste("Polynomial Regression (Degree", optimal_degree, ")"),
       caption = "Fitted polynomial regression line") +
  theme_minimal()

#################Polynomial Regression on Weighted Dataset
# Load necessary library
library(ggplot2)

# Define a range of polynomial degrees to consider
max_degree <- 10
degrees <- 1:max_degree

# Initialize vectors to store cross-validated error
cv_errors <- numeric(length(degrees))

# Perform cross-validation for each polynomial degree
for (i in 1:length(degrees)) {
  # Fit polynomial regression model with weights
  poly_model <- lm(Footprint_Length_to_Center_of_Mass ~ poly(Centroid_Size, degrees[i]), data = maindata, weights = maindata$Weights)
  
  # Calculate cross-validated error using 5-fold cross-validation
  cv_errors[i] <- mean(sapply(split(maindata, cut(seq(nrow(maindata)), breaks = 5)), function(indices) {
    test_data <- maindata[indices, ]
    train_data <- maindata[-indices, ]
    pred <- predict(poly_model, newdata = test_data)
    mse(test_data$Footprint_Length_to_Center_of_Mass, pred)
  }))
}

# Determine the degree with the lowest cross-validated error
optimal_degree <- degrees[which.min(cv_errors)]

# Fit the final polynomial regression model with the optimal degree
final_poly_model <- lm(Footprint_Length_to_Center_of_Mass ~ poly(Centroid_Size, optimal_degree), data = maindata, weights = maindata$Weights)

# Plot cross-validated error versus polynomial degree
ggplot(data.frame(Degree = degrees, CV_Error = cv_errors), aes(x = Degree, y = CV_Error)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  geom_vline(xintercept = optimal_degree, linetype = "dashed") +
  labs(x = "Polynomial Degree", y = "Cross-Validated Error",
       title = "Cross-Validated Error vs. Polynomial Degree",
       caption = paste("Optimal Degree:", optimal_degree)) +
  theme_minimal()

# Plot the final polynomial regression model
newdata <- data.frame(Centroid_Size = seq(min(maindata$Centroid_Size), max(maindata$Centroid_Size), length.out = 100))
pred <- predict(final_poly_model, newdata = newdata)
ggplot(maindata, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  geom_line(data = newdata, aes(y = pred), color = "blue") +
  labs(x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL_CM)",
       title = paste("Polynomial Regression (Degree", optimal_degree, ")"),
       caption = "Fitted polynomial regression line") +
  theme_minimal()
###############################
# Load necessary libraries
library(ggplot2)

# Partition the dataset where CS > 50
subset_data <- maindata[maindata$Centroid_Size > 50, ]

# Define polynomial degree
degree <- 12  # You can change the degree as needed

# Fit polynomial regression model
poly_model <- lm(Footprint_Length_to_Center_of_Mass ~ poly(Centroid_Size, degree), data = subset_data)

# Plot the polynomial regression model
newdata <- data.frame(Centroid_Size = seq(min(subset_data$Centroid_Size), max(subset_data$Centroid_Size), length.out = 100))
pred <- predict(poly_model, newdata = newdata)
ggplot(subset_data, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  geom_line(data = newdata, aes(y = pred), color = "blue") +
  labs(x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL_CM)",
       title = paste("Polynomial Regression Analysis (Degree", degree, ") for CS > 50"),
       caption = "Fitted polynomial regression line") +
  theme_minimal()
#############################
#
# Load necessary library
library(ggplot2)

# Subset the dataset based on the condition (CS > 30)
subset_data <- maindata[maindata$Centroid_Size > 30, ]

# Function to calculate mean squared error
mse <- function(true, pred) {
  mean((true - pred)^2)
}

# Define a range of polynomial degrees to consider
max_degree <- 16
degrees <- 1:max_degree

# Initialize vectors to store cross-validated error
cv_errors <- numeric(length(degrees))

# Perform cross-validation for each polynomial degree
for (i in 1:length(degrees)) {
  # Fit polynomial regression model
  poly_model <- lm(Footprint_Length_to_Center_of_Mass ~ poly(Centroid_Size, degrees[i]), data = subset_data)
  
  # Calculate cross-validated error using 5-fold cross-validation
  cv_errors[i] <- mean(sapply(split(seq(nrow(subset_data)), cut(seq(nrow(subset_data)), breaks = 5)), function(indices) {
    test_data <- subset_data[indices, ]
    train_data <- subset_data[-indices, ]
    pred <- predict(poly_model, newdata = test_data)
    mse(test_data$Footprint_Length_to_Center_of_Mass, pred)
  }))
}

# Determine the degree with the lowest cross-validated error
optimal_degree <- degrees[which.min(cv_errors)]

# Fit the final polynomial regression model with the optimal degree
final_poly_model <- lm(Footprint_Length_to_Center_of_Mass ~ poly(Centroid_Size, optimal_degree), data = subset_data)

# Plot cross-validated error versus polynomial degree
ggplot(data.frame(Degree = degrees, CV_Error = cv_errors), aes(x = Degree, y = CV_Error)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  geom_vline(xintercept = optimal_degree, linetype = "dashed") +
  labs(x = "Polynomial Degree", y = "Cross-Validated Error",
       title = "Cross-Validated Error vs. Polynomial Degree",
       caption = paste("Optimal Degree:", optimal_degree)) +
  theme_minimal()

# Plot the final polynomial regression model
newdata <- data.frame(Centroid_Size = seq(min(subset_data$Centroid_Size), max(subset_data$Centroid_Size), length.out = 100))
pred <- predict(final_poly_model, newdata = newdata)
ggplot(subset_data, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  geom_line(data = newdata, aes(y = pred), color = "blue") +
  labs(x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL_CM)",
       title = paste("Polynomial Regression (Degree", optimal_degree, ")"),
       caption = "Fitted polynomial regression line") +
  theme_minimal()
#########################
#Bayesian Heirarchical Inferences
#########################


# Load necessary packages
library(rstan)

# Define the Stan model
stan_code <- '
data {
  int<lower=0> N;                    // Number of observations
  vector[N] CS;                      // Centroid size
  vector[N] FL_CM;                   // Footprint length relative to center of mass
}
parameters {
  real beta0;                        // Intercept
  real beta1;                        // Slope
  real<lower=0> sigma2;              // Error variance
  real<lower=0> tau2;                // Hyperparameter for coefficients
}
model {
  // Priors
  beta0 ~ normal(0, 10);             // Weakly informative prior for intercept
  beta1 ~ normal(0, 10);             // Weakly informative prior for slope
  sigma2 ~ inv_gamma(0.001, 0.001);  // Weakly informative prior for error variance
  tau2 ~ inv_gamma(0.001, 0.001);    // Weakly informative prior for hyperparameter

  // Likelihood
  for (i in 1:N) {
    CS[i] ~ normal(beta0 + beta1 * FL_CM[i], sqrt(sigma2));
  }
}
'

# Prepare data
maindata <- dinosaur_data[, c(ncol(dinosaur_data)-1, ncol(dinosaur_data))]
colnames(maindata) <- c("Centroid_Size", "Footprint_Length_to_Center_of_Mass")

stan_data <- list(
  N = nrow(maindata),
  CS = maindata$Centroid_Size,
  FL_CM = maindata$Footprint_Length_to_Center_of_Mass
)

# Compile the Stan model
stan_model <- stan_model(model_code = stan_code)

# Fit the model
fit <- sampling(stan_model, data = stan_data, chains = 4, iter = 2000, warmup = 1000)

# Print summary of the fitted model
print(fit)

##############

#Posterior Distributions of model parameters
#############

library(ggplot2)
library(gridExtra)

# Convert posterior samples to data frame
posterior_samples_df <- as.data.frame(fit)

# Plot posterior distributions of model parameters
posterior_plots <- lapply(names(posterior_samples_df), function(param) {
  ggplot(posterior_samples_df, aes(x = .data[[param]])) +
    geom_density(fill = "skyblue", color = "black") +
    labs(title = paste("Posterior Distribution of", param),
         x = param, y = "Density")
})

# Combine plots
grid.arrange(grobs = posterior_plots)
###############
#another Experiment
#################

library(ggplot2)
library(gridExtra)

# Convert posterior samples to data frame
posterior_samples_df <- as.data.frame(fit)

# Plot posterior distributions of model parameters
posterior_plots <- lapply(names(posterior_samples_df), function(param) {
  ggplot(posterior_samples_df, aes(x = .data[[param]])) +
    geom_density(fill = "skyblue", color = "black") +
    labs(title = paste("Posterior Distribution of", param),
         x = param, y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(size = 8),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 8))
})

# Combine plots with adjusted size
grid.arrange(grobs = posterior_plots, ncol = 2, widths = c(3, 3))

##Predictive Inferences

#####################
# Generate new values of FL_CM for prediction
FL_CM_new <- c(0.5, 0.6, 0.7)  # Example values of FL_CM for prediction

# Extract posterior samples from the fitted model
posterior_samples <- as.matrix(fit)

# Define function for predictive inference
predictive_inference <- function(FL_CM_new, posterior_samples) {
  # Extract posterior samples of parameters
  beta0_samples <- posterior_samples[, "beta0"]
  beta1_samples <- posterior_samples[, "beta1"]
  sigma2_samples <- posterior_samples[, "sigma2"]
  
  # Initialize vector for storing predicted CS values
  CS_predicted <- numeric(length(FL_CM_new))
  
  # Iterate over each new value of FL_CM for prediction
  for (i in 1:length(FL_CM_new)) {
    # Calculate predicted CS using posterior samples of parameters
    CS_predicted_samples <- rnorm(nrow(posterior_samples),
                                  mean = beta0_samples + beta1_samples * FL_CM_new[i],
                                  sd = sqrt(sigma2_samples))
    # Compute mean of predicted CS values
    CS_predicted[i] <- mean(CS_predicted_samples)
  }
  
  return(CS_predicted)
}

# Perform predictive inference
CS_predicted <- predictive_inference(FL_CM_new, posterior_samples)

# Print predicted CS values
print(CS_predicted)

# Create a data frame with predicted CS values and corresponding FL_CM values
prediction_df <- data.frame(FL_CM_new = FL_CM_new, CS_predicted = CS_predicted)

# Plot predicted CS values
predictive_plot <- ggplot(prediction_df, aes(x = FL_CM_new, y = CS_predicted)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "red", size = 1) +
  labs(title = "Predictive Inference",
       x = "Footprint Length to Center of Mass (FL_CM)",
       y = "Predicted Centroid Size (CS)")

# Display the plot
print(predictive_plot)

################################
#BIC BASED OPTIMAL POLYNOMIAL REGRESSION FIT
##############################
# Load necessary libraries
library(polynom)
library(ggplot2)

# Data
maindata <- dinosaur_data[, c(ncol(dinosaur_data)-1, ncol(dinosaur_data))]
colnames(maindata) <- c("Centroid_Size", "Footprint_Length_to_Center_of_Mass")

# Function to fit polynomial regression and calculate BIC
fit_polynomial <- function(data, degree) {
  # Fit polynomial regression
  fit <- lm(Footprint_Length_to_Center_of_Mass ~ poly(Centroid_Size, degree, raw = TRUE), data = data)
  
  # Calculate BIC
  n <- nrow(data)
  RSS <- sum(residuals(fit)^2)
  k <- degree + 1  # Number of parameters including intercept
  BIC <- n * log(RSS/n) + k * log(n)
  
  return(list(fit = fit, BIC = BIC))
}

# Perform polynomial regression fits for degrees 1 to 5
degrees <- 1:15
fits <- lapply(degrees, function(degree) fit_polynomial(maindata, degree))

# Extract BIC values
BIC_values <- sapply(fits, function(fit) fit$BIC)

# Find optimal degree with minimum BIC
optimal_degree <- degrees[which.min(BIC_values)]
cat("Optimal Degree:", optimal_degree, "\n")

# Plot BIC values
ggplot(data.frame(Degree = degrees, BIC = BIC_values), aes(x = Degree, y = BIC)) +
  geom_line() +
  geom_point(color = "blue") +
  labs(title = "BIC-based Optimal Polynomial Regression Fit",
       x = "Degree of Polynomial", y = "BIC") +
  theme_minimal()
################
#################

#for CS>90 

# Subset data where Centroid_Size is greater than 60
subset_data <- maindata[maindata$Centroid_Size > 90, ]

# Perform polynomial regression fits for degrees 1 to 5
fits <- lapply(degrees, function(degree) fit_polynomial(subset_data, degree))

# Extract BIC values
BIC_values <- sapply(fits, function(fit) fit$BIC)

# Find optimal degree with minimum BIC
optimal_degree <- degrees[which.min(BIC_values)]
cat("Optimal Degree:", optimal_degree, "\n")

# Plot BIC values
# Plot BIC values
ggplot(data.frame(Degree = degrees, BIC = BIC_values), aes(x = Degree, y = BIC)) +
  geom_line() +
  geom_point(color = "blue") +
  geom_vline(xintercept = optimal_degree, linetype = "dashed", color = "red") +  # Vertical line for optimal degree
  annotate("text", x = optimal_degree, y = max(BIC_values), label = paste("Optimal Degree:", optimal_degree), vjust = -0.5, color = "red") +  # Text annotation for optimal degree
  labs(title = "BIC-based Optimal Polynomial Regression Fit (CS > 60)",
       x = "Degree of Polynomial", y = "BIC") +
  theme_minimal()

# Plot BIC values with optimal degree representation
ggplot(data.frame(Degree = degrees, BIC = BIC_values), aes(x = Degree, y = BIC)) +
  geom_line() +
  geom_point(color = "blue") +
  geom_vline(xintercept = optimal_degree, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = optimal_degree + 0.2, y = max(BIC_values), label = paste("Optimal Degree:", optimal_degree), color = "red", size = 5, hjust = 0) +
  labs(title = "BIC-based Optimal Polynomial Regression Fit (CS > 90)",
       x = "Degree of Polynomial", y = "BIC") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12))  # Adjust

####Draw with degree 5
library(ggplot2)

# Partition the dataset where CS > 50
subset_data <- maindata[maindata$Centroid_Size > 50, ]

# Define polynomial degree
degree <- 5  # You can change the degree as needed

# Fit polynomial regression model
poly_model <- lm(Footprint_Length_to_Center_of_Mass ~ poly(Centroid_Size, degree), data = subset_data)

# Plot the polynomial regression model
newdata <- data.frame(Centroid_Size = seq(min(subset_data$Centroid_Size), max(subset_data$Centroid_Size), length.out = 100))
pred <- predict(poly_model, newdata = newdata)
ggplot(subset_data, aes(x = Centroid_Size, y = Footprint_Length_to_Center_of_Mass)) +
  geom_point() +
  geom_line(data = newdata, aes(y = pred), color = "blue") +
  labs(x = "Centroid Size (CS)", y = "Footprint Length to Center of Mass (FL_CM)",
       title = paste("Polynomial Regression Analysis (Degree", degree, ") for CS > 90"),
       caption = "Fitted polynomial regression line") +
  theme_minimal()+
  theme(plot.title = element_text(size = 12))  # Adjust

#############################