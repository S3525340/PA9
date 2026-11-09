# Daryion King 
# 11/9/2026
# Fuel_Efficiency_Model.r
# A model of fuel efficiency, measured in miles per gallon (MPG),
# as a function of vehicle weight (in thousands of pounds)

# Loading in mtcars dataset
data(mtcars)


# Plotting the data
plot(mtcars$wt, mtcars$mpg,
     main = "Vehicle Weight vs. Fuel Efficiency",
     xlab = "Vehicle Weight (Thousands of Pounds)",
     ylab = "Miles Per Gallon (MPG)",
     pch = 19,
     col = "blue"
     )


# Linear Regression model
mpg_model <- lm(mpg ~ wt, data = mtcars)

abline(mpg_model, col = "red", lwd = 2)


# Interperting the slope
model_summary <- summary(mpg_model)
slope <- coef(mpg_model)["wt"]
p_value <- model_summary$coefficients["wt", "Pr(>|t|)"]
slope_ci <- confint(mpg_model, "wt", level = 0.95)

cat("Slope Estimate:", slope, "\n")
cat("p-value:", p_value, "\n")
cat("95% Confidence Interval for Slope:",
    slope_ci[1], slope_ci[2], "\n")

if (p_value < 0.05) {
  cat("The slope is statistically significant at the 0.05 level.\n")
  cat("Vehicle weight has a significant relationship with MPG.\n")
} else {
  cat("The slope is not statistically significant at 0.05.\n")
}


# Predicting MPG for a vehicles weight
vehicle_weight <- 6000 / 1000
new_vehicle <- data.frame(wt = vehicle_weight)

mpg_prediction <- predict(mpg_model,
                          newdata = new_vehicle,
                          interval = "prediction",
                          level = 0.95)

cat("Predicted MPG for a 6,000-pound vehicle:\n")
print(mpg_prediction)


# Evaluating the models prediction
min_weight <- min(mtcars$wt)
max_weight <- max(mtcars$wt)

cat("\nDataset Weight Range:", min_weight, "to", max_weight,
    "thousand pounds.\n")

if (vehicle_weight < min_weight ||
    vehicle_weight > max_weight) {
  cat("The vehicle weight is outside the dataset range.\n")
  cat("Use caution when interpreting this prediction.\n")
} else {
  cat("The vehicle weight is within the dataset range.\n")
  cat("The prediction is interpolation, but uncertainty remains.\n")
}
