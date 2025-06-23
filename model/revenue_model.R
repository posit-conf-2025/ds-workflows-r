# Simple Revenue Prediction Model
# Using tidymodels to predict revenue from coca cola sales data

library(tidymodels)
library(dplyr)
library(readr)

# Load the clean dataset
coca_cola_data <- read_csv("coca_cola_sales_data_clean.csv")

# Simple data exploration
cat("Dataset dimensions:", nrow(coca_cola_data), "x", ncol(coca_cola_data), "\n")
cat("Revenue range: $", round(min(coca_cola_data$revenue), 2), 
    " to $", round(max(coca_cola_data$revenue), 2), "\n")

# Prepare the data for modeling
model_data <- coca_cola_data %>%
  select(units_sold, unit_price, product, region, store_type, revenue) %>%
  # Convert character variables to factors
  mutate(
    product = as.factor(product),
    region = as.factor(region),
    store_type = as.factor(store_type)
  )

# Split the data
set.seed(123)
data_split <- initial_split(model_data, prop = 0.8, strata = revenue)
train_data <- training(data_split)
test_data <- testing(data_split)

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

# Create a simple recipe
revenue_recipe <- recipe(revenue ~ ., data = train_data) %>%
  # Create dummy variables for categorical predictors
  step_dummy(all_nominal_predictors()) %>%
  # Normalize numeric predictors
  step_normalize(all_numeric_predictors())

# Specify the model (simple linear regression)
linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Create workflow
revenue_workflow <- workflow() %>%
  add_recipe(revenue_recipe) %>%
  add_model(linear_model)

# Fit the model
cat("Training model...\n")
fitted_model <- revenue_workflow %>%
  fit(data = train_data)

cat("Model training complete!\n")

# Make predictions on test set
predictions <- fitted_model %>%
  predict(test_data) %>%
  bind_cols(test_data)

# Calculate model performance
model_metrics <- predictions %>%
  metrics(truth = revenue, estimate = .pred)

cat("\nModel Performance:\n")
print(model_metrics)

# Show some example predictions
cat("\nSample Predictions:\n")
sample_predictions <- predictions %>%
  select(units_sold, unit_price, product, region, store_type, 
         actual_revenue = revenue, predicted_revenue = .pred) %>%
  slice_head(n = 10)

print(sample_predictions)

# Save the fitted model for API deployment
saveRDS(fitted_model, "revenue_model.rds")
cat("\nModel saved as 'revenue_model.rds'\n")

# Create a simple prediction function for API use
predict_revenue <- function(units_sold, unit_price, product, region, store_type) {
  # Load model if not already loaded
  if (!exists("fitted_model")) {
    fitted_model <- readRDS("revenue_model.rds")
  }
  
  # Create input data frame
  input_data <- data.frame(
    units_sold = units_sold,
    unit_price = unit_price,
    product = as.factor(product),
    region = as.factor(region),
    store_type = as.factor(store_type)
  )
  
  # Make prediction
  prediction <- fitted_model %>%
    predict(input_data)
  
  return(prediction$.pred[1])
}

# Test the prediction function
test_prediction <- predict_revenue(
  units_sold = 50,
  unit_price = 1.50,
  product = "Coca-Cola Classic",
  region = "North America",
  store_type = "Supermarket"
)

cat("\nTest prediction for 50 units at $1.50 each:", round(test_prediction, 2), "\n")
cat("Expected revenue (50 * 1.50):", 50 * 1.50, "\n")

