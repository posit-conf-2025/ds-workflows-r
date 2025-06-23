# Create monitoring dataset that matches posit_cola_sales_clean structure
library(dplyr)
library(lubridate)

# Set seed for reproducibility
set.seed(456)  # Different seed for monitoring data

# Create monitoring dataset (1000 rows)
n_monitoring <- 1000

# Match the exact structure of posit_cola_sales_clean
monitoring_data <- tibble(
  # Recent dates (simulating new data for 2024)
  date = sample(seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day"), 
                n_monitoring, replace = TRUE),
  
  # Same products with slight distribution changes
  product = sample(c("Posit Classic", "Posit Zero", "Posit Lite", "Quarto Fizz", 
                    "Connect Orange", "Workbench Energy", "Cloud Water"), 
                  n_monitoring, replace = TRUE, 
                  prob = c(0.38, 0.16, 0.16, 0.13, 0.09, 0.04, 0.04)),
  
  # Same regions with slight market evolution
  region = sample(c("North America", "Europe", "Asia Pacific", "Latin America", "Africa"), 
                 n_monitoring, replace = TRUE, 
                 prob = c(0.33, 0.27, 0.26, 0.09, 0.05)),
  
  # Same store types
  store_type = sample(c("Supermarket", "Convenience Store", "Restaurant", 
                       "Vending Machine", "Gas Station"), 
                     n_monitoring, replace = TRUE),
  
  # Store IDs in similar range
  store_id = sample(1:8000, n_monitoring, replace = TRUE)
) |>
  mutate(
    # Add seasonal effects for 2024 data
    month = month(date),
    
    # Base units with seasonal patterns (similar to training data)
    base_units = case_when(
      month %in% c(6, 7, 8) ~ rpois(n(), lambda = 48),  # Summer boost
      month %in% c(11, 12) ~ rpois(n(), lambda = 42),   # Holiday season
      month %in% c(1, 2) ~ rpois(n(), lambda = 28),     # Post-holiday
      TRUE ~ rpois(n(), lambda = 37)                     # Regular months
    ),
    
    # Product-specific adjustments (with slight market evolution)
    product_multiplier = case_when(
      product == "Posit Classic" ~ runif(n(), 1.1, 1.7),  # Slight decline
      product == "Diet Coke" ~ runif(n(), 0.85, 1.25),        # Slight growth
      product == "Coke Zero" ~ runif(n(), 0.75, 1.15),        # Growth
      product == "Sprite" ~ runif(n(), 0.95, 1.35),           # Stable
      product == "Fanta Orange" ~ runif(n(), 0.65, 1.05),     # Slight growth
      product == "Coca-Cola Energy" ~ runif(n(), 0.35, 0.75), # Growth category
      TRUE ~ runif(n(), 0.85, 1.25)                           # Water growing
    ),
    
    # Regional adjustments with market changes
    region_multiplier = case_when(
      region == "North America" ~ runif(n(), 1.2, 1.6),   # Slight decline
      region == "Europe" ~ runif(n(), 1.15, 1.45),        # Stable
      region == "Asia Pacific" ~ runif(n(), 1.05, 1.35),  # Growth
      region == "Latin America" ~ runif(n(), 0.85, 1.15), # Recovery
      TRUE ~ runif(n(), 0.65, 0.95)                       # Growth
    ),
    
    # Store type effects
    store_multiplier = case_when(
      store_type == "Supermarket" ~ runif(n(), 1.4, 2.3),
      store_type == "Convenience Store" ~ runif(n(), 0.85, 1.6),
      store_type == "Restaurant" ~ runif(n(), 1.1, 1.9),  # Recovery
      store_type == "Vending Machine" ~ runif(n(), 0.35, 0.85),
      TRUE ~ runif(n(), 0.75, 1.35)
    ),
    
    # 2024 growth factor
    growth_factor = runif(n(), 1.10, 1.25),  # 10-25% growth
    
    # Calculate units sold
    units_sold = pmax(1, round(base_units * product_multiplier * region_multiplier * 
                              store_multiplier * growth_factor)),
    
    # Unit prices with inflation (matching training data patterns)
    base_price = case_when(
      product == "Coca-Cola Classic" ~ runif(n(), 1.30, 1.95),  # Inflation
      product == "Diet Coke" ~ runif(n(), 1.35, 2.00),
      product == "Coke Zero" ~ runif(n(), 1.35, 2.00),
      product == "Sprite" ~ runif(n(), 1.25, 1.90),
      product == "Fanta Orange" ~ runif(n(), 1.20, 1.85),
      product == "Coca-Cola Energy" ~ runif(n(), 2.70, 3.80),
      TRUE ~ runif(n(), 0.90, 1.35)  # Water
    ),
    
    # Regional and channel pricing adjustments
    unit_price = base_price * case_when(
      region == "North America" ~ runif(n(), 1.15, 1.35),
      region == "Europe" ~ runif(n(), 1.25, 1.50),
      TRUE ~ runif(n(), 0.85, 1.15)
    ) * case_when(
      store_type == "Vending Machine" ~ runif(n(), 1.35, 1.70),
      store_type == "Restaurant" ~ runif(n(), 1.50, 1.90),
      TRUE ~ runif(n(), 0.95, 1.15)
    ),
    
    # Promotional campaigns
    promotion = sample(c("None", "Summer Sale", "BOGO", "Holiday Special", "New Product Launch"), 
                      n_monitoring, replace = TRUE, 
                      prob = c(0.65, 0.15, 0.10, 0.05, 0.05)),
    
    # Apply promotion discounts
    unit_price = if_else(promotion != "None", 
                        unit_price * runif(n(), 0.80, 0.90), 
                        unit_price),
    
    # Calculate revenue
    revenue = units_sold * unit_price
  ) |>
  # Select EXACT same columns as coca_cola_sales_clean
  select(date, product, region, store_type, store_id, units_sold, unit_price, revenue, promotion)

# Display summary
cat("Monitoring Dataset Created!\n")
cat("Records:", nrow(monitoring_data), "\n")
cat("Columns:", ncol(monitoring_data), "\n")
cat("Column names match:", identical(names(monitoring_data), names(coca_cola_data)), "\n")

# Show structure comparison
cat("\nColumn structure:\n")
str(monitoring_data)

# Show summary statistics
cat("\nSummary of monitoring data:\n")
summary(monitoring_data)

# Preview the data
cat("\nFirst 10 rows:\n")
head(monitoring_data, 10)

# Save the monitoring dataset
write.csv(monitoring_data, "data/posit_cola_sales_monitoring_data.csv", row.names = FALSE)
cat("\nMonitoring dataset saved as 'data/coca_cola_sales_monitoring_data.csv'\n")