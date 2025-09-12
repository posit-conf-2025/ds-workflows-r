library(tidyverse)

# Read in Data
cola_sales_data <- read_csv("posit_cola_sales_data_sample.csv")

# Create a ggplot visualization
ggplot(sales_data, aes(x = unit_price, y = units_sold, color = revenue)) +
    geom_point(alpha = 0.7) +
    scale_color_gradient(low = blue, high = red) +
    labs(title = "Units Sold vs Unit Price (colored by Revenue)",
        x = "Unit Price",
        y = "Units Sold",
        color = "Revenue") +
    theme_minimal()  

