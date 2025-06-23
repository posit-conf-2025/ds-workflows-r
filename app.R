library(shiny)
library(bslib)
library(dplyr)
library(httr)
library(jsonlite)
library(vetiver)

# API configuration
API_URL <- "https://connect.posit.it/content/127ec7f3-d930-4f52-8f49-52b03f0c0250/predict"
# IMPORTANT: Ensure your CONNECT_API_KEY environment variable is set
API_KEY <- Sys.getenv("CONNECT_API_KEY")

# Create vetiver endpoint
endpoint <- vetiver_endpoint(API_URL)

# Define prediction function that calls the API
predict_revenue <- function(units_sold, unit_price, product, region, store_type) {

  # Prepare the data for API request
  request_data <- tibble(
      units_sold = units_sold,
      unit_price = unit_price,
      product = product,
      region = region,
      store_type = store_type
  )

  # Make API request using vetiver
  response <- predict(endpoint,
            request_data,
            httr::add_headers(Authorization = paste("Key", API_KEY)))$.pred

  return(response)
}


# Define UI with bslib theme
ui <- page_sidebar(
  title = "🥤 Posit Cola Revenue Predictor",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#4A90E2",  # Posit blue
    base_font = font_google("Inter"),
    heading_font = font_google("Inter", wght = 600)
  ),
  
  sidebar = sidebar(
    title = "Sales Parameters",
    width = 350,
    
    card(
      card_header("Product Details"),
      selectInput("product",
                  "Product:",
                  choices = c("Posit Classic", "Posit Lite", "Workbench Energy", "Quarto Fizz", "Connect Orange", "Cloud Water"),
                  selected = "Posit Classic"),
      
      selectInput("region",
                  "Region:",
                  choices = c("North America", "Europe", "Asia Pacific", "Latin America"),
                  selected = "North America"),
      
      selectInput("store_type",
                  "Store Type:",
                  choices = c("Supermarket", "Gas Station", "Convenience Store"),
                  selected = "Supermarket")
    ),
    
    card(
      card_header("Sales Metrics"),
      numericInput("units_sold",
                   "Units Sold:",
                   value = 100,
                   min = 1,
                   max = 1000,
                   step = 1),

      numericInput("unit_price",
                   "Unit Price ($):",
                   value = 1.50,
                   min = 0.50,
                   max = 10.00,
                   step = 0.01)
    ),
    
    br(),
    actionButton("predict", 
                 "🔮 Predict Revenue", 
                 class = "btn-primary btn-lg w-100",
                 style = "font-weight: 600;")
  ),

  # Main content area
  layout_columns(
    col_widths = c(12, 12),
    
    # Prediction Results Card
    card(
      card_header(
        class = "bg-primary text-white",
        "💰 Prediction Results"
      ),
      card_body(
        div(
          class = "text-center",
          h4("Predicted Revenue:", class = "mb-3"),
          div(
            textOutput("predicted_revenue"),
            class = "fs-2 fw-bold text-primary mb-4",
            style = "min-height: 60px; display: flex; align-items: center; justify-content: center;"
          )
        )
      )
    ),
    
    # Input Summary Card
    card(
      card_header(
        class = "bg-secondary text-white",
        "📊 Input Summary"
      ),
      card_body(
        tableOutput("input_summary")
      )
    )
  ),
  
  # Footer info
  hr(),
  div(
    class = "text-muted small text-center",
    p("This app calls the Posit Cola revenue prediction API (deployed via vetiver) to predict revenue based on the input parameters."),
    p(strong("Note:"), "Make sure the CONNECT_API_KEY environment variable is set to use the API.")
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive value to store the text to be displayed for predicted revenue
  display_revenue_text <- reactiveVal("Click 'Predict Revenue' to see prediction")

  # Watch for input changes: reset the displayed text to the initial message
  observeEvent(list(input$units_sold, input$unit_price, input$product, input$region, input$store_type), {
    display_revenue_text("Click 'Predict Revenue' to see prediction")
  }, ignoreNULL = FALSE) # ignoreNULL = FALSE ensures it fires on initial load too

  # Handle predict button click
  observeEvent(input$predict, {
    # Immediately show "Predicting..." while the calculation is in progress
    display_revenue_text("Predicting...")

    # Perform the prediction call. This part is synchronous and will block
    # the UI until it completes. The "Predicting..." message will appear
    # briefly before the result if the API call is fast.
    result <- tryCatch({
      predict_revenue(
        units_sold = input$units_sold,
        unit_price = input$unit_price,
        product = input$product,
        region = input$region,
        store_type = input$store_type
      )
    }, error = function(e) {
      # Return an error message if the prediction fails
      return(paste("Error:", e$message))
    })

    # Update the displayed text with the final result or error message
    if (is.character(result) && startsWith(result, "Error:")) {
      display_revenue_text(result)
    } else {
      display_revenue_text(paste("$", round(as.numeric(result), 2)))
    }
  })

  # Display predicted revenue: simply output the value from the reactiveVal
  output$predicted_revenue <- renderText({
    display_revenue_text()
  })

  # Display input summary
  output$input_summary <- renderTable({
    current_text <- display_revenue_text()
    # Only show the input summary if a valid prediction has been made
    if (current_text == "Click 'Predict Revenue' to see prediction" ||
        current_text == "Predicting..." ||
        startsWith(current_text, "Error:")) {
      return(NULL) # Return NULL to hide the table
    } else {
      data.frame(
        Parameter = c("Units Sold", "Unit Price", "Product", "Region", "Store Type"),
        Value = c(
          input$units_sold,
          paste("$", input$unit_price),
          input$product,
          input$region,
          input$store_type
        )
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
