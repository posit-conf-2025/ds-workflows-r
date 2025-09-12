library(shiny)
library(bslib)
library(querychat)
library(readr)
library(purrr)
library(ellmer)

# Read data
# Make a client service to interact with S3
s3_client <- paws::s3()

# Bucket information
bucket_name <- "ptd-demo-bucket"
file_key <- "posit_cola_sales_data_validated.csv"

# Read the data from S3
response <- s3_client$get_object(
Bucket = bucket_name,
Key = file_key
)

cola_data <- read_csv(rawToChar(response$Body))

# 1. Create a data source for querychat
cola_source <- querychat_data_source(cola_data)

# 2. Configure querychat with the data source
querychat_config <- querychat_init(
  cola_source, 
  create_chat_func = purrr::partial(ellmer::chat_aws_bedrock, 
    model = "us.anthropic.claude-sonnet-4-20250514-v1:0"))

ui <- page_sidebar(
  sidebar = querychat_sidebar("chat"),
  DT::DTOutput("dt")
)

server <- function(input, output, session) {

  # 4. Create a querychat object using the config from step 2.
  querychat <- querychat_server("chat", querychat_config)

  output$dt <- DT::renderDT({
    # 5. Use the filtered/sorted data frame anywhere you wish, via the
    #    querychat$df() reactive.
    DT::datatable(querychat$df())
  })
}

shinyApp(ui, server)