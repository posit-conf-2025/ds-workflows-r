library(shiny)
library(bslib)
library(querychat)
library(readr)

# Read data
cola_data <- read_csv("../data/posit_cola_sales_data_clean.csv")

# 1. Create a data source for querychat
cola_source <- querychat_data_source(cola_data)

# 2. Configure querychat with the data source
querychat_config <- querychat_init(cola_source)

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