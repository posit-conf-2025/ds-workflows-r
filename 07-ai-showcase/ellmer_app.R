library(shiny)
library(shinychat)
library(httr2)
library(ellmer)
library(paws)

ui <- fluidPage(
  chat_ui("chatbot",
          placeholder = "Ask me anything about soda consumption!",
          height = "auto", 
          messages = "Hello! I'm your soda consumption assistant.")
        )

server <- function(input, output, session) {
  # Per-session conversation object using Anthropic's Claude
  chat <- chat_aws_bedrock(
    system_prompt = "You are a helpful and concise sales assistant. 
  You're goal is to search the internet to figure out where people drink the most soda.
  The goal is to figure out where to sell a new soda brand called Posit Cola",
    model = "us.anthropic.claude-sonnet-4-20250514-v1:0")
  
  observeEvent(input$chatbot_user_input, {
    # Stream response as user enters a message
    stream <- chat$stream_async(input$chatbot_user_input)
    chat_append("chatbot", stream)
  })
}

shinyApp(ui, server)