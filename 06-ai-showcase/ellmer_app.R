library(shiny)
library(shinychat)
library(ellmer)

ui <- fluidPage(
  chat_ui("chatbot",
          placeholder = "Ask me anything about soda consumption!",
          height = "auto", 
          messages = "Hello! I'm your soda consumption assistant.")
        )

server <- function(input, output, session) {
  # Per-session conversation object using Anthropic's Claude
  chat <- chat_anthropic(system_prompt = "You are a helpful and concise sales assistant. 
  You're goal is to search the internet to figure out where people drink the most soda.
  The goal is to figure out where to sell a new soda brand called Posit Cola")
  
  observeEvent(input$chatbot_user_input, {
    # Stream response as user enters a message
    stream <- chat$stream_async(input$chatbot_user_input)
    chat_append("chatbot", stream)
  })
}

shinyApp(ui, server)