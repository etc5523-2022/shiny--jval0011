library(tidyverse)
library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Is Your Favorite Airline Safe?"),



  # Sidebar with a slider input for ratings
  sidebarLayout(
    sidebarPanel(
      sliderInput("rate",
                  "Rating:",
                  min = 1,
                  max = 5,
                  value = 3)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      DT::dataTableOutput("rat")
    )))


server <- function(input, output, session) {

  rateTable <- reactive({
    req(input$rate)
    if(input$rate == 1){
      rateTable <- rat1
    } else if(input$rate == 2){
      rateTable <- rat2
    } else if(input$rate == 3){
      rateTable <- rat3
    } else if(input$rate == 4){
      rateTable <- rat4
    } else if(input$rate == 5){
      rateTable <- rat5
    }
  })

  output$rat <- DT::renderDataTable({
    DT::datatable(rateTable(), escape = FALSE)
    })


}

# Run the application
shinyApp(ui = ui, server = server)




