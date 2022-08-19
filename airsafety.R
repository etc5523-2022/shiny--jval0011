library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Is Your Favorite Airline Safe?"),



    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("aero1",
                        "Choose a Airline 1:",
                        unique(air$airline),
                        selected = "Aer Lingus"),
            selectInput("aero2",
                        "Choose a Airline 2:",
                        unique(air$airline),
                        selected = "Aer Lingus")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          splitLayout(
            plotOutput("incPlot1"),
            plotOutput("incPlot2")
            )

        )
    )
)


#use patchwork ex:p1+p2 and make if 1checked if 2 checked

server <- function(input, output) {

  output$incPlot1 <- renderPlot({
    air %>%
      filter(airline == input$aero1) %>%
      ggplot(aes(x = n_events,
                 y = year_range)) +
      geom_col(aes(fill = year_range)) +
      facet_wrap(~type_of_event, scales = "free", ncol=1)+
      labs(x = "Number of Events",
           y = "Range of Year") +
      scale_y_discrete(labels = c("85_99" = "1985 to 1999", "00_14" = "2000 to 2014")) +
      theme_bw() +
      theme(legend.position="none")

  })

  output$incPlot2 <- renderPlot({
    air %>%
      filter(airline == input$aero2) %>%
      ggplot(aes(x = n_events,
                 y = year_range)) +
      geom_col(aes(fill = year_range)) +
      facet_wrap(~type_of_event, scales = "free", ncol=1)+
      labs(x = "Number of Events",
           y = "Range of Year") +
      scale_y_discrete(labels = c("85_99" = "1985 to 1999", "00_14" = "2000 to 2014")) +
      theme_bw() +
      theme(legend.position="none")

  })

}

# Run the application
shinyApp(ui = ui, server = server)
