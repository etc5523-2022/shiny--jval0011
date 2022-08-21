library(tidyverse)
library(shiny)
library(plotly)
library(DT)
library(bslib)

# Define UI for application
ui <- fluidPage(

  theme = bs_theme(version = 4, bootswatch = "minty"),

  titlePanel("Is Your Favorite Airline✈️ Safe?"),

  tabsetPanel(
    tabPanel("About the App",
              div(class = "about",
                  uiOutput('about'))
    ),
    tabPanel("Compare Mode",

  sidebarLayout(
    sidebarPanel(
      selectInput("aero1",
                  "Choose a Airline 1:",
                  unique(air_pic$airline),
                  selected = "Aer Lingus"),
      selectInput("aero2",
                  "Choose a Airline 2:",
                  unique(air_pic$airline),
                  selected = "Aeroflot"),
      radioButtons("event1",
                   "Types of Event:",
                   c("Incidents" = "incidents",
                     "Fatal Accidents" = "fatal_accidents",
                     "Fatalities" = "fatalities"),
                   selected = "incidents")
    ),

    mainPanel(
      splitLayout(
        verticalLayout(
          fluidRow(
            column(8,
                   br(),
                   h1(textOutput("aeroname1"))),
            column(4, uiOutput("aeropic1"))
          ),
          plotlyOutput("incPlot1"),
          br(),
          br(),
          div( class = "rating",
          h3("Rating"),
          textOutput("aerorate1"))),
        verticalLayout(
          fluidRow(
            column(8,
                   br(),
                   h1(textOutput("aeroname2"))),
            column(4, uiOutput("aeropic2"))
          ),
          plotlyOutput("incPlot2"),
          br(),
          br(),
          div(
            class = "rating",
            h3("Rating"),
          textOutput("aerorate2")))
      )

    )
  )),
  tabPanel("Rate Mode",
    sidebarLayout(
      sidebarPanel(
        sliderInput("rate",
                    "Rating:",
                    min = 1,
                    max = 5,
                    value = 3)
      ),

      mainPanel(
        DT::dataTableOutput("rat")
      ))
  )),
  includeCSS("styles.css")
)


server <- function(input, output, session) {

  observe({
    updateSelectInput(session, "aero2", choices = setdiff(unique(air$airline), input$aero1))
  })

  output$aeroname1 <- reactive({input$aero1})
  output$aeroname2 <- reactive({input$aero2})

  output$aeropic1 <- renderUI({
    pic1 <- air_pic %>%
      filter(airline == input$aero1)
    tags$img(src = pic1$logo, height="70%", width="70%")
  })

  output$aeropic2 <- renderUI({
    pic2 <- air_pic %>%
      filter(airline == input$aero2)
    tags$img(src = pic2$logo, height="70%", width="70%")
  })

  y_label <- reactive({
    req(input$event1)
    if(input$event1 == "incidents"){
      y_label <- "Number of Incidents"
    } else if(input$event1 == "fatal_accidents"){
      y_label <- "Number of Fatal Accidents"
    } else if(input$event1 == "fatalities"){
      y_label <- "Number of Fatalities"
    }})


  output$incPlot1 <- renderPlotly({
    p1 <- air %>%
      filter(airline == input$aero1, type_of_event == input$event1) %>%
      ggplot(aes(y = n_events,
                 x = year_range)) +
      geom_col(aes(fill = year_range)) +
      labs(y = y_label(),
           x = "Range of Year") +
      scale_x_discrete(labels = c("85_99" = "1985 to 1999", "00_14" = "2000 to 2014")) +
      theme_bw() +
      theme(legend.position="none")
    ggplotly(p1)
  })

  output$incPlot2 <- renderPlotly({
    p2 <- air %>%
      filter(airline == input$aero2, type_of_event == input$event1) %>%
      ggplot(aes(y = n_events,
                 x = year_range)) +
      geom_col(aes(fill = year_range)) +
      labs(y = y_label(),
           x = "Range of Year") +
      scale_x_discrete(labels = c("85_99" = "1985 to 1999", "00_14" = "2000 to 2014")) +
      theme_bw() +
      theme(legend.position="none")
    ggplotly(p2)
  })

  output$aerorate1 <- reactive({
    stars1 <- air_pic %>%
      filter(airline == input$aero1)
    stars1$stars
  })

  output$aerorate2 <- reactive({
    stars2 <- air_pic %>%
      filter(airline == input$aero2)
    stars2$stars
  })

  rateTable <- reactive({
    req(input$rate)
    if(input$rate == 1){
      rateTable <- air_pic %>%
        filter(rating == 1) %>%
        select(airline, pictags) %>%
        tibble()
    } else if(input$rate == 2){
      rateTable <- air_pic %>%
        filter(rating == 2) %>%
        select(airline, pictags) %>%
        tibble()
    } else if(input$rate == 3){
      rateTable <- air_pic %>%
        filter(rating == 3) %>%
        select(airline, pictags) %>%
        tibble()
    } else if(input$rate == 4){
      rateTable <- air_pic %>%
        filter(rating == 4) %>%
        select(airline, pictags) %>%
        tibble()
    } else if(input$rate == 5){
      rateTable <-  air_pic %>%
        filter(rating == 5) %>%
        select(airline, pictags) %>%
        tibble()
    }
  })

  output$rat <- DT::renderDataTable({
    DT::datatable(rateTable(),
                  colnames = c("Airline", "Logo"),
                  escape = FALSE)
  })

  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })

}

shinyApp(ui = ui, server = server)
