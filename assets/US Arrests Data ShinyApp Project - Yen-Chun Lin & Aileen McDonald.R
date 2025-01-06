library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)

USArrests <- datasets::USArrests
USArrests$State <- rownames(USArrests)

ui_usarrests <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(
        HTML(
          "
          body {
            background-color: lavender;
          }
          "
        )
      )
    ),
    titlePanel("USArrests Data"),
    h4(p("Yen-Chun Lin & Aileen McDonald")),  # Comma removed here
    fluidRow(
      sidebarPanel(
        selectInput(ns("var1"), "Variable 1", choices = names(USArrests)),
        selectInput(ns("var2"), "Variable 2", choices = names(USArrests)),
        selectInput(ns("state"), "Select a State", choices = unique(USArrests$State))
      ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Plot1", plotOutput(ns("plot1"))),
          tabPanel("Plot2", plotOutput(ns("plot2"))),
          tabPanel("Table", DTOutput(ns("table"))),
          tabPanel("Summary", verbatimTextOutput(ns("summary"))),
          p("The USArrests dataset contains data on arrests per 100,000 residents for assult, murder, and rape in each of the 50 US states in 1973, as well as the percentage of the populations living in urban areas. The histogram uses the first variable, the scatterplot uses both the first and second variable, and the data table contains all of the variables. The scatterplot allows you to select a state and the specific data point will be displayed in red. The comparison tab allows you to use the sliders to look at how different levels of Urban populations and arrests for Murder contribute to one another.")
        )
      )
    )
  )
}

server_usarrests <- function(input, output, session) {
  output$plot1 <- renderPlot({
    ggplot(USArrests, aes_string(x = input$var1)) +
      geom_histogram(fill = "purple") +
      ggtitle("USArrests Histogram")
  })
  
  output$plot2 <- renderPlot({
    req(input$state, input$var1, input$var2)
    filtered_data <- USArrests[USArrests$State == input$state, ]
    
    p <- ggplot(USArrests, aes_string(x = input$var1, y = input$var2)) +
      geom_point(color = "green") +
      ggtitle("USArrests SCATTERPLOT")
    
    if (nrow(filtered_data) > 0) {
      p <- p +
        geom_point(data = filtered_data, aes_string(x = input$var1, y = input$var2), color = "red", size = 3)
    }
    
    p  
  })
  
  output$table <- renderDT({
    datatable(USArrests, options = list(pageLength = 10))
  })
  
  output$summary <- renderPrint({
    summary(USArrests[[input$var1]])
  })
}

ui_product <- function(id) {
  ns <- NS(id)
  fluidPage(
    sliderInput(ns("x"), "If x is Urban Pop", min = 32.00, max = 91.00, value = 40),
    sliderInput(ns("y"), "and y is Murder", min = 0.800, max = 17.400, value = 2),
    "then, (x * y) is", textOutput(ns("product"))
  )
}

server_product <- function(input, output, session) {
  output$product <- renderText({ 
    product <- input$x * input$y
    as.character(product)
  })
}

ui <- navbarPage(
  "Combined App",
  tabPanel("USArrests", ui_usarrests("usarrests")),
  tabPanel("Product Calculation", ui_product("product"))
)

server <- function(input, output, session) {
  callModule(server_usarrests, "usarrests")
  callModule(server_product, "product")
}

shinyApp(ui = ui, server = server)
