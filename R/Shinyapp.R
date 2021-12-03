#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(LinearRegressionNNY)

# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("General Linear Regression"),
  sidebarPanel(
    p("Select the inputs for the Dependent Variable"),
    selectInput(inputId = "DepVar", label = "Dependent Variables", multiple = FALSE, choices = list("Speed", "Distance")),
    p("Select the inputs for the Independent Variable"),
    selectInput(inputId = "IndVar", label = "Independent Variables", multiple = FALSE, choices = list("Speed", "Distance"))
  ),
  mainPanel(
    verbatimTextOutput(outputId = "RegSum"),
    verbatimTextOutput(outputId = "IndPrint"),
    verbatimTextOutput(outputId = "DepPrint"),
    plotOutput("distPlot")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  model <- reactive({linear_model(cars$dist, cars$speed, alpha = 0.05)})
  output$DepPrint <- renderPrint({input$DepVar})
  output$IndPrint <- renderPrint({input$IndVar})
  output$RegSum <- renderPrint({summary(model())})
  output$distPlot <- renderPlot({plot(input$DepVar~input$IndVar)}) #If I change the plot using the cars data set then it works, but when I set the plot to take from the input then it showed errors
}
?renderPlot
# Run the application
shinyApp(ui = ui, server = server)
