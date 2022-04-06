library(shiny)

ui <- fluidPage(
  ## creating two action buttons
  actionButton(inputId = "norm", label = "Normal"),
  actionButton(inputId = "unif", label = "Uniform"),
  plotOutput("hist")
)

server <- function(input, output) {
  
  ## creating reactive value 
  rv <- reactiveValues(data = rnorm(100))
  
  ## if user clicks norm generate data of 100 normal data points 
  observeEvent(input$norm, { rv$data <- rnorm(100) })
  ## if user chooses uni generate 100 uni data points 
  observeEvent(input$unif, { rv$data <- runif(100) })
  
  ## plot histogram of whatever data was chosen
  output$hist <- renderPlot({ 
    hist(rv$data) 
  })
}

shinyApp(ui = ui, server = server)