library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("fristplot", height = 250)),
      box(
        title = "Controls", sliderInput("slider", "num of obv: ", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histogram <- rnorm(500)

  output$firstplot <- renderPlot({
    data <- histogram[seq_len(input$slider)]
    hist(data)
  }) 
}



shinyApp(ui, server)

