library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "df",
              label = "Choose degree of freedom",
              value = 0, min = 1, max = 7),
  headerPanel("Student's T distribution"),
  plotOutput('plot1')
  )

server <- function(input, output) {
  output$plot1 <- renderPlot({
    curve(dt(x, df = input$df), from = 0, to = 5,
          type = "b", col = "blue", ylab = "Density")
  })
  
}

shinyApp(ui = ui, server = server)

# 
# curve(dt(x, df = 1), from = 0, to = 5,
#       type = "b", col = "blue", ylab = "Density",
#       main = "T distribution shape = 2 rate = 4")
