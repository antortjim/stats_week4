library("shiny")

lower <- -5
upper <- 20

ui <- fluidPage(
  titlePanel("Distributions of probability"),
  sidebarPanel(
    sliderInput(inputId = "range",
                label = "Select range",
                value = c(0, upper), min = lower, max = upper, step = 0.1),
    sliderInput("df",
                label = "Select degrees of freedom",
                value = 1, min = 1, max = 10),
    sliderInput("shape",
                label = "Select shape",
                value = 2, min = 0, max = 5, step = 0.1),
    sliderInput("rate",
                label = "Select rate",
                value = 2, min = 0, max = 5, step = 0.1),
    sliderInput("lambda",
                label = "Select lambda",
                value = 2, min = 0, max = 5, step = 0.1)
  ), #end sidebarPanel
  
  mainPanel(
    plotOutput('plot1'),
    checkboxGroupInput("active.distributions", "Show distributions:",
                       c("X²" = 1,
                         "Exponential" = 2,
                         "Gamma" = 3,
                         "Student's T" = 4), inline = TRUE)
      
  )  # end mainPanel
)


# Initalize parameters of the simulation and data structures

server <- function(input, output, session) {
 
  library("ggplot2")
  library("magrittr")
  theme_set(theme_bw(base_size = 20))
  
  length.out <- 501
  id <- c("chi", "exponential", "gamma", "student")
  
  simulateData <- function(input, output) {
    
    active.distributions <- 1:length(id) %in% (input$active.distributions %>% as.numeric)
    
    active.id <- id[active.distributions]
    
    x <- seq(input$range[1], input$range[2], length.out = length.out)
    
    chisq.distribution <- curve(dchisq(x, df = input$df), from = input$range[1], to = input$range[2], n = length.out)$y
    exp.distribution <- curve(dexp(x, rate = input$lambda), from = input$range[1], to = input$range[2], n = length.out)$y
    gamma.distribution <- curve(dgamma(x, shape = input$shape, rate = input$rate), from = input$range[1], to = input$range[2], n = length.out)$y
    t.distribution <- curve(dt(x, df = input$df), from = input$range[1], to = input$range[2], n = length.out)$y
    
    distributions <- list(chisq.distribution, exp.distribution, gamma.distribution, t.distribution)[active.distributions] %>% unlist
    
    data <- data.frame(X = rep(x, times = length(active.id)),
                 id = rep(active.id, each = length.out),
                 value = distributions)
      
    return(data)
  }
  
  data <- reactive({
    simulateData(input, output)
  })
  
  observe({
    output$plot1 <- renderPlot({
      if(data()$value %>% is.null) {
        p <- ggplot()
      } else {
        
        p <- data() %>% ggplot(aes(x = X, y = value, col = id, fill = id)) +
        geom_area(alpha = 0.5, position = 'identity') +
        geom_line(size = 1.5) +
        guides(col = F) 
      }
      p
    })
  })
  
 
}

shinyApp(ui = ui, server = server)