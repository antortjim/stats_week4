library("magrittr")
# Read data
neuron <- read.table("http://math.ku.dk/~tfb525/teaching/statbe/neuronspikes.txt",
                     col.names = "time")


# Expressions we want to minimize
mllk.gamma <- function(params, data) {
  -(dgamma(data, shape = params[1], rate = params[2]) %>% log %>% sum)
}

mllk.exponential <- function(rate, data) {
  -(dexp(data, rate) %>% log %>% sum)
}

# Optimize
exp.optim <- optimize(mllk.exponential, interval = c(-10, 10), data = neuron$time)
exp.optim <- exp.optim$minimum

gamma.optim <- optim(c(1, 1), mllk.gamma, data = neuron$time)

MLL1 <- mllk.exponential(exp.optim, neuron$time)
MLL2 <- mllk.gamma(gamma.optim$par, neuron$time)

statistic <- 2 * (MLL1 - MLL2)
p.value <- 1 - pchisq(statistic, df = 1)


ui <- fluidPage(
  #generate a slider input
  #store the number under num
  fluidRow( tags$b("Distribution parameters"), tags$br(), tags$br() ),
  
  fluidRow(
    column(4, tags$b("Exponential"), tags$br()),
    column(8, tags$b("Gamma"), tags$br())),
  
 
  fluidRow(column(4, 
                  sliderInput(inputId = "lambda",
                                  label = "Choose exponential rate",
                                  value = exp.optim, min = 0.01, max = 3)),
           
           
                      column(4, 
                             sliderInput(inputId = "shape",
                                 label = "Choose shape",
                                 value = gamma.optim$par[1], min = 0.01, max = 3)),
                      column(4, sliderInput(inputId = "rate",
                                 label = "Choose rate",
                                 value = gamma.optim$par[2], min = 0.01, max = 3))),

  #leave a display space and plot the hist object
  fluidRow(
    plotOutput("hist")
  ),
  
  fluidRow(
    column(12, tags$b("Gamma distribution fits way better our data")
  ))
)

server <- function(input, output) {
 
  
  #generate the hist object
 
    output$hist <- renderPlot (execOnResize = T, height = 400, width = 800, {
      library("ggplot2")
      length.out <-10001
      lower <- 0
      upper <- 4
      time.series <- seq(lower, upper, length.out = length.out)
     
      
      exp.data <- curve(dexp(x, input$lambda),
                         from = lower, to = upper, n = length.out)$y
      gamma.data <- curve(dgamma(x, input$shape, input$rate),
                          from = lower, to = upper, n = length.out)$y
      
      experimental.data <- density(x = neuron$time, from = lower, to = upper, n = length.out)$y
      
      
      distribution <- data.frame(time = rep(time.series, times = 3),
                                 id = factor(rep(c("exp", "gamma", "experimental"), each = length.out)),
                                 value = c(exp.data, gamma.data, experimental.data))
      
      p <- neuron %>% ggplot(aes(x = time, ..density..)) + geom_histogram(bins = 60, alpha = 0.2) +
        geom_line(data = distribution, aes(x = time, y = value,
                                                         color = id,
                                                         linetype = id == "experimental"),
                                size = 2) +
        scale_color_discrete(name="Distributions",
                         breaks=c("exp", "gamma", "experimental"),
                         labels=c("Exponential", "Gamma", "Density")) +
        ggtitle("Fitting exponential and gamma distributions to data") +
        scale_linetype_discrete(guide = FALSE) + 
        theme_bw() +
        theme(text = element_text(size = 20))
        print(p)
    })

    
}  


shinyApp(ui = ui, server = server)
