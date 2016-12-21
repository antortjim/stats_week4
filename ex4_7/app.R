library("magrittr")
# Read data
neuron <- read.table("http://math.ku.dk/~tfb525/teaching/statbe/neuronspikes.txt",
                     col.names = "time")


# Expressions we want to minimize
mllk.gamma <- function(params, data) {
  -(dgamma(data, shape = params[1], rate = params[2], log = T) %>% sum)
}

mllk.exponential <- function(rate, data) {
  -(dexp(data, rate, log = T) %>% sum)
}

# Optimize and compute minus log likelihood
exponential.optim <- optimize(mllk.exponential, interval = c(-10, 10), data = neuron$time)
gamma.optim <- optim(c(1, 1), mllk.gamma, data = neuron$time)

# Compute statistic
statistic <- 2 * (exponential.optim$objective - gamma.optim$value)

# Compute p value
p.value <- 1 - pchisq(statistic, df = 1)


ui <- fluidPage(
  
  titlePanel("Which distribution fits best the neuron dataset?"),
  sidebarPanel(sliderInput(inputId = "lambda",
                        label = "Choose exponential rate",
                        value = exponential.optim$minimum, min = 0.01, max = 3),
            sliderInput(inputId = "shape",
                        label = "Choose shape",
                        value = gamma.optim$par[1], min = 0.01, max = 3),
            sliderInput(inputId = "rate",
                        label = "Choose rate",
                        value = gamma.optim$par[2], min = 0.01, max = 3),
            helpText("Parameters initialized at optimal values"),
            helpText("This is the best possible fit for both distributions, check on your own! :)")), # end of sidebarPanel

  
  mainPanel(
    plotOutput("hist")
    ), # end of mainPanel
  helpText("Gamma distribution fits way better our data")
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
