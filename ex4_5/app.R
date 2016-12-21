library("shiny")
library("ggplot2")

ui <- fluidPage(
  titlePanel("Exploring confidence intervals"),
  sidebarPanel(
    numericInput(inputId = "N", 
                 label = "Choose number of simulations (N)",
                 value = 25, min = 25, max = 1000, step = 1),
    numericInput(inputId = "n",
                 label = "Choose sample size (n)",
                 value = 20, min = 20, max = 100),
    helpText("Simulate N samples of size n and plot confidence intervals if N is up to 100"),
    #actionButton(inputId = "go", label = "Plot"),
sliderInput(inputId = "p",
            label = "Select probability of the random interval covering μ",
            value = 0.95, min = 0.5, max = 0.99),
    helpText("Modifies the CI so that μ is covered with the selected probability")
  ), #end sidebarPanel
  
  mainPanel(
  plotOutput('plot1'),
  tableOutput('summary')
  ) # end mainPanel
)

# Initalize parameters of the simulation and data structures

server <- function(input, output, session) {
  
      mu <- 2
      sigma <- 3
      simulateData <- function(input, output) {
        # Reactive expression that should not become invalid
        # if input$alpha changes

        sem.realiz <- numeric(length = input$N)
        mean.realiz <- numeric(length = input$N)
        
        # Simulate N samples of N(mu, sigma) and compute confidence interval of the mean of the sample
        for(i in 1:input$N) {
          # Simulate data
          x <- rnorm(input$n, mu, sigma)
          
          # Compute mean value
          average <- mean(x)
          mean.realiz[i] <- average
          
          # Compute standard error of the mean
          sem <- sd(x)/sqrt(input$n)
          sem.realiz[i] <- sem
          
        }
        mean.realiz <- mean.realiz
        sem.realiz <- sem.realiz
        return(list(mean.realiz, sem.realiz))
      }
      
      generateDataFrame <- function(input, output, realizations)  {
        
        z <- qt(input$p + (1 - input$p) / 2, df = input$n - 1 )
        inferior <- realizations[[1]] - z * realizations[[2]]
        superior <- realizations[[1]] + z * realizations[[2]]
        
        # Is the theoretical (actual) mean within the confidence interval?
        result <- mu > inferior & mu < superior
        
        # Plot results
        # Generate data frame with data to feed to ggplot2 
        df <- data.frame(X = 1:input$N,
                         MU = rep(x = mu, times = input$N),
                         m.real = realizations[[1]],
                         L = inferior,
                         U = superior,
                         R = result)

        z <- round(z, digits = 3)
        return(list(df, z = z))
      }
      
      
      computeStatistics <- function(df) {
        fraction <- sum(df$R) / length(df$R)
        fraction <- fraction * 100
        round(fraction, digits = 4)
        return(fraction)
      }     
      
      realizations <- reactive({
       simulateData(input, output)
      })
      
      
      df <- reactive({
       generateDataFrame(input, output, realizations())[[1]]
      })
      
      z <- reactive({
        generateDataFrame(input, output, realizations())[[2]]
      })
     
      fraction <- reactive({
        computeStatistics(df())
      })
      
        output$summary <- renderTable({
        df <- data.frame(fraction(), z())
        names(df) <- c("Fraction of successful intervals", "Selected percentil in t")
        df
      }, include.rownames = FALSE)
      

      
      observe({
            if(input$N <= 100) {
            output$plot1 <- renderPlot({
              # Generate ggplot
              result <- df()[["R"]]
              p <- ggplot(df(), aes(x = X, y = MU)) +
              geom_hline(yintercept = mu) +
              geom_errorbar(aes(ymax = U, ymin = L)) +
              geom_point(aes(y = m.real), size = 2) +
              scale_y_continuous(name = "X", limits = c(mu - 5, mu + 5)) +
              scale_x_continuous(name = "Simulations") +
              ggtitle("Confidence intervals at α selected by user")
              # Add colors according to result
              for(i in 1:input$N) {
                left <- i - 0.5
                right <- i + 0.5
                if(result[i]) {
                  # If the mean is contained in the CI, give green background
                  p <- p + geom_rect(aes(ymin= -Inf, ymax = Inf), fill = "green", xmin = left, xmax = right, alpha = 0.01)
                } else {
                  # Else, give red background
                  p <- p + geom_rect(aes(ymin= -Inf, ymax = Inf), fill = "red", xmin = left, xmax = right, alpha = 0.01)
                }
              }
              p <- p + theme_bw()
              p
            
              })
            } else {
              output$plot1 <- renderPlot({
                ggplot()
              })
            }
        })
}

shinyApp(ui = ui, server = server)

