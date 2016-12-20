library("ggplot2")
library("ggvis")
library("ggfortify")

alpha <- 1 - pnorm(q = 2, mean = 0, sd = 1)
beta <- 1 - pnorm(2, mean = 2, sd = 1)

little.difference <- function(set1, set2) {
  difference <- abs(set1 - set2) 
  pos <- which.min(difference)
  return(pos)
}


lower <- -5
upper <- +5
length.out <- 1001

tau <- seq(from = lower, to = upper, length.out = length.out)
alpha <- curve( {1 - pnorm(x, mean = 0, sd = 1)},
                from = lower, to = upper, n = length.out)$y

beta <- curve( {pnorm(x, mean = 2, sd = 1)},
               from = lower, to = upper, n = length.out)$y

power <- 1 - beta


data <- data.frame(
  tau = tau,
  name = as.factor(rep(c("alpha", "beta", "power"), each = length.out)),
  value = c(alpha, beta, power)
)


ggplot(data = data) + geom_line(aes(x = tau, y = value, colour = name, linetype = name == "power"), size = 2) +
  scale_x_continuous(name = "τ",
                     breaks = seq(lower, upper, 1)) +
  ggtitle("α, β, and power as a function of τ") +
  scale_linetype_discrete(guide = FALSE) +
  scale_color_manual(name = "Parameters",
                     values = c("red", "green", "blue"),
                     breaks = c("alpha", "beta", "power"),
                     labels = c("α", "β", "Power")) +
  scale_y_continuous(name = "P") + 
  theme_bw()


data$id <- 1:nrow(data)


base <- data %>%
  ggvis(~tau, ~value, stroke = ~name, key := ~id) %>%
  #layer_lines(strokeWidth := 5) %>%
  layer_points(fill = ~name)
base %>% add_tooltip(function(data){paste0(data$name, "<br />Value: ", round(data$value, digits = 2), "<br />Tau:", data$tau)}, "hover")
  

p <- ggdistribution(dnorm, tau, mean = 0, sd = 1, fill = "red")
ggdistribution(dnorm, tau, mean = 2, sd = 1, p = p, fill = "blue") + 
  scale_x_continuous(breaks = seq(lower, upper, 1)) +
  theme_bw()

#showing alpha
p <- ggdistribution(dnorm, tau[tau > 2], mean = 0, sd = 1, fill = "gray9", col = "white")
ggdistribution(dnorm, tau, mean = 0, sd = 1, p = p) +
  scale_x_continuous(breaks = seq(lower, upper, 1)) +
  theme_bw()


#showing beta
p <- ggdistribution(dnorm, tau[tau <= 2], mean = 2, sd = 1, fill = "gray9", col = "white")
ggdistribution(dnorm, tau, mean = 2, sd = 1, p = p) +
  scale_x_continuous(breaks = seq(lower, upper, 1)) +
  theme_bw()
