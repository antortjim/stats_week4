library("ggplot2")
# 4.5

# Initalize parameters of the simulation and data structures
n <- 20
mu <- 2
sigma <- 3
N <- 100
z.975 <- qnorm(0.975)
inferior <- numeric(N)
superior <- numeric(N)

# Simulate N samples of N(mu, sigma) and compute confidence interval of the mean of the sample
for(i in 1:N) {
  
  # Simulate data
  x <- rnorm(n, mu, sigma)
  
  # Compute mean value
  average <- mean(x)
  
  # Compute standard error of the mean
  sem <- sd(x)/sqrt(n)
  
  # Compute lower and upper bounds of the confidence interval
  inf <- average - z.975 * sem
  sup <- average + z.975 * sem
  
  # Store these values
  inferior[i] <- inf
  superior[i] <- sup
}

# Is the theoretical (actual) mean within the confidence interval?
result <- mu > inferior & mu < superior

# Plot results
# Generate data frame with data to feed to ggplot2 
df <- data.frame(x = 1:N,
                 F = rep(mu, N),
                 L = inferior,
                 U = superior,
                 R = result)

# Generate ggplot
p <- ggplot(df, aes(x = x, y = F)) +
  geom_hline(yintercept = mu) +
  geom_errorbar(aes(ymax = U, ymin = L)) +
  theme_bw()

p

# Add colors according to result
for(i in 1:N) {
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

p
