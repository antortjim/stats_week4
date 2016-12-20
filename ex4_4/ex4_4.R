library("ggfortify")
library("magrittr")

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#ex 4.4
#4.4.1
x <- rnorm(20, 2, 4)
y <- rnorm(40, 2.5, 4)
x.mean <- mean(x)
y.mean <- mean(y)
xy <- c(x, y)

# Plotting data
p <- ggdistribution(dnorm, x = seq(-10, 14, 0.1), mean = 2, sd = 4, fill = "blue", alpha = 0.2) +
  geom_vline(xintercept = 2, linetype = "longdash") +
  scale_x_continuous(breaks = seq(-10, 14, 2))

p <- ggdistribution(dnorm, x = seq(-10, 14, 0.1), p = p, mean = 2.5, sd = 4, fill = "red", alpha = 0.2) +
  geom_vline(xintercept = 2.5, linetype = "longdash") +
  ggtitle(label = "Theoretical distributions")

q <- autoplot(density(x), fill = "blue")
q <- autoplot(density(y), p = q, fill = "red") +
  ggtitle("Experimental data") +
  scale_x_continuous(breaks = seq(-10, 14, 2))

q

multiplot(p, q, cols = 2)



mllk.normal <- function(params, data) {
  sum(-log(dnorm(data, mean = params, sd = 4)))
}


# 4.4.2 Perform the maximum likelihood estimation for the full model and the nested model, and compute
#the maximized likelihood values (or the minimized minus-log-likelihood values)

# optimize parameters for the full and nested models
x.params <- optimize(mllk.normal, interval = c(-10, 10), data = x)
y.params <- optimize(mllk.normal, interval = c(-10, 10), data = y)
xy.params <- optimize(mllk.normal, interval = c(-10, 10), data = xy)

# compute likelihood of full model using optimized parameters
full.model.l <- c(dnorm(x, mean = x.params$minimum, sd = 4),
                    dnorm(y, mean = y.params$minimum, sd = 4)) %>% prod


# compute likelihood of nested model using optimized parameters
nested.model.l <- dnorm(xy, mean = xy.params$minimum, sd = 4) %>% prod


#4.4.3. Compute the likelihood ratio test statistic and the p-value.
# compute q, the q of the likelihood of both models
q <- nested.model.l / full.model.l

# likelihood ratio test
#-2log(q) ~ chi squared with d - d0 degrees of freedom
# dimensions of the full model (3)
# dimensions of the nested model (2)
# d - d0 = 1
lrt.pval <- 1 - pchisq(-2*log(q), df = 1)
# the null hypothesis is likely enough (there's not enought evidence that it is false)
# we can't reject the null hypothesis -> the nested model is sufficient
# (there's no evidence that the means are different)

#4.4.4. Perform a two-sample t-test for x and y with equal variance. Do you get the same p-value?
# students t test
ttest.pval <- t.test(x, y, var.equal = TRUE, alternative = "two.sided")$p.value

ggplot(data = data.frame(test = as.factor(c("Likelihood ratio", "Student's T")), p_value = c(lrt.pval, ttest.pval))) +
  geom_bar(aes(x = test, y = p_value, fill=test), stat = "identity") +
  scale_y_continuous(limits = 0:1)


# p <- ggdistribution(dchisq, x = seq(0.005, 3, 0.005), df = 1, fill = "red") + 
#   geom_vline(xintercept = statistic)
# p
