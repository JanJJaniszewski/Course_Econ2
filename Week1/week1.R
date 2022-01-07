library(pacman)
p_load(tidyverse)
p_load(quantreg)



#### Question 3 ####
q3 <- read.csv('./Week1/assignment1b.csv')

y <- q3$lntotexp

n = length(y)
plot((1:n - 1)/(n - 1), sort(y), type="l",
       main = "Quantiles for the NYC Rain Data",
       xlab = "Sample Fraction",
       ylab = "Sample Quantile"
     )


abline(a=median(y), b=0)
abline(a=quantile(y, 0.9), b=0)
abline(a=quantile(y, 0.1), b=0)

taus <- seq(0.05, 0.95, 0.05)
rqs <- list()
k = 1
for (tau in taus){
  k = k + 1
  rqs[k] <- rq(lntotexp ~ ., data = q3, tau=tau)
}

rqs
