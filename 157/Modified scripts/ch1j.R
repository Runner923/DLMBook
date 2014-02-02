### R code for Chapter 1


bimodal <- function(x) log(prod(dnorm(x, mean = 3)) +
                           prod(dnorm(x, mean = -3)))
supp <- function(x) all(x > (-10)) * all(x < 10)
y <- arms( c(-2, 2), bimodal, supp, 500 )

plot(y, xlab="", ylab="", xlim=c(-6, 6), ylim=c(-6, 6))												# For the plotting the data (modified by Hagiwara)
