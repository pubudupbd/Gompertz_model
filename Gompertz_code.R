# I am testing this code for my model
fit.gompertz <- function(data, time){
  d <- data.frame(y=data, t=time)
  
  # Must have at least 3 datapoints at different times
  if (length(unique(d$t)) < 3) stop("too few data points to fit curve")
  
  # Pick starting values ###
  i <- which.max(diff(d$y))
  starting.values <- c(a=max(d$y), 
                       mu=max(diff(d$y))/(d[i+1,"t"]-d[i, "t"]), 
                       lambda=i)
  print("Starting Values for Optimization: ")
  print(starting.values)
  ##########################
  
  formula.gompertz <- "y~a*exp(-exp(mu*exp(1)/a*(lambda-t)+1))"
  nls(formula.gompertz, d, starting.values)
}

gompertz <- function(time, a, mu, lambda){
  y <- a*exp(-exp(mu*exp(1)/a*(lambda-time)+1))
  return(data.frame(time=time, y=y))
}

d <- gompertz(1:100, 10, 2, 30)
plot(d)

plot(d, ylab="microbial abundance")
lines(d$time, predict(fit))

# Add some normal(0,0.5) noise centered around the deterministic signal
for(i in 1:nrow(d)) d[i,2] <- rnorm(1, d[i,2], 1)

(fit <- fit.gompertz(d$y, d$time))


safe.fit.gompertz <- safely(fit.gompertz)

