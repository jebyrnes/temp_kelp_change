library(rethinking)
library(dplyr)
set.seed(2002)

adf <- data.frame(x1=runif(100,0,10),
                  x2 = runif(100,0,10)) %>%
  mutate(y=rnorm(100,x1*2+x2*3+3, 5))


precis(lm(y~x1+x2, data=adf))

#here's a straight linear regression
#in rethinking syntax - nice, no?
lm_mod <- alist(
  
  #likelihood
  y ~ dnorm(yhat, sd_y),
  
  #link function
  yhat <- a + b[1]*x1 + b[2]*x2,
  
  #priors
  a ~ dnorm(0,10),
  b ~ dnorm(0,10),
  sd_y ~ dunif(0,20)
  
)

lm_fit <- map2stan(lm_mod, data=adf,
                   start=list(b=c(1,1)),
                   adapt_delta=0.95)

precis(lm(y~x1+x2, data=adf))
precis(lm_fit, depth=2)

