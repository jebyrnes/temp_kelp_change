library(rethinking)


ir_mod <- alist(
  Sepal.Length ~ dnorm(sl_mu, sigma_sl),
  
  sl_mu <- b1*Sepal.Width + b2*Petal.Length + b3[Species],
  
  sigma_sl ~ dcauchy(0,1),
  #a ~ dnorm(0,10),
  b1 ~ dnorm(0,10),
  b2 ~ dnorm(0,10),
  b3[Species] ~ dnorm(0,10)
)

lr_fit <- map2stan(ir_mod, data=iris)

summary(lr_fit)

summary(lm(Sepal.Length ~ Sepal.Width + Petal.Length + Species+0, data=iris))
