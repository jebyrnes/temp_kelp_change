library(rethinking)
make_data <- function(slope =1, int = 1, sd_e = 5, group=1, x=1:20){
  ret <- data.frame(x=x)
  ret <- within(ret, {
    y <- rnorm(length(x), int + slope*x, sd_e)
    group <- group
  })
  
  ret
}

set.seed(31415)
my_df <- rbind(make_data(),
               make_data(slope=5, sd_e=15, group=2),
               make_data(slope=3, sd_e=8, group=3))

ggplot2::qplot(x, y, color=group, data=my_df)

mod <- alist(
  #likelihood
  y ~ dnorm(y_loc, sd_loc), # likelihood
  
  #model
  y_loc <- a[group] + b[group]*x,
  sd_loc <- sd_e[group],
  
  #priors
  a[group] ~ dnorm(0,10),
  b[group] ~ dnorm(0,10),
  
  sd_e[group] ~ dcauchy(0,1) #prior for study level errors
)

fit <- map2stan(mod, data=my_df,
                constraints=list(sd_e="lower=0") , 
                start=list(sd_e=rep(1,3)))
