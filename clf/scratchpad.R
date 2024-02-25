test <- "rweibull"

match.fun(test)(10, 4)
stat_function(fun=match.fun(distrib()), args=argumenter, aes(fill = "orange"), geom  ="area") +
  




library(tidyverse)

distrib <- "dunif"
range <- case_when(distrib == "dnorm" ~ c(-3,3),
                   distrib == "dunif" ~ c(-3,3),
                   distrib == "dpois" ~ c(-3,3),
                   .default =  c(-3,3))

argumenter <- case_when(distrib == "dnorm" ~ list(0,1),
                   distrib == "dunif" ~ list(-3,3),
                   distrib == "dpois" ~ list(3),
                   .default =  list(-3,3))

ggplot(data.frame(x=range), aes(x)) + 
  stat_function(fun=match.fun(distrib), args=argumenter, aes(fill = "orange"), geom  ="area") +
  scale_fill_manual(values = "orange", guide  =F)

ggplot(data.frame(x=seq(0,10,by=1)), aes(x)) + 
  stat_function(fun=dpois, args=list(1), aes(fill = "orange"), geom  ="density") +
  scale_fill_manual(values = "orange", guide  =F)
dpois(.1,1)


norm = rnorm(n),
unif = runif(n),
lnorm = rlnorm(n),
exp = rexp(n),
rnorm(n)))

ggplot(data.frame(x=c(-10,10)), aes(x)) + 
  stat_function(fun=dwilcox,
                args=list(.5), aes(fill = "orange"), geom  ="density") +
  scale_fill_manual(values = "orange", guide  =F)



sample_size

library(SuppDists)

rcauchy(1)
switch()

SuppDists::