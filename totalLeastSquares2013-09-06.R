# Total least squares or Orthogonal regression
# 06/09/2013

library(rgr)

setwd('~/Dropbox/Documents/statsforbios')

x <-rnorm(50)
y <- x*2+rnorm(50)


linear <- lm(y~x)
orthogonal <- gx.rma(x,y)

plot(y~x)
abline(linear$coef, col='blue')
abline(orthogonal$a0, orthogonal$a1, col='red')
