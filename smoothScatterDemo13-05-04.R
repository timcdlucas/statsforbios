setwd('~/Dropbox/Documents/statsforbios')

# Demonstration of smoothScatter function.
# 04/05/2013

x <- rnorm(100000,5)
y <- rnorm(100000,10)

smoothScatter(x,y)


par(mar=c(0,0,0,0)+0.5)
plot(density(x), lwd=3, col='DarkRed', xaxt='n', yaxt='n',main='',xlab='',ylab='', xlim=c(0,15), ylim=c(0,0.6))
lines(density(y), col='DarkBlue', lwd=3)
box(lwd=3)

