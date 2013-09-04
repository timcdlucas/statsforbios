
setwd('~/Dropbox/Documents/linear reg')

x <- rnorm(100000,5)
y <- rnorm(100000,10)

smoothScatter(x,y)


par(mar=c(0,0,0,0)+0.5)
plot(density(x), lwd=3, col='DarkRed', xaxt='n', yaxt='n',main='',xlab='',ylab='', xlim=c(0,15), ylim=c(0,0.6))
lines(density(y), col='DarkBlue', lwd=3)
box(lwd=3)




###############################################################################

plot( runif(10^3,max=seq(5,0.1, length.out=10^3)) )


png('quantileReg.png')
plot( runif(10^3,max=seq(5,0.1, length.out=10^3)), main='plot( runif(10^3,max=seq(5,0.1, length.out=10^3)) )', xlab='', ylab='', xaxt='n',yaxt='n' )
box(lwd=2)
dev.off()

##################################################################################


awdadwawd
2
