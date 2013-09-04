
setwd('~/Dropbox/Documents/statsforbios')

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


x<-1:25;y<-x+rnorm(25,0,15)

b<-ecdf(replicate(1000,lm(y~sample(x,25,T))$coef[2]))

p<-1-b(lm(y~x)$coef[2])



png('bootstrapReg.png')
par(cex=1.5)
plot(density(replicate(1000,lm(y~sample(x,25,T))$coef[2])), xlim=c(-2,2), main='Bootstrap distribution of regression slopes', lwd=3, xlab='Slope Parameter, n=1000')
abline(v=lm(y~x)$coef[2], col='Red', lwd=3)
box(lwd=3)
dev.off()

plot(y~x)
