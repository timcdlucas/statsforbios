
setwd('~/Dropbox/Documents/statsforbios')


# Trying to get bootstrapping into 140 characters
#
# 04/05/2013


x<-1:25;y<-x+rnorm(25,0,15) # Make data

# Replicate 1000 times, resampling x to break x~y relationship, and save slope coefficient each time.
# Then create empirical cumulative density object
b<-ecdf(replicate(1000,lm(y~sample(x,25,T))$coef[2])) 

# Which quantile of the bootstrap distribution is our real slope parameter in?
# P value is 1 minus quantile.
p<-1-b(lm(y~x)$coef[2])


# Plot bootstrapped distribution
png('bootstrapReg.png')
par(cex=1.5)
plot(density(replicate(1000,lm(y~sample(x,25,T))$coef[2])), xlim=c(-2,2), main='Bootstrap distribution of regression slopes', lwd=3, xlab='Slope Parameter, n=1000')
abline(v=lm(y~x)$coef[2], col='Red', lwd=3)
box(lwd=3)
dev.off()

# Plot data. I wanted something that gives p>0.
plot(y~x)
