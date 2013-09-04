
setwd('~/Dropbox/Documents/statsforbios')

# Example dataset for when quantile regression is useful.
# 04/05/2013

plot( runif(10^3,max=seq(5,0.1, length.out=10^3)) )


png('quantileReg.png')
plot( runif(10^3,max=seq(5,0.1, length.out=10^3)), main='plot( runif(10^3,max=seq(5,0.1, length.out=10^3)) )', xlab='', ylab='', xaxt='n',yaxt='n' )
box(lwd=2)
dev.off()


