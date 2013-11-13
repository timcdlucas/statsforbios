## Jackknife a p value for a linear regression 
# 05/09/2013

# 140 char version
x<-1:25 
y<-x+rnorm(25,0,15) 
b<-ecdf(replicate(1000,lm(sample(y,13,F)~sample(x,13,F))$coef[2])) 
p<-1-b(lm(y~x)$coef[2])


# Fuller version
# Invent some data
x<-1:25 
y<-x+rnorm(25,0,15) 

# for each replicate, subsample the data without replacement. Then calculate slope parameter and put into epimirical CDF.
b<-ecdf(replicate(1000,lm(sample(y,13,F)~sample(x,13,F))$coef[2])) 

# What quantile is the original slope parameter in?
# This is a one tail test. p=2p for a two tail test.
p<-1-b(lm(y~x)$coef[2])


# Plot bootstrapped distribution

par(cex=1.5)
plot(density(replicate(1000,lm(y~sample(x,25,T))$coef[2])), xlim=c(-2,2), main='Jackknife distribution of regression slopes', lwd=3, xlab='Slope Parameter, n=1000')
abline(v=lm(y~x)$coef[2], col='Red', lwd=3)
box(lwd=3)


# Better do a cumulative distribution as I posted a link about that yesterday.

plot(b, main='CDF of jackknifed slope parameters', xlab='Slope. n=1000', ylab='Cumulative density', lwd=3)
abline(h=0.95,lty=2, lwd=2)
abline(h=b(lm(y~x)$coef[2]), col='Red', lwd=3)
text(-1.6,0.91,'95% significance')
text(-1.6,0.62,'Original slope estimate', col='Red')
box(lwd=2)

# Plot data. I wanted something that gives p>0.
plot(y~x)
