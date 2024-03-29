# Comparison between wilcox and ANOVA tests
# 06-09-2013

setwd('~/Dropbox/Documents/statsforbios')

# To compare anova and wilcox test, I'll draw numbers from normal distributions and apply the test. Do this for different sample sizes and replicate.
# What sample sizes and how many replicates of each.
nvec <- c(3:14,seq(15,100,by=5))
reps <- 500

# Empty matrices to fill
pAnova <- matrix(0,nrow=length(nvec),ncol=reps)
pWilcox <- matrix(0,nrow=length(nvec),ncol=reps)


# Run tests for each sample size (this loop is slow. I should rewrite it.)
for(i in 1:length(nvec)){

n <- nvec[i]

# Replicate tests with different random numbers.
for(r in 1:reps){
d <- cbind(c(rnorm(n,mean=1,sd=1),rnorm(n,mean=1.5,sd=1)), c(rep(1,n),rep(2,n)))

pAnova[i,r] <- (anova(lm(d[,1]~d[,2])))$Pr[1]
pWilcox[i,r] <- (wilcox.test(d[,1]~d[,2]))$p.value

}


}

#plot

png('anovaVsWilcox-sampleSize.png')
par(cex=1.3)
plot(apply(pAnova,1,median)~nvec,type='l', col='Blue', xlab='Sample size', ylab='p value', lwd=3, main='rnorms with mean=1 and 1.5 and sd=1')
lines(apply(pWilcox,1,median)~nvec,type='l', col='Red', lwd=3)
lines(apply(pAnova, 1, function(x) quantile(x, 0.75))~nvec, lty=2, lwd=2, col='Blue')
lines(apply(pAnova, 1, function(x) quantile(x, 0.25))~nvec, lty=2, lwd=2, col='Blue')
lines(apply(pWilcox, 1, function(x) quantile(x, 0.75))~nvec, lty=2, lwd=2, col='Red')
lines(apply(pWilcox, 1, function(x) quantile(x, 0.25))~nvec, lty=2, lwd=2, col='Red')
legend('topright', inset=0.01,fill=c('Red','Blue'),legend=c('Anova', 'Wilcox'), box.lwd=2)
box(lwd=2)
dev.off()




# To compare anova and wilcox test, I'll draw numbers from normal distributions and apply the test. Do this for different standard deviations and replicate.
# What sample sizes and how many replicates of each.
sdvec <- c(seq(0.02,0.98, by=0.02), seq(1,5,by=0.2))
reps <- 500

# Empty matrices to fill
pAnova <- matrix(0,nrow=length(sdvec),ncol=reps)
pWilcox <- matrix(0,nrow=length(sdvec),ncol=reps)


# Run tests for each sample size (this loop is slow. I should rewrite it.)
for(i in 1:length(sdvec)){

sd <- sdvec[i]

# Replicate tests with different random numbers.
for(r in 1:reps){
d <- cbind(c(rnorm(30,mean=1,sd=sd),rnorm(30,mean=1.5,sd=sd)), c(rep(1,30),rep(2,30)))

pAnova[i,r] <- (anova(lm(d[,1]~d[,2])))$Pr[1]
pWilcox[i,r] <- (wilcox.test(d[,1]~d[,2]))$p.value

}
}

#plot
	
png('anovaVsWilcox-sd.png')
par(cex=1.3)
plot(apply(pAnova,1,median)~sdvec,type='l', col='Blue', xlab='Standard Deviation', ylab='p value', lwd=3, main='rnorms with mean=1 and 1.5 and n=30')
lines(apply(pWilcox,1,median)~sdvec,type='l', col='Red', lwd=3)
lines(apply(pAnova, 1, function(x) quantile(x, 0.75))~sdvec, lty=2, lwd=2, col='Blue')
lines(apply(pAnova, 1, function(x) quantile(x, 0.25))~sdvec, lty=2, lwd=2, col='Blue')
lines(apply(pWilcox, 1, function(x) quantile(x, 0.75))~sdvec, lty=2, lwd=2, col='Red')
lines(apply(pWilcox, 1, function(x) quantile(x, 0.25))~sdvec, lty=2, lwd=2, col='Red')
legend('bottomright', inset=0.01,fill=c('Red','Blue'),legend=c('Anova', 'Wilcox'), box.lwd=2)
box(lwd=2)
dev.off()






# To compare anova and wilcox test, I'll draw numbers from normal distributions and apply the test. Do this for different effect sizes and replicate.
# What sample sizes and how many replicates of each.
effectvec <- seq(0.1,5,by=0.1)
reps <- 500

# Empty matrices to fill
pAnova <- matrix(0,nrow=length(effectvec),ncol=reps)
pWilcox <- matrix(0,nrow=length(effectvec),ncol=reps)


# Run tests for each sample size (this loop is slow. I should rewrite it.)
for(i in 1:length(effectvec)){

# Replicate tests with different random numbers.
for(r in 1:reps){
d <- cbind(c(rnorm(30,mean=1,sd=1),rnorm(30,mean=1+effectvec[i],sd=1)), c(rep(1,30),rep(2,30)))

pAnova[i,r] <- (anova(lm(d[,1]~d[,2])))$Pr[1]
pWilcox[i,r] <- (wilcox.test(d[,1]~d[,2]))$p.value

}
}

#plot
	
png('anovaVsWilcox-effect.png')
par(cex=1.3)
plot(apply(pAnova,1,median)~effectvec,type='l', col='Blue', xlab='Difference between means', ylab='p value', lwd=3, main='rnorms with sd=2 and n=30')
lines(apply(pWilcox,1,median)~effectvec,type='l', col='Red', lwd=3)
lines(apply(pAnova, 1, function(x) quantile(x, 0.75))~effectvec, lty=2, lwd=2, col='Blue')
lines(apply(pAnova, 1, function(x) quantile(x, 0.25))~effectvec, lty=2, lwd=2, col='Blue')
lines(apply(pWilcox, 1, function(x) quantile(x, 0.75))~effectvec, lty=2, lwd=2, col='Red')
lines(apply(pWilcox, 1, function(x) quantile(x, 0.25))~effectvec, lty=2, lwd=2, col='Red')
legend('topright', inset=0.01,fill=c('Red','Blue'),legend=c('Anova', 'Wilcox'), box.lwd=2)
box(lwd=2)
dev.off()




