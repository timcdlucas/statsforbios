library(pwr)

setwd('~/Dropbox/Documents/statsforbios')

effect <- seq(0.02, 0.6, by = 0.02)
n <- rep(0,length(effect))

for(i in 1:length(effect)){
n[i] <- (pwr.anova.test(2, sig.level=0.05, power=0.95, f=effect[i]))$n
}

png('powerAnalysis.png')
par(cex=1.4, mar=c(5,5,1,1))
plot(n~effect, type='l', log='y', xlab='Effect Size', lwd=3)
box(lwd=2)
dev.off()
