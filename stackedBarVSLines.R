
# Stacked bars vs lines
library(ggplot2)
library(reshape2)
library(RColorBrewer)


d <- data.frame(a = dpois(1:10, 4),
                b = dpois(3:12, 2),
                c = dpois(1:10, 3),
                d = dpois(3:12, 1),
                e = dpois(1:10, 0.3),
                f = dpois(3:12, 3.2),
                g = dexp(1:10, 0.1),
                h = dexp(1:10, 0.4),
                i = dexp(1:10, 0.5),
                j = dexp(1:10, 1),
                k = seq(0.01,0.2,length.out=10),
                l = seq(0.2,0.1, length.out=10),
                m = rep(0.01,10),
                n = rep(0.1,10),
                o = rep(0.001,10),
                p = rep(0.2,10))


pal <- c(brewer.pal(12, 'Set3'), brewer.pal(5, 'Paired'))


proportions <- apply(d, 1, function(x) x/sum(x))



png('~/Dropbox/Documents/statsforbios/stackedbarvslines.png', width=1000)
par(mfrow=c(1,2))

barplot(data.matrix(proportions), beside=FALSE, col=pal)


plot(proportions[1,] ~ c(1:10), col=pal[1], type='l', ylim=c(0, max(proportions)), lwd=2.3)
for(i in 2:16){
        lines(proportions[i,] ~ c(1:10), col=pal[i], lwd=2.3)
}
dev.off()


