# Make a probability surface for banner. Pretty terrible coding througout.
# 2013-05-05

setwd('~/Dropbox/Documents/statsforbios')
library(MASS)

x <- rnorm(10^5,0,1.7)
y <-rnorm(10^5,0,1)

n=50
ncol=100
den3d <- kde2d(x, y, n=n)
den3d$z <- den3d$z*80

jet.colors <- colorRampPalette( c("white", 'dodgerblue4') )(ncol+1)

zfacet <- den3d$z[-1, -1] + den3d$z[-1, -n] + den3d$z[-n, -1] + den3d$z[-n, -n]


surfaceColors <- jet.colors[round(100*(zfacet/max(zfacet)))+1]


# 1252×626
png('banner.png',width=1252, heigh=626)
persp(den3d, box=FALSE, theta=20, phi=25, col=surfaceColors, scale=FALSE)
dev.off()




