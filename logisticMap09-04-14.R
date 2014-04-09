# Very slow running logistic map

h=rep(0.5,200)
x=c()
for(r in seq(0,4,0.1)){
	for(n in 2:200){
		h[n]=r*h[n-1]*(1-h[n-1])
		x = rbind(x, cbind(r, h[190:200]))
	}
}
plot(x)


png('/home/tim/Dropbox/Documents/statsforbios/logisticMap.png')
plot(x)
dev.off()

