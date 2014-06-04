# Plots for explaining GLM
library('RColorBrewer')
library('tikzDevice')

plot(1:10,1:10, col='white')
text(runif(1,0,10),runif(1,0,10),expression(paste(frac(1,paste(sigma, sqrt(paste(2, pi)))),~e^-~frac((x~-~mu)^2,paste(2, sigma^2)))), cex=1.3)




r <- brewer.pal( 8, 'Set1')[1]
b <- brewer.pal( 8, 'Set1')[2]
g <- brewer.pal( 8, 'Set1')[3]
br <- brewer.pal( 8, 'Set1')[7]
arrowWidth=8

tikz("~/Dropbox/Documents/statsforbios/glm/normalPDF.tex", width = 6, height = 6, 
standAlone = TRUE,
packages = c("\\usepackage{tikz}",
"\\usepackage[active,tightpage,psfixbb]{preview}",
"\\PreviewEnvironment{pgfpicture}",
"\\setlength\\PreviewBorder{0pt}",
"\\usepackage{amssymb}",
"\\usepackage[scaled]{helvet}",
"\\renewcommand*\\familydefault{\\sfdefault}", 
"\\usepackage[T1]{fontenc}",
"\\usepackage{textgreek}"))



par(mar=c(3,4,0,0)+0.3, cex=1.8)
plot(dnorm(seq(-3,5,length.out=1000,), 0, 1 )~ seq(-3,5,length.out=1000,), type='l', col = r, lwd=11, ylab='Density', xlab='',xaxs='i', yaxs='i', ylim=c(0,0.45), xlim=c(-3,6.2))
text(3.6, 0.3, '$\\frac{1}{\\sigma \\sqrt{2\\pi}} e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$', cex=1.8)



dev.off()





tikz("~/Dropbox/Documents/statsforbios/glm/normalPDF2.tex", width = 6, height = 6, 
standAlone = TRUE,
packages = c("\\usepackage{tikz}",
"\\usepackage[active,tightpage,psfixbb]{preview}",
"\\PreviewEnvironment{pgfpicture}",
"\\setlength\\PreviewBorder{0pt}",
"\\usepackage{amssymb}",
"\\usepackage[scaled]{helvet}",
"\\renewcommand*\\familydefault{\\sfdefault}", 
"\\usepackage[T1]{fontenc}",
"\\usepackage{textgreek}"))



par(mar=c(3,4,0,0)+0.3, cex=1.8)
plot(dnorm(seq(-3,5,length.out=1000,), 0, 1 )~ seq(-3,5,length.out=1000,), type='l', col = r, lwd=11, ylab='Density', xlab='',xaxs='i', yaxs='i', ylim=c(0,0.45), xlim=c(-3,6.2))
text(3.6, 0.3, '$\\frac{1}{\\sigma \\sqrt{2\\pi}} e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$', cex=1.8)
points(-1,0.004, cex=2, col=b, pch=16)
arrows(-1,0.004, -1, dnorm(-1), col=b, lwd=arrowWidth)
arrows(-1, dnorm(-1), -2.9, dnorm(-1), col=b, lwd=arrowWidth)


dev.off()




tikz("~/Dropbox/Documents/statsforbios/glm/likelihoodNorm.tex", width = 6, height = 6, 
standAlone = TRUE,
packages = c("\\usepackage{tikz}",
"\\usepackage[active,tightpage,psfixbb]{preview}",
"\\PreviewEnvironment{pgfpicture}",
"\\setlength\\PreviewBorder{0pt}",
"\\usepackage{amssymb}",
"\\usepackage[scaled]{helvet}",
"\\renewcommand*\\familydefault{\\sfdefault}", 
"\\usepackage[T1]{fontenc}",
"\\usepackage{textgreek}"))



par(mar=c(4,4,0,0)+0.3, cex=1.8)
plot(dnorm(seq(-3,5,length.out=1000,), -1, 1 )~ seq(-3,5,length.out=1000,), type='l', col = g, lwd=11, ylab='Likelihood', xlab='\\textmu',xaxs='i', yaxs='i', ylim=c(0,0.45), xlim=c(-3,6.2))
text(3, 0.38, '$\\mathcal{L}=\\frac{1}{\\sigma \\sqrt{2\\pi}} e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$', cex=1.6)
points(-1,0.004, cex=2, col=b, pch=16)
arrows(-1,0.004, -1, dnorm(-1), col=b, lwd=arrowWidth)
#arrows(-1, dnorm(-1), -2.9, dnorm(-1), col=b, lwd=arrowWidth)


dev.off()





tikz("~/Dropbox/Documents/statsforbios/glm/normalPDF3.tex", width = 6, height = 6, 
standAlone = TRUE,
packages = c("\\usepackage{tikz}",
"\\usepackage[active,tightpage,psfixbb]{preview}",
"\\PreviewEnvironment{pgfpicture}",
"\\setlength\\PreviewBorder{0pt}",
"\\usepackage{amssymb}",
"\\usepackage[scaled]{helvet}",
"\\renewcommand*\\familydefault{\\sfdefault}", 
"\\usepackage[T1]{fontenc}",
"\\usepackage{textgreek}"))



par(mar=c(4,4,0,0)+0.3, cex=1.8)
plot(dnorm(seq(-3,5,length.out=1000,), 0, 1 )~ seq(-3,5,length.out=1000,), type='l', col = r, lwd=11, ylab='Density', xlab='x',xaxs='i', yaxs='i', ylim=c(0,0.45), xlim=c(-3,6.2))
text(3.6, 0.4, '$\\frac{1}{\\sigma \\sqrt{2\\pi}} e^{-\\frac{({\\color[HTML]{377EB8}x_1}-\\mu)^2}{2\\sigma^2}}$', cex=1.5)
text(3.4, 0.33, '$\\times\\frac{1}{\\sigma \\sqrt{2\\pi}} e^{-\\frac{({\\color[HTML]{A65628}x_2}-\\mu)^2}{2\\sigma^2}}$', cex=1.5)
points(-1,0.004, cex=2, col=b, pch=16)
arrows(-1,0.004, -1, dnorm(-1), col=b, lwd=arrowWidth)
arrows(-1, dnorm(-1), -2.9, dnorm(-1), col=b, lwd=arrowWidth)
points(-0.3,0.004, cex=2, col=br, pch=16)
arrows(-0.3,0.004, -0.3, dnorm(-0.3), col=br, lwd=arrowWidth)
arrows(-0.3, dnorm(-0.3), -2.9, dnorm(-0.3), col=br, lwd=arrowWidth)
text(-1.6,0.02, '$x_1$', cex=1.5, col=b)
text(0.3,0.02, '$x_2$', cex=1.5, col=br)

dev.off()




