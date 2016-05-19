library(ggplot2)
library(tidyr)
library(nnet)
library(ggthemes)
library(e1071)
library(gbm)
library(segmented)
library(palettetown)

setwd('~/Dropbox/Documents/statsforbios/differentMLmodels')

x = runif(1000, -10, 10)
y = rnorm(1000, sd = 50) + x^3 

df <- data.frame(x, y)
ggplot(df, aes(x, y)) + geom_point()


newx <- data.frame(x = seq(-15, 15, length.out = 2000))


nn4 <- nnet(x, y, size = 4, linout = TRUE)
nn4.preds <- predict(nn4, newx)


nn1 <- nnet(x, y, size = 1, linout = TRUE)
nn1.preds <- predict(nn1, newx)




nn20 <- nnet(x, y, size = 20, linout = TRUE)
nn20.preds <- predict(nn20, newx)



df.out <- cbind(newx, nn4 = nn4.preds, nn1 = nn1.preds, nn20 = nn20.preds)

longdf <- gather(df.out, Model, Prediction, -x)


ggplot(longdf, aes(x, Prediction, colour = Model)) + 
  geom_point(data = df, aes(x, y), colour = 'black', alpha = 0.4) +
  geom_line(size = 1.3)   +
  theme_tufte() + theme(text = element_text(size = 30)) + ylab('y')


ggsave('neuralNets.png', dpi = 500)



svmlin <- svm(x, y, kernel = 'linear')
svmlin.preds <- predict(svmlin, newdata = newx)

svmpoly <- svm(x, y, kernel = 'polynomial')
svmpoly.preds <- predict(svmpoly, newdata = newx)


svmrad <- svm(x, y, kernel = 'radial')
svmrad.preds <- predict(svmrad, newdata = newx)




df.out <- cbind(newx, linear = svmlin.preds, poly3 = svmpoly.preds, rad = svmrad.preds)

longdf <- gather(df.out, Model, Prediction, -x)


ggplot(longdf, aes(x, Prediction, colour = Model)) + 
  geom_point(data = df, aes(x, y), colour = 'black', alpha = 0.4) +
  geom_line(size = 1.3)   +
  theme_dark() + theme(text = element_text(size = 30)) + ylab('y')

ggsave('svm.png', dpi = 500)




gbm10000 <- gbm(y ~ x, data = data.frame(x, y), distribution = 'gaussian', n.trees = 10000)
gbm100.preds <- predict(gbm10000, newdata = newx, n.trees = 100, type = 'response')
gbm1000.preds <- predict(gbm10000, newdata = newx, n.trees = 1000, type = 'response')
gbm10000.preds <- predict(gbm10000, newdata = newx, n.trees = 10000, type = 'response')


gbmlap10000 <- gbm(y ~ x, data = data.frame(x, y), distribution = 'laplace', n.trees = 10000)
gbmlap1000.preds <- predict(gbmlap10000, newdata = newx, n.trees = 1000, type = 'response')

gbmt10000 <- gbm(y ~ x, data = data.frame(x, y), distribution = 'tdist', n.trees = 10000)
gbmt1000.preds <- predict(gbmt10000, newdata = newx, n.trees = 1000, type = 'response')

gbmt810000 <- gbm(y ~ x, data = data.frame(x, y), distribution = list(name = 'tdist', df = 8), n.trees = 10000)
gbmt81000.preds <- predict(gbmt810000, newdata = newx, n.trees = 1000, type = 'response')


df.out <- cbind(newx, gaus100 = gbm100.preds, gaus1000 = gbm1000.preds, gaus10000 = gbm10000.preds, 
                laplace1000 = gbmlap1000.preds,
                tdist4.1000 = gbmt1000.preds,
                tdist8.1000 = gbmt81000.preds)

longdf <- gather(df.out, Model, Prediction, -x)


ggplot(longdf, aes(x, Prediction, colour = Model)) + 
  geom_point(data = df, aes(x, y), colour = 'black', alpha = 0.4) +
  geom_line(size = 1.3)   +
  theme_few() + theme(text = element_text(size = 30)) + ylab('y') +
  scale_colour_hc()

ggsave('gbm.png', dpi = 500)





xylm <- lm(y ~ x, data = data.frame(x, y))

seg2 <- segmented(xylm, seg.Z = ~x, psi = 2)
seg2.preds <- predict(seg2, newx)


seg3 <- segmented(xylm, seg.Z = ~x, psi = c(-1, 1, 2))
seg3.preds <- predict(seg3, newx)


seg8 <- segmented(xylm, seg.Z = ~x, psi = c(-4, -3, -2, -1, 1, 2, 3, 5))
seg8.preds <- predict(seg8, newx)







df.out <- cbind(newx, seg2 = seg2.preds,  seg3 = seg3.preds,  seg8 = seg8.preds)

longdf <- gather(df.out, Model, Prediction, -x)


ggplot(longdf, aes(x, Prediction, colour = Model)) + 
  geom_point(data = df, aes(x, y), colour = 'black', alpha = 0.4) +
  geom_line(size = 1.3)   +
  theme_solarized() + theme(text = element_text(size = 30)) + ylab('y') +
  scale_colour_poke(pokemon = 'charizard', spread = 3)
ggsave('seg.png', dpi = 500)







