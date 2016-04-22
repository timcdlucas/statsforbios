library(rpart)
library(ggplot2)
library(tidyr)
library(palettetown)

source('https://raw.githubusercontent.com/timcdlucas/ggplotThemes/master/theme_tcdl.R')

d <- data.frame(x = runif(100))

d$y <- d$x * 0.3 + rnorm(100, 0.4, 0.05)

ggplot(d, aes(x, y)) +
  geom_point() +  
  ylim(0, 1)

tree <- rpart(y ~ x, data = d, method = 'anova')


fits <- data.frame(x = seq(-1, 2, length.out = 1000))
fits$tree <- predict(tree, newdata = data.frame(x = fits$x), type = 'vector')

glm <- lm(y ~ x, data = d)

fits$lm <- predict(glm, newdata = data.frame(x = fits$x))


fits.ti <- gather(fits, key = model, value = y, -x)

ggplot(d, aes(x, y)) +
  geom_point() +  
  geom_line(data = fits.ti, aes(x, y, colour = model), size = 1.5) +
  ylim(0, 1) +
  scale_colour_poke(pokemon = 'Charizard', spread = 2) +
  theme_pres

ggsave('treevslm.png', dpi = 400)
