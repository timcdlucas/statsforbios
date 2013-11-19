## Example use of tufte themes
## Tim Lucas
## 19-11-13

library(ggplot2)
library(ggthemes)

setwd('~/Dropbox/Documents/statsforbios')

## Tufte style bar charts
(ggplot(mtcars, aes(factor(cyl), mpg))
+ geom_tufteboxplot() +
theme_tufte())


# Bar chart with boxes
(ggplot(mtcars, aes(factor(cyl), mpg))
 + geom_tufteboxplot(usebox=TRUE, fatten=1) +
theme_tufte())


## Range plot

(ggplot(mtcars, aes(wt, mpg))
+ geom_point() + geom_rangeframe()
+ theme_tufte())

# Range plot with rug

(ggplot(mtcars, aes(wt, mpg))
+ geom_point() + geom_rangeframe()
+ geom_rug() + theme_tufte())
