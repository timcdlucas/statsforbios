
# Needs extrafont
#   Also need to do all the stuff to get extra font working (font_import() etc)
#   https://cran.r-project.org/web/packages/extrafont/README.html
#   And will also require the Lato font family to be installed.
library(extrafont)


library(ggplot2)
library(grid)

# Mostly stolen from @noamross
#  https://github.com/noamross/noamtools/blob/master/R/theme_nr.R

theme_tcdl <-theme(text = element_text(family = "Lato Light", size = 12),
        panel.grid.major.x = element_line(colour = "#ECECEC", size = 0.3, linetype = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "#ECECEC", size = 0.3, linetype = 1),
        legend.title = element_text(size = 10, colour  =  "#8B8B8B"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 10, colour  =  "#8B8B8B"),
        axis.text = element_text(color = "grey", size = 11, family  =  "Lato Black"),
        axis.title = element_text(size = 11),
        axis.title.y = element_text(vjust = 1.5),
        axis.title.x = element_text(vjust = -1),
        axis.line  =  element_line(colour  =  "grey"),
        title = element_text(size = 22),
        panel.border  =  element_blank(), 
        plot.margin  =  unit(c(0.3,0.1,1,1.3), "lines"),
        panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(colour = NA, fill = 'transparent'),
        strip.background = element_rect(colour = 'grey', fill = 'grey'),
        strip.text = element_text(colour = 'white', family = 'Lato Light', size = 10)
)

theme_set(theme_grey() + theme_tcdl)



d <- data.frame(x = rep(letters[5:8], each = 1000), 
                model = rep(letters[1:4], 1000),
                error = rnorm(4000),
                expression = rep(letters[1:4], 1000))
                


ggplot(d, aes(x = x, y = error, colour = expression, fill = expression)) + 
  facet_grid(. ~ model) +
  geom_boxplot(outlier.colour = grey(0.3), notch = TRUE, notchwidth = 0.7, alpha = 0.7) +
  scale_colour_manual(values = brewer.pal(8, 'Set1')[c(5, 4, 2, 1)]) +
  scale_fill_manual(values = brewer.pal(8, 'Set1')[c(5, 4, 2, 1)]) +
  stat_summary(geom = "crossbar", width=0.45, fatten=0, color="white", 
    fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) +
  theme(legend.position = 'none') +
  ylab('Percentage Error') +
  xlab('Max Angle') +
  scale_x_discrete(breaks=letters[5:8],
                      labels=c(expression(phantom(over(0, 0))*0*phantom(0)), 
                        expression(over(pi, 3)), 
                        expression(over(2*pi, 3)), 
                        expression(phantom(over(0, 0))*pi*phantom(0))))


