
d <- data.frame(x = gl(2, 50), y = rnorm(100))

ggplot(d, aes(x, y)) +
  geom_boxplot()





calcWidth <- function(v, p){
  dens <- density(v)
  return(dens$y[round(512 * p)])
}

ggplot(d, aes(x, y)) +
  geom_violin() +
  stat_summary(geom = "crossbar",  
    fun.data = function(v){ return(c(y = median(v), ymin = median(v), ymax = median(v), width = calcWidth(v, 0.5))) }) +
  stat_summary(geom = "crossbar",  
    fun.data = function(v){ return(c(y = quantile(v, 0.25)[[1]], ymin = quantile(v, 0.25)[[1]], ymax = quantile(v, 0.25)[[1]], width = calcWidth(v, 0.25))) }) +
  stat_summary(geom = "crossbar",  
    fun.data = function(v){ return(c(y = quantile(v, 0.75)[[1]], ymin = quantile(v, 0.75)[[1]], ymax = quantile(v, 0.75)[[1]], width = calcWidth(v, 0.75))) }) 
