# infix function for applying a function
# 10-09-2013


# Useful when you want the object to be foremost when reading the code

'%do%' <- function(x, FUN = round){
 out <- FUN(x)
}


# examples
c(1:4) %do% mean

x <- c('1','a','3') %do% factor

rnorm(1000) %do% density %do% plot


