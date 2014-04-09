# Slightly fuller version of species distribution model using SVMs by @blasmbenito 
# https://twitter.com/BlasMBenito/status/453807628398100483

# Load packages
library(kernlab)
library(dismo)

# simulate some data. y is precense absence. a,b are envionmental variables etc.
my.dataframe <- data.frame(y = sample(c(0,1), 100, replace=T), a=rnorm(100), b=rnorm(100))

# train out SVM model.
m <- ksvm(y~., data=my.dataframe, kernel="laplacedot", kpar=list(sigma=1))

# Make predictions for new data points.
m.map <- dismo::predict(m, data.frame(a=rnorm(100), b=rnorm(100)))


