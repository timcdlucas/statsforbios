statsforbios
============

Code snippets for twitter handle [@statsforbios](www.twitter.com/statsforbios)

There is a [wiki](https://github.com/timcdlucas/statsforbios/wiki). Feel free to add to it. I don't really know what it's for but oh well.

If you want to add some snippets or example code or whatever, clone the repo, add your files, then submit a pull requset. 

Feel free to use, change copy anything that I have written in this repo, no accreditation needed (though it is always nice..)
To be clear, all the R files, images that have been created by me are licensed under [The Unlicense](http://unlicense.org/). However there is some other stuff in the repo such as my blog style pages and some twitter data etc. I don't own any of that. The balzac jekyll theme is in this repo and is under an MIT [license](LICENSE).



## Statistical techniques



##### [totalLeastSquares2013-09-06.R](totalLeastSquares2013-09-06.R)
Example total least squares analysis using `gx.rma` in `rgr`. Total least squares is a regression method that minimises the error in both x and y coordinates rather than just y. 


##### [jackknifeRegression13-05-04.R](jackknifeRegression13-05-04.R) 	
140 char jackknife for confidence intervals of a OLS regression. 

##### [bootstrapRegression13-05-04.R](bootstrapRegression13-05-04.R) 	
Very basic bootstrap of regression with 140 char version and fuller version. Bootstrap is used to calculate confidence intervals for a OLS regression.

##### [neuralNetworkExample2013-09-02.R](neuralNetworkExample2013-09-02.R)	
Simplest possible neural network analysis (machine learning method) using the `nnet` package.

##### [powerAnalysis03-10-13.R](powerAnalysis03-10-13.R) 	
Example power analysis. Used to make a plot of the sample size needed for a range of effect sizes. Performed using the `pwr` package


##### [smoothScatterDemo13-05-04.R](smoothScatterDemo13-05-04.R) 
Demonstration of `smoothScatter()` function that creates and plots a 2D kernel over a cloud of points.

  
  
  
## Programming techniques


##### [infixApplyFunctions2013-09-10.R](infixApplyFunctions2013-09-10.R) 
Messing around with infix functions i.e. 2 + 2 is infix while add(2,2) is not. 
  

##### [juliaRegression.jl](Jula regression)
Absolute bare bones linear regression and plotting in Julia

##### [sympyExample.py](SymPy symbolic algebra)
Very basic example of using symbolic algebra in Python with SymPy.

##### [rSymPyExample.R](rSymPyExample)
Computer algebra using SymPy in R via rSymPy.

##### [floatingPoint2014-2-28.R](floating point problems)
Some examples of why floating points can be dangerous

  
  

## Analysis I think is interesting

##### [anovaVswilcox2013-09-05.R](anovaVswilcox2013-09-05.R) 	
Compare power of anova and wilcox tests for a range of n, variance and difference in means.

  
  
  
  

## Plots for various uses 

##### [stackedBarVSLines.R](Stacked bar chart alternatives)
Comparing stacked bar charts with alternatives. Particularly lines charts. Due to this conversation on [https://twitter.com/_NickGolding_/status/407637683830140929](twitter)

##### [quantileRegressionData13-05-04.R](quantileRegressionData13-05-04.R) 	
Just creating a plot of trangle like data that are common in ecology and a useful application of quantile regression. No quantile regression is actually run.

##### [surfaceForBanner2013-05-05.R](surfaceForBanner2013-05-05.R) 	
The 3D gaussian surface used as my twitter banner. Uses `persp` in `MASS`

##### [tuftePlots.R](tuftePlots.R)
Example usage of Tufte style themes from `ggthemes`. Range plot and mimimal bar charts.


