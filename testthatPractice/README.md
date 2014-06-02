# example use of testthat package

## What it does

`testthat` is a package that provides a suite of functions to make unit testing easy in R. 

Unit testing is a way to test that your code is doing what you think it is doing. Maybe you edited a function and now it does something slightly different to what you expect. Maybe when writing your code you didn't consider a case that will cause a bug. 

The idea is to write you code as modular functions. Then in a seperate testing file you write some examples of expected outcomes. To test sqrt() you will check that sqrt(4) does in fact equal 2. 


## This example

There are two files. testthatPracticeFunctions.R contains a few very simple functions one of which is deliberately written wrong. The second file contains the unit tests. To run the tests go to an R console and do
    library(testthat) 
    test_file('pathToThisFile/testthatPracticeTestings.R)

Assuming you have already installed the `testthat` package this will give some output that confirms that one of the functions in testthatPracticeFunctions.R is not doing what it should be.


## More info

For a more in depth run through of the package see here.

[http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf](http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf)


