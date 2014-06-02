# This script is the test suite that will test all the functions in testThatPracticeFunctions.R
#
# To run, go to an R console and do
# > library(testthat) 
# > test_file('pathToThisFile/testthatPracticeTestings.R)
#
# For more ways to run e.g. automatically etc. see http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf

context("Test all functions in testThatPracticeFunctions.R")

source('testthatPracticeFunctions.R')


# Put in some known square numbers to check basic functionality.
test_that("squareFunc gives correct square numbers", {
	expect_that(squareFunc(2), equals(4))
	expect_that(squareFunc(1), equals(1))	
	expect_that(squareFunc(-1), equals(1))
})


# Check that we get a number as output, and that exceptions are handled
test_that("squareFunc uses types correctly", {
	expect_true(is.numeric(squareFunc(2)))
	expect_error(squareFunc('c'))
})


# Check addAB (which I have written incorrectly)

test_that("addAB gives correct output", {
	expect_equal(addAB(1,1), 2)
	expect_equal(addAB(2,6), 8)
	expect_false(addAB(2,2)==5)
})

