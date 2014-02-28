# Problems with floating point arithmetic.
# Tim Lucas 28-2-2014



# Computers deal with discrete numbers.

# Make some small numbers
smallVals <- seq(0,10e-16,10e-18)

# Do calculations. The answers should be equal.
s <- sin(smallVals)
c <- cos(pi/2-smallVals)

# But they aren't
plot(s~c)
abline(0,1)


# Other examples

1 - 0.9 - 0.1


