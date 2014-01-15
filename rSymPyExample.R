
# Call the rSymPy library
library(rSymPy)

# Create a sympy variable 'x'. This is only callable withint sympy.
sympy("var('x')")

# Integrate
sympy("i = integrate(x**2)")

# Solve
sympy("solve(i, x)")
