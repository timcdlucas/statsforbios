
# Import sympy
from sympy import *

# Create our symbolic x
x = symbols('x')

# Integrate! It assumes wrt x.
i = integrate(x**2)

# Solve 
solve(i, x)

