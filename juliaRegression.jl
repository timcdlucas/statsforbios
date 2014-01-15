# A little go at Julia

# Add some packages
using Winston

# Create some data
x = linspace(0, 10, 100)

y = x + randn(100)



# Linear regression
lm = linreg(x, y)


p = FramedPlot(
        aspect_ratio=1,
        xrange=(0,10),
        yrange=(minimum(y),maximum(y)))


d = Points(x, y, kind="filled circle")
setattr(d, label="a points")


m = Slope(1, lm)
setattr(m, label="slope")

add(p, a, m)


