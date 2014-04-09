#Wavelet analysis

# Simulate periodic data
t=rnorm(1e4)+5*sin(seq(1,50,length.out=1e4))

# Run wavelet analysis, and plot. Simples.
w=Rwave::cwt(t,100)
