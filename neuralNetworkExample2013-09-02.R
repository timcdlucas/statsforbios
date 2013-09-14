# Neural network example


d<-data.frame(x1=1:100, x2=rnorm(100), y=rep(c('a','b'),each=50))  # create a training dataset

nn <- nnet::nnet(y~x1+x2,d, size=3) # Train the neural network

unseen <- data.frame(x1=sample(100,50), x2=rnorm(50)) # Create 50 datapoints to try and classify.

round(predict(nn,unseen[,1:2])) # Apply neural network to unknown data.
