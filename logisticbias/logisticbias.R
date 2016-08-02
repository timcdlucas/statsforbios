
# How many sites, how many sampling occasions
sites <- 10
time <- 2

# Create population samples
p <- data.frame(pop = rpois(sites * time, 100), time = rep(1:time, by = sites), site = rep(letters[1:sites], each = time))

# Which site do we choose to sample? The biggest at time == 1!
whichSite <- p$site == levels(p$site)[which.max(p[p$time == 1, 'pop'])]

ggplot(p, aes(time, pop, colour = site)) + 
  geom_point(size = 2) + 
  geom_line() +
  geom_smooth(data = p[whichSite, ], method = 'lm')



# Now how does it look with 10 sampling occasions
sites <- 10
time <- 10

p <- data.frame(pop = rpois(sites * time, 100), time = rep(1:time, by = sites), site = rep(letters[1:sites], each = time))

whichSite <- p$site == levels(p$site)[which.max(p[p$time == 1, 'pop'])]

ggplot(p, aes(time, pop, colour = site)) + 
  geom_point(size = 2) + 
  geom_line() +
  geom_smooth(data = p[whichSite, ], method = 'lm')





# Do some bigger sims.


# How many sites, how many sampling occasions
sites <- 10
time <- 2

reps <- 1000
slope <- rep(NA, reps)

for(i in 1:reps){
  # Create population samples
  p <- data.frame(pop = rpois(sites * time, 100), time = rep(1:time, by = sites), site = rep(letters[1:sites], each = time))

  whichSite <- p$site == levels(p$site)[which.max(p[p$time == 1, 'pop'])]

  slope[i] <- coef(lm(pop ~ time, data = p[whichSite, ]))[2]

}






# How many sites, how many sampling occasions
sites <- 10
time <- 10

reps <- 1000
slope2 <- rep(NA, reps)

for(i in 1:reps){
  # Create population samples
  p <- data.frame(pop = rpois(sites * time, 100), time = rep(1:time, by = sites), site = rep(letters[1:sites], each = time))

  whichSite <- p$site == levels(p$site)[which.max(p[p$time == 1, 'pop'])]

  slope2[i] <- coef(lm(pop ~ time, data = p[whichSite, ]))[2]

}


results <- data.frame(slope = c(slope, slope2), samplingoccasions = c(rep(2, reps), rep(10, reps)))


ggplot(results, aes(x = slope, fill = factor(samplingoccasions), group = samplingoccasions)) + 
  geom_density(alpha = 0.6)

ggsave('~/Dropbox/Documents/statsforbios/logisticbias/results.png')

  
