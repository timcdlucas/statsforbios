df <- data.frame(x = rpois(100, 3), y = rep(c('a', 'b'), 50), sub = sample(letters[1:3], 100, TRUE))

df2 <-  rbind(
    cbind(df, faceter = "Whole Sample")
    , cbind(df[df$sub == "a" ,], faceter = "Subset A")
    #other subsets go here...
)

ggplot(df2, aes(x = y, y = x)) + 
  geom_jitter(position = position_jitter(width = .2, height = 0)) +
  facet_wrap(~ faceter)



ggplot(df2, aes(x = y, y = x)) + 
  geom_dotplot()
  facet_wrap(~ faceter)


ggsave('~/Desktop/jit.png')


# calculate offsets
df3 <- ddply(df2, .(x, y), transform, offset = (1:length(y)-1)/50)
qplot(y, x, data=df3) + stat_identity(aes(as.numeric(y)+offset)) 

ggsave('~/Desktop/stack.png')


