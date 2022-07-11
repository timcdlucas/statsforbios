
library(dplyr)

N <- rep(c(50, 100, 500, 1000, 5000, 10000), each = 30)

out <- list()

for(i in seq_along(N)){
  
  x1 <- rnorm(N[i])
  x2 <- x1 + rnorm(N[i], sd = 0.1)
  y = 2*x1 + x2 + rnorm(N[i])
  
  m <- lm(y ~ x1 + x2, data = data.frame(y, x1, x2))

  out[[i]] <-   
  broom::tidy(m, conf.int = TRUE) %>%
    slice(2:3) %>% 
    cbind(N = N[i])
}


out_df <- do.call(rbind, out)

hline <- data.frame(term = c('x1', 'x2'), true = c(2, 1))

ggplot(out_df, aes(x = N)) + 
  scale_x_continuous(trans = 'log10') + 
  geom_jitter(aes(y = conf.low), width = 0.05, height = 0, 
              colour = 'darkred', alpha = 0.5) + 
  geom_jitter(aes(y = conf.high), width = 0.05, height = 0, 
              colour = 'darkblue', alpha = 0.5) + 
  facet_grid(~term, scales = 'free_y') + 
  geom_hline(data = hline, aes(yintercept = true))
ggsave('C:/Users/tcdl2/Dropbox/Documents/statsforbios/collinearity/confints.png')



png('C:/Users/tcdl2/Dropbox/Documents/statsforbios/collinearity/underlying_collinearity.png')
plot(x1, x2)
dev.off()
