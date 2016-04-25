
library(ggplot2)
library(dplyr)


studies <- 10000

d <- data.frame(DiffMeans = rnorm(studies, 0, 0.3), 
                N1 = rpois(studies, 40),
                N2 = rpois(studies, 40),
                p1 = NA, p2 = NA)

for(i in 1:nrow(d)){

  # original experiment
  data <- data.frame(value = rnorm(d$N1[i], ),
                     treatment = sample(c('a', 'b'), d$N1[i], replace = TRUE))
  
  data$value[data$treatment == 'a'] <- data$value[data$treatment == 'a'] + d$DiffMeans[i]

  d$p1[i] <- t.test(value ~ treatment, data = data)$p.value


  # replication
  data2 <- data.frame(value = rnorm(d$N2[i]),
                     treatment = sample(c('a', 'b'), d$N2[i], replace = TRUE))
  
  data2$value[data2$treatment == 'a'] <- data2$value[data2$treatment == 'a'] + d$DiffMeans[i]

  d$p2[i] <- t.test(value ~ treatment, data = data2)$p.value
}
  


d %>%
  subset(p1 < 0.05) %>%
  ggplot(., aes(x = p2)) + 
    geom_density(fill = 'steelblue', alpha = 0.3) + 
   # scale_x_log10() +
    xlim(c(0, 0.2)) +
    geom_density(aes(x = p1), fill = 'firebrick3', alpha = 0.3) +
    geom_vline(xintercept = 0.05, colour = 'red', size = 1) +
    xlab('p value')

ggsave('~/Dropbox/Documents/statsforbios/replication.png')

