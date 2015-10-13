data(beaver1)


library(ggplot2)
library(ggthemes)
library(palettetown)
library(lubridate)
library(dplyr)
library(stringr)

setwd('~/Dropbox/Documents/statsforbios/')

beavers <- rbind(cbind(beaver1, beaver = '1'), cbind(beaver2, beaver = '2'))
hr <- as.character(beavers$time) %>%
        str_pad(., 4, side = "left", pad = "0") %>%
        substring(1, 2)  

min <- as.character(beavers$time) %>%
         str_pad(., 4, side = "left", pad = "0") %>%
         substring(3, 4)                      

beavers$date <- as.POSIXct(paste0('1990 ', beavers$day, ' ', hr, ':', min), format = '%Y %j %H:%M')
start <- beavers %>%
           group_by(beaver) %>%
           summarise(start = min(date)) 

beavers$start <- start$start[as.numeric(beavers$beaver)]



beavers <- beavers %>%
             mutate(elapsedHours = as.numeric(interval(start, date))/3600)


ggplot(beavers, aes(x = elapsedHours, y = temp, colour = beaver, symbol = activ)) +
  geom_line() +
  geom_point(aes(size = factor(activ))) +
  scale_colour_poke(pokemon = 'Nidorina', spread = 2, name = 'Beaver') +
  ylab(expression('Temperature ('~degree~C~')')) +
  xlab('Hours since start') +
  scale_size_manual(values = c(0, 3), name = 'Activity', labels = c('', 'Active')) +
  theme_fivethirtyeight(base_family = 'lato light')







