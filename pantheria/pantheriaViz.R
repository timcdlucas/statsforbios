
source('misc/theme_tcdl.R')

png('~/Dropbox/Documents/statsforbios/imgs/sizeLatitude.png')

pantheria %>%
  dplyr::select(MSW05_Order, X26.2_GR_MaxLat_dd, X5.1_AdultBodyMass_g) %>%
  filter(complete.cases(.)) %>% 
  mutate(lat = abs(X26.2_GR_MaxLat_dd)) %>%
  ggplot(., aes(x = lat, y = X5.1_AdultBodyMass_g)) +
    geom_point(alpha = 0.6) +
    scale_y_continuous(trans = 'log10',
                        breaks = trans_breaks('log10', function(x) 10^x),
                        labels = trans_format('log10', math_format(10^.x))) +
    geom_smooth(colour = pokepal('tangela')[1], fill = pokepal('tangela')[2]) +
    theme_pres + 
    xlab('Absolute Maximum Latitude') +
    ylab('Adult Body Mass (g)') +
    annotate(geom = 'text', label = 'Data: Pantheria\n n = 3268', x = 60, y = 1.1e6, family = 'lato light') 

dev.off()





png('~/Dropbox/Documents/statsforbios/imgs/popabundance.png')

pantheria %>%
  dplyr::select(MSW05_Order, X21.1_PopulationDensity_n.km2, X26.1_GR_Area_km2) %>%
  filter(complete.cases(.)) %>%
  mutate(PopSize = X21.1_PopulationDensity_n.km2 * X26.1_GR_Area_km2) %>%
  ggplot(., aes(x = PopSize)) + 
    geom_histogram(colour = pokepal('gardevoir')[3], fill = pokepal('gardevoir')[4]) +
    scale_x_continuous(trans = 'log10',
                        breaks = trans_breaks('log10', function(x) 10^x),
                        labels = trans_format('log10', math_format(10^.x))) +
  xlab('Pop. Density x Geographic Range Size') +
  ylab('Count') +
  theme_pres +
  annotate(geom = 'text', label = 'Data: Pantheria', x = 10e3, y = 65, family = 'lato light') 
  

dev.off()

pantheria %>%
  filter(MSW05_Order == 'Chiroptera') %>%
  dplyr::select(MSW05_Order, X26.1_GR_Area_km2) %>%
  filter(complete.cases(.)) %>% dim


