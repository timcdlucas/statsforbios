
#+ dataRead

library(ggplot2)
library(magrittr)
library(ggthemes)
library(grid)
library(dplyr)
library(palettetown)
library(cowplot)

source('~/Dropbox/phd/Documents/thesis/misc/theme_tcdl.R')

p <- read.table(file = '~/Dropbox/phd/Documents/thesis/data/Chapter3/PanTHERIA_1-0_WR05_Aug2008.txt',
  header = TRUE, sep = "\t", na.strings = c("-999", "-999.00"))

names(p) <- c("Order", "Family",
"Genus", "Species",
"Binomial", "ActivityCycle",
"AdultMass", "AdultForearmLen_mm",
"AdultLen", "AgeatEyeOpening_d",
"AgeatFirstBirth_d", "BasalMetRate_mLO2hr",
"BasalMetRateMass_g", "DietBreadth",
"DispersalAge_d", "GestationLen_d",
"HabitatBreadth", "HomeRange_km2",
"HomeRange_Indiv_km2", "InterbirthInterval_d",
"LitterSize", "LittersPerYear",
"MaxLongevity_m", "NeonateMass",
"NeonateLen", "PopulationDensity_n.km2",
"PopulationGrpSize", "SexualMaturityAge_d",
"SocialGrpSize", "TeatNumber",
"Terrestriality", "TrophicLevel",
"WeaningAge_d", "WeaningBodyMass_g",
"WeaningHeadBodyLen_mm", "References",
"AdultBodyMass_g_EXT", "LittersPerYear_EXT",
"NeonateBodyMass_g_EXT", "WeaningBodyMass_g_EXT",
"Area_km2", "MaxLat_dd",
"MinLat_dd", "MidRangeLat_dd",
"MaxLong_dd", "MinLong_dd",
"MidRangeLong_dd", "HuPopDen_Min_n.km2",
"HuPopDen_Mean_n.km2", "HuPopDen_5p_n.km2",
"HuPopDen_Change", "Precip_Mean_mm",
"Temp_Mean_01degC", "AET_Mean_mm",
"PET_Mean_mm")


apply(p, 2, function(x) sum(is.na(x)))

#'## geom_point

#' Plot (x, y) points

#+ point

theme_set(theme_grey() + theme(text = element_text(size = 22)))


ggplot(p, aes(x = MaxLongevity_m, y = SocialGrpSize)) +
  geom_point() +
  annotate('text', x = 1000, y = 70, label = 'Data: Pantheria')
ggsave('~/Dropbox/Documents/statsforbios/ggplot2/point.png')



ggplot(p, aes(x = MaxLongevity_m, y = SocialGrpSize)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  annotate('text', x = 10, y = 70, label = 'Data: Pantheria')
ggsave('~/Dropbox/Documents/statsforbios/ggplot2/point2.png')


p %>%
  mutate(hlight = Order == 'Primates') %>%
ggplot(., aes(x = MaxLongevity_m, y = SocialGrpSize, col = hlight)) +
  geom_point() + 
  scale_colour_poke(pokemon = 'golbat', spread = 2) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  annotate('text', x = 10, y = 70, label = 'Data: Pantheria')





#'## geom_bin2d

#' Bin (x, y) points and plot with colour to show intensity

#+ bin2d

theme_set(theme_solarized_2() + theme(text = element_text(size = 22)))

ggplot(p, aes(x = log(AdultLen), y = AdultForearmLen_mm)) +
  geom_bin2d() +
  annotate('text', x = 4, y = 230, label = 'Data: Pantheria')
ggsave('~/Dropbox/Documents/statsforbios/ggplot2/bin2d.png')


#'## geom_segment

#' Connect (x1, y1) to (x2, y2) for a set of data points.


#+ segment
theme_set(theme_solarized_2(light = FALSE) + theme(text = element_text(size = 22)))
p %>% group_by(Order) %>% summarise(n()) %>% print.data.frame


p %>% 
  filter(Order == 'Primates') %>% 
  select(NeonateMass, NeonateLen, AdultMass, AdultLen) %>%
  filter(complete.cases(.)) %>%
ggplot(.,aes(NeonateMass, NeonateLen, xend=AdultMass, yend=AdultLen)) + 
  geom_segment(arrow=arrow()) +
  xlab('Body Mass (g)') + ylab('Head-Body length (mm)') +
  annotate('text', x = 500, y = 450, label = 'Data: Pantheria', colour = 'grey') +
  #scale_x_log10() + scale_y_log10() +
  ggtitle('Primate growth (neonate to adult)')

ggsave('~/Dropbox/Documents/statsforbios/ggplot2/segment.png')







#'## geom_area
 
#' 


#+ area
theme_set(theme_minimal() + theme_hc() + theme(text = element_text(size = 22)))



ggplot(p, aes(x = MidRangeLat_dd, y = SocialGrpSize)) + 
  geom_area(stat = 'smooth', method = 'loess', span = 0.2, fill = pokepal('pineco')[1], colour = pokepal('pineco')[2]) +
  xlab('Latitude (middle)') +
  ylab('Smoothed Social Group Size') +
  annotate('text', x = -35, y = 8, label = 'Data: Pantheria', colour = 'darkgrey') 

ggsave('~/Dropbox/Documents/statsforbios/ggplot2/area.png')







#'## geom_bar
 
#' 


#+ bar
theme_set(theme_tufte() + theme(text = element_text(size = 22)))



ggplot(p, aes(x = reorder(Order,Order,
                     function(x) length(x)))) + 
  geom_bar() +
  coord_flip() +
  ylab('Number of Species in Pantheria') + 
  xlab('Order')


ggsave('~/Dropbox/Documents/statsforbios/ggplot2/bar.png')



#'## geom_boxplot
 
#' 


#+ boxplot
theme_set(theme_minimal() + theme_tcdl + theme(text = element_text(size = 22)))



p %>%
  filter(!is.na(TrophicLevel)) %>%
ggplot(., aes(x = factor(TrophicLevel), y = AdultMass)) + 
  geom_boxplot(notch = TRUE) +
  scale_y_log10() +
  xlab('Trophic Level') +
  ylab('Body Mass (g)') +
  scale_x_discrete(labels = c('Herbivore', 'Omnivore', 'Predator')) +
  annotate('text', x = 1, y = 7e7, label = 'Data: Pantheria', colour = 'darkgrey') 


ggsave('~/Dropbox/Documents/statsforbios/ggplot2/boxplot.png')


p %>% 
  dplyr::filter(Genus == 'Vampyressa') %>%
  dplyr::select(Binomial, Area_km2, HuPopDen_Mean_n.km2) %>%
  dplyr::filter(complete.cases(.)) %>%
ggplot(., aes(x = HuPopDen_Mean_n.km2, y = Area_km2, label = Binomial)) +
  geom_text() +
  xlim(-5, 50)

ggsave('~/Dropbox/Documents/statsforbios/ggplot2/geom_text.png')


p %>% 
  dplyr::filter(Genus == 'Gerbillus') %>%
  dplyr::select(MinLat_dd, MaxLat_dd, MaxLong_dd, MinLong_dd) %>%
  ggplot(., aes(xmin = MinLong_dd, xmax = MaxLong_dd, ymin = MinLat_dd, ymax = MaxLat_dd)) +
    geom_rect(alpha = 0.2) +
    theme_cowplot()+
    xlab('Longitude') + ylab('Latitude') +
    ggtitle("Gerbillus species Lat/Lon ranges.") +
    annotate('text', x = 0, y = 0, label = 'Data: Pantheria', colour = 'Black') 

ggsave('~/Dropbox/Documents/statsforbios/ggplot2/geom_rect.png')






