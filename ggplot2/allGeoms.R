
#+ dataRead

library(ggplot2)
library(magrittr)
library(ggthemes)
library(grid)
library(dplyr)

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


#+ plots

theme_set(theme_grey() + theme(text = element_text(size = 22)))


ggplot(p, aes(x = MaxLongevity_m, y = SocialGrpSize)) +
  geom_point() +
  annotate('text', x = 1000, y = 70, label = 'Data: Pantheria')
ggsave('~/Dropbox/Documents/statsforbios/ggplot2/point.png')


theme_set(theme_solarized_2() + theme(text = element_text(size = 22)))

ggplot(p, aes(x = log(AdultLen), y = AdultForearmLen_mm)) +
  geom_bin2d() +
  annotate('text', x = 4, y = 230, label = 'Data: Pantheria')
ggsave('~/Dropbox/Documents/statsforbios/ggplot2/bin2d.png')


p %>% group_by(Order) %>% summarise(n()) %>% print.data.frame

theme_set(theme_solarized_2(light = FALSE) + theme(text = element_text(size = 22)))
p %>% 
  filter(Order == 'Primates') %>% 
  select(NeonateMass, NeonateLen, AdultMass, AdultLen) %>%
  filter(complete.cases(.)) %>%
ggplot(.,aes(NeonateMass, NeonateLen, xend=AdultMass, yend=AdultLen)) + 
  geom_segment(arrow=arrow()) +
  xlab('Body Mass (g)') + ylab('Head-Body length (mm)') +
  #scale_x_log10() + scale_y_log10() +
  ggtitle('Primate growth (neonate to adult)')

ggsave('~/Dropbox/Documents/statsforbios/ggplot2/segment.png')





