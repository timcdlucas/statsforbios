

```r
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
```

```
##                   Order                  Family                   Genus 
##                       0                       0                       0 
##                 Species                Binomial           ActivityCycle 
##                       0                       0                    3757 
##               AdultMass      AdultForearmLen_mm                AdultLen 
##                    1874                    4513                    3475 
##       AgeatEyeOpening_d       AgeatFirstBirth_d     BasalMetRate_mLO2hr 
##                    4940                    4971                    4843 
##      BasalMetRateMass_g             DietBreadth          DispersalAge_d 
##                    4843                    3255                    5272 
##          GestationLen_d          HabitatBreadth           HomeRange_km2 
##                    4054                    2692                    4709 
##     HomeRange_Indiv_km2    InterbirthInterval_d              LitterSize 
##                    4790                    4721                    2915 
##          LittersPerYear          MaxLongevity_m             NeonateMass 
##                    4522                    4403                    4331 
##              NeonateLen PopulationDensity_n.km2       PopulationGrpSize 
##                    5190                    4460                    5028 
##     SexualMaturityAge_d           SocialGrpSize              TeatNumber 
##                    4365                    4709                    4775 
##          Terrestriality            TrophicLevel            WeaningAge_d 
##                    2780                    3255                    4252 
##       WeaningBodyMass_g   WeaningHeadBodyLen_mm              References 
##                    4929                    5369                    1250 
##     AdultBodyMass_g_EXT      LittersPerYear_EXT   NeonateBodyMass_g_EXT 
##                    5022                    5022                    5392 
##   WeaningBodyMass_g_EXT                Area_km2               MaxLat_dd 
##                    5391                     748                     748 
##               MinLat_dd          MidRangeLat_dd              MaxLong_dd 
##                     748                     748                     748 
##              MinLong_dd         MidRangeLong_dd      HuPopDen_Min_n.km2 
##                     748                     748                     748 
##     HuPopDen_Mean_n.km2       HuPopDen_5p_n.km2         HuPopDen_Change 
##                     748                     748                     791 
##          Precip_Mean_mm        Temp_Mean_01degC             AET_Mean_mm 
##                     883                     883                    1111 
##             PET_Mean_mm 
##                    1111
```

```r
theme_set(theme_grey() + theme(text = element_text(size = 22)))


ggplot(p, aes(x = MaxLongevity_m, y = SocialGrpSize)) +
  geom_point() +
  annotate('text', x = 1000, y = 70, label = 'Data: Pantheria')
```

```
## Warning: Removed 5047 rows containing missing values (geom_point).
```

![plot of chunk plots](figure/plots-1.png) 

```r
ggsave('~/Dropbox/Documents/statsforbios/ggplot2/point.png')
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 5047 rows containing missing values (geom_point).
```

```r
theme_set(theme_solarized_2() + theme(text = element_text(size = 22)))
```

```
## Warning: New theme missing the following elements: panel.margin.x,
## panel.margin.y
```

```r
ggplot(p, aes(x = log(AdultLen), y = AdultForearmLen_mm)) +
  geom_bin2d() +
  annotate('text', x = 4, y = 230, label = 'Data: Pantheria')
```

![plot of chunk plots](figure/plots-2.png) 

```r
ggsave('~/Dropbox/Documents/statsforbios/ggplot2/bin2d.png')
```

```
## Saving 7 x 7 in image
```

```r
p %>% group_by(Order) %>% summarise(n()) %>% print.data.frame
```

```
##               Order  n()
## 1      Afrosoricida   51
## 2      Artiodactyla  240
## 3         Carnivora  286
## 4           Cetacea   84
## 5        Chiroptera 1116
## 6         Cingulata   21
## 7    Dasyuromorphia   71
## 8        Dermoptera    2
## 9   Didelphimorphia   87
## 10    Diprotodontia  143
## 11   Erinaceomorpha   24
## 12       Hyracoidea    4
## 13       Lagomorpha   92
## 14    Macroscelidea   15
## 15   Microbiotheria    1
## 16      Monotremata    5
## 17 Notoryctemorphia    2
## 18 Paucituberculata    6
## 19  Peramelemorphia   21
## 20   Perissodactyla   17
## 21        Pholidota    8
## 22           Pilosa   10
## 23         Primates  376
## 24      Proboscidea    3
## 25         Rodentia 2277
## 26       Scandentia   20
## 27          Sirenia    5
## 28     Soricomorpha  428
## 29    Tubulidentata    1
```

```r
theme_set(theme_solarized_2(light = FALSE) + theme(text = element_text(size = 22)))
```

```
## Warning: New theme missing the following elements: panel.margin.x,
## panel.margin.y
```

```r
p %>% 
  filter(Order == 'Primates') %>% 
  select(NeonateMass, NeonateLen, AdultMass, AdultLen) %>%
  filter(complete.cases(.)) %>%
ggplot(.,aes(NeonateMass, NeonateLen, xend=AdultMass, yend=AdultLen)) + 
  geom_segment(arrow=arrow()) +
  xlab('Body Mass (g)') + ylab('Head-Body length (mm)') +
  #scale_x_log10() + scale_y_log10() +
  ggtitle('Primate growth (neonate to adult)')
```

![plot of chunk plots](figure/plots-3.png) 

```r
ggsave('~/Dropbox/Documents/statsforbios/ggplot2/segment.png')
```

```
## Saving 7 x 7 in image
```

