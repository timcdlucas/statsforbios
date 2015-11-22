+analysis


```r
setwd('~/Dropbox/Documents/statsforbios/biomass')


library(dplyr)
library(sqldf)


# Get pantheria data.
pantheria <- read.table(file = '~/Dropbox/phd/Documents/thesis/data/Chapter3/PanTHERIA_1-0_WR05_Aug2008.txt', header = TRUE, sep = "\t", na.strings = c("-999", "-999.00"))

# URL <- "http://esapubs.org/archive/ecol/e090/184/PanTHERIA_1-0_WR05_Aug2008.txt"
# pantheria <- read.table(file=URL,header=TRUE,sep="\t",na.strings=c("-999","-999.00"))

# Read wikipedia data that was saved to google docs using http://blog.ouseful.info/2008/10/14/data-scraping-wikipedia-with-google-spreadsheets/
#  Scraped from https://en.wikipedia.org/wiki/List_of_even-toed_ungulates_by_population
d <- read.csv('eventoedbiomass.csv', stringsAsFactors = FALSE)

# Clean names
d$Binomial.name <- gsub('[*]', '', d$Binomial.name)

# Remove domestic species.
d <- d[-grep('Domestic', d$Status), ]


# Clean population estimates
d$pop <- gsub('[*]|[<]|\ ', '', d$Population)
d$pop <- gsub('\\[.*\\]', '', d$pop)

d$popNum <- NA
d$popNum[grep('–', d$pop)] <- sapply(strsplit(d$pop[grep('–', d$pop)], '–'), function(x) (as.numeric(x[1]) + as.numeric(x[2]))/2 )
d$popNum[-grep('–', d$pop)] <- as.numeric(d$pop[-grep('–', d$pop)])


d$popNum
```

```
##   [1]     750     250     250     275     300     300     500     500
##   [9]     500    1250     750     750     715     950    1274    1123
##  [17]    1500    2350    1900    2500    2500    2500    2500    3250
##  [25]    3200    3200    3400    3500    4300    4291    4500    5000
##  [33]    5500    7500    7000    7500    7750    8000   10000   10000
##  [41]   10000   10000   10000   14500   21500   14000   15000   15000
##  [49]   17500   18000   18000   20000   50000   25000   28000   28000
##  [57]   30000   30000   32000   35000   42500   36000   42000   42000
##  [65]   50000   50000   50000   62000   67000   73000   75000   76000
##  [73]   80000   99500   95000   95000  100000  100000  101000  114000
##  [81]  118000  130000  136500  130000  136000  160000  170000  170000
##  [89]  188000  200000  212000  219000  278000  287000  300000  347273
##  [97]  350000  362000  365000  373000  382000  400000  482000  485600
## [105]  511000  530000  562750  550000  600000  700000  725000  750000
## [113]  890000  971000 1000000 1340000 1500000 1550000 1660000 2000000
## [121] 2250000 2137000 7000000
```

```r
# Join pantheria and wiki data.

mass <- sqldf("
  SELECT [X5.1_AdultBodyMass_g]
  AS mass
  FROM d
  LEFT JOIN pantheria
  ON d.[Binomial.name]=pantheria.MSW05_Binomial
  ")


d <- cbind(d, mass)

allMass <- sum(na.omit(d$popNum * d$mass))

#convert to tonnes
allMasTon <- allMass / 907185

allMasTon
```

```
## [1] 3457289
```

```r
# 104 species w/ estimates.
#   Which is about half of ungulates, biased towards more populous species?
length(na.omit(d$mass))
```

```
## [1] 104
```

