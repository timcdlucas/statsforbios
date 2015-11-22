setwd('~/Dropbox/Documents/statsforbios/biomass')


library(dplyr)
library(sqldf)


# Get pantheria data.
#pantheria <- read.table(file = '~/Dropbox/phd/Documents/thesis/data/Chapter3/PanTHERIA_1-0_WR05_Aug2008.txt',
  header = TRUE, sep = "\t", na.strings = c("-999", "-999.00"))

URL <- "http://esapubs.org/archive/ecol/e090/184/PanTHERIA_1-0_WR05_Aug2008.txt"
pantheria <- read.table(file=URL,header=TRUE,sep="\t",na.strings=c("-999","-999.00"))

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



# 104 species w/ estimates.
#   Which is about half of ungulates, biased towards more populous species?
length(na.omit(d$mass))






