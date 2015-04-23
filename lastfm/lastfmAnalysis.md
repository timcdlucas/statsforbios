---
title: "Last.fm Data analysis"
output:
  html_document:
---


Last.fm Data analysis
=======================


I discovered that you can download your [last.fm](www.last.fm) [data](http://www.last.fm/settings/dataexporter).
For those that don't use it, last.fm records all the music you listen to (as long as you listen on something that supports it) and records it for you.
It uses this data to give you recomendations of other artists you might like.
It also gives you weekly, monthly, 6 monthly lists of who you have been listening to.

If I go to a care home when I am old, I intend to put my last.fm profile in as part of my [care plan](http://www.nhs.uk/Planners/Yourhealth/Pages/Careplan.aspx).
I don't want to spend the last years of my life listening to cruddy music.

Anyway, you can now download all your data quite easily.
So here I will just have a play around and see what is interesting.
By the way, I am [t3h_m013](www.last.fm/user/t3h_m013) on last.fm.
This is an old and embaressing nickname... but oh well.


Read in the data
-----------------

First some libraries

```r
library(ggplot2)
library(magrittr)
library(dplyr)
library(lubridate)
library(RColorBrewer)

theme_set(theme_minimal())
knit_theme$set("solarized-light")
```

Then read in the data.


```r
d <- read.table('data/scrobbles.tsv')
```

```
## Error in scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings, : line 1091 did not have 15 elements
```

Ok. It doesn't work. Blerg. 

I've made a copy of the file and removed the offending line. I guess this will just move the error on to the next line with a similar problem.



```r
d <- read.table('data/scrobbles\ (copy).tsv')
```

```
## Error in scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings, : line 1096 did not have 15 elements
```

Ok it's to do with quoted text inside song names. 
"07 - Excepts From ""The Six Wives Of Henry VIII""" for example. 
Funny that I've never encountered this before.



```r
d <- read.table('data/scrobbles.tsv', header = TRUE, sep = '\t', stringsAsFactors = FALSE, quote = "\"")
dim(d)
```

```
## [1] 104571     15
```

```r
names(d)
```

```
##  [1] "ISO.time"                "unixtime"               
##  [3] "track.name"              "track.mbid"             
##  [5] "artist.name"             "artist.mbid"            
##  [7] "uncorrected.track.name"  "uncorrected.track.mbid" 
##  [9] "uncorrected.artist.name" "uncorrected.artist.mbid"
## [11] "album.name"              "album.mbid"             
## [13] "album.artist.name"       "album.artist.mbid"      
## [15] "application"
```

Hooray. It works. 
We have some fairly obvious column names.
Note that last.fm matches incorrectly named artists.
So "uncorrected*" are the original data.

So let's look at some basic overview stuff.


```r
artistData <- data.frame(table(d$artist.name))
names(artistData)[1] <- 'Artist'

artistData[order(artistData$Freq, decreasing = TRUE), ] %>% head
```

```
##                  Artist Freq
## 1926       Van Morrison 5707
## 675        Frank Turner 2789
## 1356          Radiohead 2743
## 1651        The Beatles 2296
## 365  Coheed and Cambria 1819
## 1496          Sigur Rós 1734
```

```r
ggplot(artistData, aes(x = Freq)) + 
  geom_density() 
```

![plot of chunk someBasics](figure/someBasics-1.png) 

Ok, the top artists match.

![screenshot](figure/topArtistsScreen.png)


And as expected, there's a few artists with loads of listen, and lots of artists with very few listens.

Now to think of some interesting things to look at.



When do I listen to music?
---------------------------



```r
# convert to POSIXct
d$time <- ymd_hms(d$ISO.time)


# Through time
ggplot(d, aes(x = time)) +
  geom_density(adjust = 0.1) 
```

![plot of chunk times](figure/times-1.png) 

```r
# Another simple way to look at times
#   Can't work out how to do this in ggplot2. 
plot(d$time)
```

![plot of chunk times](figure/times-2.png) 

I've had some periods where my music player didn't support scrobbling and things like that. 
Seems I also just listened to less music back in 2005/2006. 
I probably listened to more CDs back then.

To give some overview, I was doing my undergraduate degree September 2006 - July 2010.
Then I spent 1 year working and travelling (I'm surprised you can't see a drop in listens there.
From 2012 I've been doing an MRes/PhD in London.



```r
# Get the time of day
d$timeOnly <- hour(d$time) + minute(d$time)/60


ggplot(d, aes(x = timeOnly)) +
  geom_density(adjust = 0.001) 
```

![plot of chunk timeofday](figure/timeofday-1.png) 

So I listen to music less at night (makes sense).
I also listen less in the evening.
Which I guess is me being either out or listening to music with other people and therefore not necessarily on my player.

That spike is I think artificial. 
But I can't think what it might be.


```r
table(d$timeOnly)[order(table(d$timeOnly), decreasing = TRUE)] %>% head
```

```
## 
## 10.3333333333333            14.95 15.5833333333333 14.7833333333333 
##              593              174              154              152 
##            15.15            14.45 
##              150              149
```

It seems the spike is 10:20 (above is decimal I think). 
I think this must be something server side at last.fm.



When did I start listening to bands?
-------------------------------------


So first I'll look at a cumulative graph of number of bands in my 'library'.
I expect there will be a quick ramp up (I already listened to plenty of bands before I started scrobbling).
I'm not sure what else I might see.
Perhaps a ramp up in about 2010.
In particular I started listening to more electonic music and importantly more compilations at that time.



```r
first <- d %>% 
  group_by(artist.name) %>% 
  select(time) %>%
  summarise(first = sort(time)[1])

first %>% head
```

```
## Source: local data frame [6 x 2]
## 
##            artist.name               first
## 1               *shels 2007-11-01 14:36:20
## 2                  +44 2012-04-27 15:32:33
## 3              101/SiD 2006-01-17 22:49:38
## 4 13th Floor Elevators 2014-07-03 15:17:43
## 5                16bit 2010-10-27 12:43:01
## 6          2 Many DJ's 2005-03-08 12:44:25
```

```r
# Add this data to artistData data.frame
#   If someone knows how to do this better please tell me. (left_join?)
artistData$first <- first$first[sapply(as.character(artistData$Artist), function(x) which(x == first$artist.name))]


ggplot(artistData, aes(x = first)) +
  geom_density(adjust = 0.2)
```

![plot of chunk bandsCumulative](figure/bandsCumulative-1.png) 

```r
ggplot(artistData, aes(x = first)) + 
  stat_ecdf()
```

![plot of chunk bandsCumulative](figure/bandsCumulative-2.png) 

Well I guess that's pretty much just when I was scrobbling lots, I added new artists. 
I guess it's nice to know I'm still discovering new music and now just listening to bands from my youth.


When did I most listen to my favourite bands?
----------------------------------------------


```r
top <- artistData[order(artistData$Freq, decreasing = TRUE), ] %>% head(., 6)

topScrobbles <- d %>%
  filter(artist.name %in% top$Artist)

topScrobbles$artist.factor <- factor(topScrobbles$artist.name)

topScrobbles %<>% select(artist.factor, time)

brks <- data.frame(interval = seq(min(topScrobbles$time), max(topScrobbles$time), by = '4 month'))

ggplot(topScrobbles, aes(x = time, fill = artist.factor)) + 
  geom_histogram(breaks = as.numeric(brks[,1]), position = 'fill') +
  scale_fill_manual(values = brewer.pal(6, 'Set3'), name = "Artist") +
  ylab('Proportion') +
  xlab('')
```

```
## Warning: position_fill requires constant width: output may be incorrect
```

![plot of chunk topArtists](figure/topArtists-1.png) 

Meh. Still not a big fan of stacked bar chart type things. Mebbe I should try something else.



Getting into a band
---------------------

I notice with some bands that I get into them and absolutely binge.
Other bands I listen to them, then months later I finally get into them.
So let's look at that.


```r
# http://stackoverflow.com/questions/10836503/convert-difftime-time-to-years-months-and-days

f <- sapply(d$artist.name, function(x) which(artistData$Artist == x))

d$sinceFirst <- as.numeric(difftime( d$time , artistData$first[f], units = "days"))

ggplot(d, aes(x = sinceFirst)) + 
  geom_histogram(binwidth = 7) +
  xlab('Days')+
  scale_y_log10() +
  ylab('Log Count')
```

```
## Warning: Stacking not well defined when ymin != 0
```

![plot of chunk fromFirst](figure/fromFirst-1.png) 

```r
d %>% 
  filter(sinceFirst >= 1) %>%
  ggplot(., aes(x = sinceFirst)) + 
    geom_histogram(binwidth = 7) +
    xlab('Days')
```

![plot of chunk fromFirst](figure/fromFirst-2.png) 

```r
ggplot(d, aes(x = sinceFirst)) + 
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(0, 120)) +
  xlab('Days') +
  ggtitle('First four months') 
```

![plot of chunk fromFirst](figure/fromFirst-3.png) 

```r
d %>% 
  filter(sinceFirst >= 1) %>%
  ggplot(., aes(x = sinceFirst)) + 
    geom_histogram(binwidth = 1) +
    scale_x_continuous(limits = c(0, 120)) +
    xlab('Days') +
    ggtitle('First four months')
```

![plot of chunk fromFirst](figure/fromFirst-4.png) 

I don't know which of these is best. 
A lot of plays are on the first day I've ever listened to that band.
Spelling mistakes in band names could accentuate this.





Where do bands come from
---------------------------

Right. This requires some web scrapping. So it's time to learn `rvest`.

First I need to get URLs for the bands. Then download ALLL the data.


```r
library(rvest)
library(XML)
```


```r
artistData$urlencode <- sapply(artistData$Artist %>% as.character, URLencode)

artistData$lastfmurl <- paste0('http://www.last.fm/music/', artistData$urlencode, '/+wiki')


# This is very slow and probably pissing off last.fm. So save locations and load from now.
# wikiData <- lapply(artistData$lastfmurl, function(x) try(html(x))) 
```


```r
# 62 out of ~2000 artists failed.



sapply(wikiData, function(x) inherits(x, 'try-error')) %>% sum
```

```
## [1] 62
```

```r
# A function to pull out the location from the last.fm data.
extractLocation <- function(x){
  
  # We've still got the failed data in there. So return NAs for those.  
  if(inherits(x, 'try-error')){
    return(NA)
  }

  # If there is now wikiFractBox, give na.
  if(is.null(html_node(x, ".wikiFactBox:nth-child(1) .factbox"))){
    return(NA)
  }

  # Extract the 'factbox' using rvest and convert to character
  factbox <- x %>%
    html_node(".wikiFactBox:nth-child(1) .factbox") %>%
    saveXML 

  # If there isn't any location data then return NA as well.
  if(!grepl('Founded in', factbox)){
    return(NA)
  }
  
  st <- 'Founded in</dt>\n        <dd>\n            <ul><li>\n'  
  pat <- paste0('(.*', st, ')(.*?)(</li>.*$)')

  location <- gsub(pat, '\\2', factbox)
  
  # clean up
  location <- gsub('(^\ *)(.*)(\ *$)', '\\2', location)
  location <- gsub('(^.*)(\ \\n.*$)', '\\1', location)
  
  return(location)
}
  


# artistData$locations <- sapply(wikiData, extractLocation)

load('data/artistData.RData')

sum(!is.na(artistData$locations))
```

```
## [1] 862
```

```r
# save(artistData, file = 'data/artistData.RData')
```

Doing it like this, unfortunately we only find ~800 locations. 
This is still awesome.
But maybe I'll have a go at scrapping wikipedia at some point.
One thing to note is that on last.fm, if multiple bands have the same name they share a page.
So there is no location markup allowed for these bands.

Right. Now to convert locations to latitude and longitude.




```r
# Code from http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps
library(RCurl)
library(RJSONIO)

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address, verbose = FALSE) {

  if(is.na(address)){ return(c(NA, NA)) }

  if(verbose) cat(address, "\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(c(lat, lng))
  } else {
    return(c(NA, NA))
  }
}

# Run google geocode
geo <- lapply(artistData$locations, gGeoCode) %>% do.call(rbind, .)

head(geo)
```

```
##          [,1]      [,2]
## [1,]       NA        NA
## [2,]       NA        NA
## [3,] 30.26715 -97.74306
## [4,]       NA        NA
## [5,]       NA        NA
## [6,]       NA        NA
```

```r
colnames(geo) <- c('latitude', 'longitude')

artistData <- cbind(artistData, geo)
```

Now to plot! 



```r
library(leaflet)




blank = list(
  list(
    type = 'Feature',
    properties = list(
      popup = 'blnk'
    ),
    geometry = list(
      type = 'MultiPoint',
      coordinates = NA
    )
  )
)

s <- artistData[!is.na(artistData$latitude), c('Artist', 'longitude', 'latitude')]

shapes <- rep(blank, nrow(s))

for(i in 1:nrow(s)){
  #shapes[[i]]$geometry$coordinates <- matrix(rnorm(2), ncol = 2) 
  shapes[[i]]$geometry$coordinates <- as.matrix(s[i, 2:3]) 
  rownames(shapes[[i]]$geometry$coordinates) <- NULL
  colnames(shapes[[i]]$geometry$coordinates) <- NULL
  shapes[[i]]$properties$popup = as.character(s[i, 1])
}

# Commented because I can't work out how to output this properly yet.
#leaflet() %>% 
#  addTiles() %>%
#  addGeoJSON(shapes)
```


This is awesome.
However, it is confusing working out how to then share this map.
You can find it here [http://timcdlucas.github.io/lastfm/testleaflet.html](http://timcdlucas.github.io/lastfm/testleaflet.html).
And I guess at some point I'll work out how to auto update that.

It looks like this.
![A map of artists](figure/leafletmap.png)



What devices?
--------------

Apparently last.fm stores which device I was using when I listened to track.
I don't quite know what data they are storing.
But some interesting questions might be whether I listen to different music on my phone compared to on my computer.


```r
table(d$application)
```

```
## 
##                               Android Last.fm             Clementine 
##                  66268                  17244                   7805 
##            Grooveshark Last.fm Flash Clienttt                MUZU.TV 
##                  13232                      7                      3 
##     Spotify Web Player          YouTube Video 
##                     10                      2
```

Ok. Most of the data is from an unknown application.
However, given that Android seems to be working I would guess that nearly all the unknown stuff is from my computer.
So I will compare `android last.fm` to everything else.


```r
phone <- d %>% 
  filter(application == 'Android Last.fm') %>%
  select(artist.name) %>%
  table %>%
  sort(decreasing = TRUE) %>%
  head(n = 10)



notPhone <- d %>% 
  filter(application != 'Android Last.fm') %>%
  select(artist.name) %>%
  table %>%
  sort(decreasing = TRUE) %>%
  head(n = 10)

phone
```

```
## .
##     Frank Turner     Van Morrison Four Year Strong  New Found Glory 
##             1276             1034              648              496 
##   Set Your Goals        Radiohead     Jack Johnson      John Martyn 
##              449              447              405              380 
## The Story So Far          Genesis 
##              356              343
```

```r
notPhone
```

```
## .
##       Van Morrison          Radiohead        The Beatles 
##               4673               2296               2256 
##          Sigur Rós Coheed and Cambria       Frank Turner 
##               1734               1690               1513 
##   Four Year Strong               Blur             Mogwai 
##                983                955                865 
##    New Found Glory 
##                850
```


Well... given that there's a bit of randomness and a bit of time bias (I got my first smart phone in about 2010) this is really quite similar.
Boring.

Diversity of bands listened to
--------------------------------






