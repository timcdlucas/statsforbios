library(textcat)
library(stringi)
library(ggplot2)

setwd('/home/tim/Dropbox/Documents/statsforbios/worldCup/')

load('AllTweets.rdata')

# Look at character encoding and recode everything as UTF-8
table(stri_enc_mark(tot$text))

tot <- tot[stri_enc_isutf8(tot$text),] # remove a couple of weird encodings.

tot$text <- stri_encode(tot$text, "", "UTF-8") #convert

table(stri_enc_mark(tot$text)) # check conversion







tot <- cbind(tot, textcat(tot$text))

names(tot) <- c(names(tot)[1:16], 'language')

save(tot, file='/home/tim/Dropbox/Documents/statsforbios/worldCup/AllTweets.rdata')




table(tot$language)

# Tidy up some british language. These are mostly English despite what they say.
# Obviously my poor knowledge of other languages will prevent me doing much about similar issues.
tot[which(tot$language=='scots'),'language'] <- 'english'
tot[which(tot$language=='welsh'),'language'] <- 'english'
tot[which(tot$language=='scots_gaelic'),'language'] <- 'english'
tot[which(tot$language=='irish'),'language'] <- 'english'
tot[which(tot$language=='manx'),'language'] <- 'english'

save(tot, file='/home/tim/Dropbox/Documents/statsforbios/worldCup/AllTweets.rdata')



##########################################################################################################

fLocations <- grep('fuck', tot$text)

table(tot$language[fLocations])

fTimes <- tot$created[fLocations]



fTime.str <- as.numeric(format(fTimes, "%H")) +  as.numeric(format(fTimes, "%M"))/60

plot(density(fTime.str, adjust=0.2))
density(fTimes)




