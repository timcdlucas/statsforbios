
# Making a nice, mouseover map of all 50 US states.

# Long version
require(googleVis)

G4 <- gvisGeoChart(CityPopularity, locationvar='City', colorvar='Popularity',
                   options=list(region='US', height=350, 
                                displayMode='markers',
                                colorAxis="{values:[200,400,600,800],
                                 colors:[\'red', \'pink\', \'orange',\'green']}")
) 
plot(G4)




# <140 char version
plot(googleVis::gvisGeoChart(CityPopularity, locationvar='City', colorvar='Popularity',options=list(region='US', displayMode='markers')))
