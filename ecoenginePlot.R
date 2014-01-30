
# Install dev version of ecoengine
library(devtools)
install_github("ropensci/ecoengine")


# Load eco engine
library(ecoengine)

# Get us some data
d <- ee_observations(genus = "otonycteris", page = "all", georeferenced = TRUE)

# plot
ee_map(d)
