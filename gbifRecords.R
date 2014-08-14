
# limit
bounds <- c(-125.0, 38.4, -121.8, 40.9)
occ(query = 'Danaus plexippus', from="inat", limit=250, geometry=bounds) # hangs
occ(query = 'Danaus plexippus', from="inat", limit=150, geometry=bounds) # works



x <- occ(query = 'Anopheles plumbeus', geometry = c(-10, 10, 45, 65), from = 'gbif', limit=1000)
y <- gbif(genus = 'Anopheles', species = 'plumbeus', ext =  c(-10, 10, 45, 65))
x$gbif$meta$found
nrow(y)





