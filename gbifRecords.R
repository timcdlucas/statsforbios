

x <- occ(query = 'Anopheles plumbeus', geometry = c(-10, 10, 45, 65), from = 'gbif', limit=1000)
y <- gbif(genus = 'Anopheles', species = 'plumbeus', ext =  c(-10, 10, 45, 65))
x$gbif$meta$found
nrow(y)
