library(tigris)
library(geojsonio)
library(sf)
library(tidyverse)
library(dbViewR)
library(readxl)
library(leaflet)

## download shape data

# seattle neighborhoods
city <- masterSpatialDB(shape_level = 'neighborhood_district_name', source='seattle_geojson')

# all tracts
tracts <- masterSpatialDB(shape_level = 'census_tract', source='wa_geojson')
citytracts <- masterSpatialDB(shape_level = 'census_tract', source='seattle_geojson')

# wa pumas
wa <- masterSpatialDB(shape_level = 'puma', source='wa_geojson')

# surround pumas
surround <- wa %>% filter(!(PUMACE10 %in% city$residence_puma))

# zip maps
dat<- read_excel('Zip_Codes_SFS-Y2_09192019.xlsx', sheet='Full List+Map')
zips <- zctas(cb=TRUE, starts_with = '98')
zips <- zips[zips$ZCTA5CE10 %in% as.character(dat$`Zip Codes`),]
zips <- st_transform(st_as_sf(zips),4326)

centers <- st_coordinates(st_centroid(zips))
colnames(centers) <- c('lon','lat')
zips <- cbind(zips,centers)

geojson_write(zips, geometry = "polygon", file = 'sfs_domain_zipcodes.geojson')


## merge neighborhoods in city with pumas in surround
# keep state, name, lowest geoid, puma, tract, domain
domain <- st_sf( regional_name = c(paste('Seattle--',as.character(city$NEIGHBO),sep=''), as.character(surround$NAME10)),
                      STATE = 53,
                      GEOID = c(rep('NA',nrow(city)), paste('53',surround$residence_puma,sep='')),
                      domain = 'SFS_year2',
                      geometry = c(city$geometry,surround$geometry)
                     )


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = domain) %>%
  addPolygons(data = st_union(zips), fillOpacity = 0, color = "red") 


# intersect zips with domain
# http://rpubs.com/sogletr/sf-ops
x1 <- st_intersects(domain, st_union(zips))
x2 <- map_lgl(x1, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})
ex1 <- domain[x2,]

# filter 1 from tacoma that barely touchs
ex1$regional_name
ex2 <- ex1[-c(21),]

# centroids
centers <- st_coordinates(st_centroid(ex2))

# adjust centroids of 3 that are mostly rural to be closer to population-weighted
centers[15,1] <- -122.1
centers[16,] <- c(-122.08,47.38)
centers[17,] <- c(-122.04,47.71)

# adjust centroid of 1 including vashon to be closer to population-weighted
centers[18,] <- c(-122.33,47.31)

colnames(centers) <- c('lon','lat')
ex3 <- cbind(ex2,centers)


leaflet() %>% 
  addTiles() %>% 
  # addPolygons(data = domain) %>%
  addPolygons(data = st_union(zips), fillOpacity = 0, color = "red") %>%
  addPolygons(data = ex2, fillOpacity = 0, color = "green") %>%
  addMarkers( data = ex3, lng=ex3$lon, lat=ex3$lat)

ex3$regional_name <- droplevels(ex3$regional_name)

geojson_write(ex3, geometry = "polygon", file = 'sfs_domain_neighborhood+puma.geojson')


## lookup table from census tracts
tracts$regional_name <- 'NA'

# city
idx <- which(tracts$residence_census_tract %in% citytracts$residence_census_tract)
for (k in idx){
  tracts$regional_name[k] <- paste('Seattle--',citytracts$residence_neighborhood_district_name[citytracts$residence_census_tract %in% tracts$residence_census_tract[k]],sep='')
}

#surround
idx <- which(paste('53',tracts$residence_puma,sep='') %in% ex3$GEOID)
for (k in idx){
  tracts$regional_name[k] <- as.character(ex3$regional_name[ex3$GEOID %in% paste('53',tracts$residence_puma[k],sep='')])
}

setdiff( ex3$regional_name,unique(tracts$regional_name))
setdiff( unique(tracts$regional_name),ex3$regional_name)


centers <- st_coordinates(st_centroid(tracts))
colnames(centers) <- c('lon','lat')
tracts <- cbind(tracts,centers)

geojson_write(tracts, geometry = "polygon", file = '2016_wa_censusTracts+sfs_domain.geojson')


