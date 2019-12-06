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
surround <- wa %>% filter(!(PUMACE10 %in% city$PUMA5CE))

## merge neighborhoods in city with pumas in surround
# keep state, name, lowest geoid, puma, tract, domain
domain <- st_sf( regional_name = c(paste('Seattle--',as.character(city$NEIGHBO),sep=''), as.character(surround$NAME10)),
                 STATE = 53,
                 GEOID = c(rep('NA',nrow(city)), paste('53',surround$residence_puma,sep='')),
                 domain = 'SFS_year2',
                 geometry = c(city$geometry,surround$geometry)
               )

# zip maps
zipsAll <- zctas(cb=TRUE, starts_with = '98')
zipsAll <- st_transform(st_as_sf(zipsAll),4326)

dat<- read_excel('Zip_Codes_SFS-Y2_09192019.xlsx', sheet='Full List+Map')
zips <- zipsAll[zipsAll$ZCTA5CE10 %in% as.character(dat$`Zip Codes`),]
zips <- st_transform(st_as_sf(zips),4326)

centers <- st_coordinates(st_centroid(zips))
colnames(centers) <- c('lon','lat')
zips <- cbind(zips,centers)

geojson_write(zips, geometry = "polygon", file = 'sfs_domain_zipcodes.geojson')


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

# filter 1 from tacoma that barely touchs and 
# filter 1 from Snohomish that overlaps a zip with no residents (it only covers an airport)
ex1$regional_name
ex2 <- ex1[-c(21,15),]
ex2$regional_name <- as.character(ex2$regional_name)


# clip rural pumas
ex2[15,] <- st_intersection(ex2[15,],st_union(zips))
# ex2[15,]$regional_name <- "King County (Southeast)--Maple Valley & Covington"

# 6 needs to filtered to major component only
tmp <- st_intersection(ex2[16,],st_union(zips))
geoms <- lapply( tmp$geometry, `[` )
tmp2<-as.data.frame(ex2[16,])
st_geometry(tmp2) <-st_sfc(geoms[[1]][[14]])
ex2[16,] <- tmp2
# ex2[16,]$regional_name <- "King County (Northeast)--Cottage Lake, Union Hill & Novelty Hill"

# centroids
centers <- st_coordinates(st_centroid(ex2))

# adjust centroids of 3 that are mostly rural to be closer to population-weighted
ex2$regional_name
centers[14,] <- c(-122.045,47.540)
centers[15,] <- c(-122.06,47.36)
centers[32,] <- c(-122.25,47.3)

# adjust centroid of 1 including vashon to be closer to population-weighted
centers[17,] <- c(-122.33,47.31)

colnames(centers) <- c('lon','lat')
ex3 <- cbind(ex2,centers)

leaflet() %>% 
  addTiles() %>% 
  # addPolygons(data = domain) %>%
  # addPolygons(data = st_union(zips), fillOpacity = 0, color = "red") %>%
  addPolygons(data = ex2, fillOpacity = 0, color = "green") %>%
  addMarkers( data = ex3, lng=ex3$lon, lat=ex3$lat) 


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


