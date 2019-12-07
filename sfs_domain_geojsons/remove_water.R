# remove water

library(geojsonio)
library(sf)
library(tidyverse)
library(dbViewR)
library(leaflet)


shp<-masterSpatialDB(source='sfs_domain_geojson', shape_level='regional_name')

p<-leaflet() %>% addTiles() 

st_erase = function(x, y) st_difference(x, (st_union(y))) # https://gis.stackexchange.com/questions/308119/function-in-r-to-get-difference-between-two-sets-of-polygons-comparable-in-speed

# remove waterbodies
water <- read_sf(dsn = 'C:/Users/mfamulare/git/seattle-geojson/Waterbodies_with_History_and_Jurisdictional_detail__wtrbdy_det_area')  %>% 
  filter(FEAT_TYPE %in% c("Lake or pond")) %>%
  # filter(SHAPE_Area >  3e7) %>%
  filter(NAME %in% c('Lake Washington','Lake Union','Lake Sammamish'))

tmp <- st_erase(shp, water) %>% select(names(shp)) 

p %>% addPolygons(data = tmp, fillOpacity = 0.2,color='red')

geojson_write(tmp, geometry = "polygon", file = '2016_wa_censusTracts+sfs_domain.geojson')