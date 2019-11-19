library(tigris)
library(geojsonio)
library(sf)
library(tidyverse)
library(dbViewR)

## download shape data

# seattle neighborhoods
city <- masterSpatialDB(shape_level = 'neighborhood_district_name', source='seattle_geojson')

# all tracts
tracts <- masterSpatialDB(shape_level = 'census_tract', source='wa_geojson')

# all pumas
surround <- masterSpatialDB(shape_level = 'puma', source='wa_geojson')


surroun


## merge neighborhoods in city with pumas in surround
domain <- surround %>% filter()