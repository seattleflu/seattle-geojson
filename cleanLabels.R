# clean shapefile headers
library(geojsonio)

filesIn<-c('censusTracts','city','cra','neighborhoods')
filesOut <- c('census_tract','city','cra','neighborhood')

badHeaders<-c('CRA_NAM','NEIGHBO')
goodHeaders<-c('CRA_NAME','NEIGHBORHOOD_DISTRICT_NAME')

for (k in 1:length(filesIn)){
  filename<-paste('seattle_geojsons/2016_seattle_',filesIn[k],'.geojson',sep='')
  shp <- geojson_read(filename,method='local',what='sp')
  
  plot(sf::st_as_sf(shp))
  
  badIdx <- names(shp) %in% badHeaders
  
  goodIdx <- badHeaders %in% names(shp)
  
  names(shp)[badIdx] <-goodHeaders[goodIdx]
  
  filename<-paste('seattle_geojsons/2016_seattle_',filesOut[k],'.geojson',sep='')
  geojson_write(shp, file=filename, geometry = 'polygon')
  
  plot(sf::st_as_sf(geojson_read(filename,method='local',what='sp')))
}
