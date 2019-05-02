library(tigris)
library(geojsonio)
library(sf)

# finding binding of pumas and tracts
# https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Equivalency_Format_Layout.pdf?#
# https://www2.census.gov/geo/docs/reference/puma/PUMSEQ10_53.txt

  pumaToTract <- readLines('PUMSEQ10_53.txt')
  
  pumaToTract <- list(PUMACE10 = substr(pumaToTract, 14, 18),
                      GEOID = paste(substr(pumaToTract, 4, 5),substr(pumaToTract, 19, 21),substr(pumaToTract, 56, 61),sep=""))
  
  idx <- !grepl('      ',pumaToTract$GEOID) # only tracts
  pumaToTract$GEOID <- pumaToTract$GEOID[idx]
  pumaToTract$PUMACE10 <- pumaToTract$PUMACE10[idx]
  
  # since there are duplicated GEOID for some reason, check if all duplicates have same PUMA 
  p<-list(PUMACE10 = list(), GEOID = unique(pumaToTract$GEOID))
  for (g in unique(pumaToTract$GEOID)){
    idx<-pumaToTract$GEOID == g
    p$PUMACE10[p$GEOID == g]<-length(unique(pumaToTract$PUMACE10[idx]))
  }
  print(unique(unlist(p$PUMACE10)) == 1)
  # all good to remove duplicates
  
  idx <- !duplicated(pumaToTract$GEOID)
  pumaToTract$GEOID <- pumaToTract$GEOID[idx]
  pumaToTract$PUMACE10 <- pumaToTract$PUMACE10[idx]

  
# wa shapes
  t <- tracts("WA")
  p <- pumas("WA")  

# add puma label to tracts  
  t$PUMACE10 <- pumaToTract$PUMACE10[match(t$GEOID, pumaToTract$GEOID)]

# write
  geojson_write(t, geometry = "polygon", file = '2016_wa_censusTracts.geojson')
    

# king county pumas  
  p <- st_as_sf(p)
  sum(kingIdx <- (regexpr("Seattle",p$NAMELSAD10)>-1) | (regexpr("King",p$NAMELSAD10)>-1))
  geojson_write(p[kingIdx,], geometry = "polygon", file = '2016_king_county_pumas.geojson')
  