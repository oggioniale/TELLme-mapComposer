# metropolis polygons preparation (exploiting OSM and Wikidata)
#
library(dplyr)

dt_Metropolis<-(function(res="none"){
  tryCatch({
    res<-readRDS("dt_Metropolis__.rds")
  }, 
  error=function(e){
    tableMetropolisWikiData <- WikidataQueryServiceR::query_wikidata(
      'SELECT DISTINCT ?city ?cityLabel ?gps ?OSM_relation_ID ?population WHERE {
      #?city wdt:P31 wd:Q1637706;
      ?city wdt:P31 wd:Q515;
      wdt:P625 ?gps;
      wdt:P402 ?OSM_relation_ID;
      wdt:P1082 ?population.
      SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
  }'
    )
    
    # TODO add views parameters, geometry
    dt_Metropolis <- tableMetropolisWikiData %>% filter(cityLabel %in% c('Buenos Aires', 'Guadalajara', 'Seville', 'Milan'))
    dt_Metropolis <- dt_Metropolis %>% 
      add_row(city = '', cityLabel = 'All metropolis', gps = 'Point(0 0)', OSM_relation_ID = 0, .before = 1) %>% 
      add_row(city = 'https://www.wikidata.org/wiki/Q1010', cityLabel = 'Maribor', gps = 'Point(15.64667 46.55472)', OSM_relation_ID = 8154489) %>%
      add_row(city = 'https://www.wikidata.org/wiki/Q1489', cityLabel = 'Mexico City', gps = 'Point(-99.12766 19.42847)', OSM_relation_ID = 1376330)
    
    saveRDS(dt_Metropolis,"dt_Metropolis.rds")
    res<<-dt_Metropolis
},
finally = return(res))
  })()


# if(F){
#   ###
#   # metropolis
#   ###
#   ### Cities points
#   # probably this website could be a good started point https://www.metropolis.org/members-list
#   # the list of metropolis is taken from wikidata
#   # WikiData query for obtain the list of the cities with millions of inhabitants (https://www.wikidata.org/wiki/Q1637706)
#   tableMetropolisWikiData <- WikidataQueryServiceR::query_wikidata(
#     'SELECT DISTINCT ?city ?cityLabel ?gps ?OSM_relation_ID ?population WHERE {
#     #?city wdt:P31 wd:Q1637706;
#     ?city wdt:P31 wd:Q515;
#       wdt:P625 ?gps;
#       wdt:P402 ?OSM_relation_ID;
#       wdt:P1082 ?population.
#     SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
#   }'
#   )
#   
#   # TODO add views parameters, geometry
#   dt_Metropolis <- tableMetropolisWikiData %>% filter(cityLabel %in% c('Buenos Aires', 'Guadalajara', 'Seville', 'Milan'))
#   dt_Metropolis <- dt_Metropolis %>% 
#     add_row(city = '', cityLabel = 'All metropolis', gps = 'Point(0 0)', OSM_relation_ID = 0, .before = 1) %>% 
#     add_row(city = 'https://www.wikidata.org/wiki/Q1010', cityLabel = 'Maribor', gps = 'Point(15.64667 46.55472)', OSM_relation_ID = 8154489) %>%
#     add_row(city = 'https://www.wikidata.org/wiki/Q1489', cityLabel = 'Mexico City', gps = 'Point(-99.12766 19.42847)', OSM_relation_ID = 1376330)
#   tibble::
#   saveRDS(dt_Metropolis,"cityGeojson/dt_Metropolis.rds")
#   rm(dt_Metropolis)
# }
#dt_Metropolis<-readRDS("cityGeojson/dt_Metropolis.rds")

citiesGPS<-(function(res="none"){
  tryCatch({
    res<-sf::st_read("citiesGPS.gpkg")
  }, 
  error=function(e){
    sf::st_as_sf(dt_Metropolis[c(2:7),] %>% as.data.frame(), wkt = 'gps')
    sf::st_crs(citiesGPS) = 4326
    
    sf::st_write(citiesGPS,"citiesGPS.gpkg")
    res<<-citiesGPS
  },
  finally = return(res))
})()

### City multipolygons
# if(F){
#   sf::st_read(
#     "cityGeojson/metropolis.gpkg"
#   )
#   metropolisPolygons$tellmeCityLabel <- c('Buenos Aires', 'Guadalajara', 'Maribor', 'Mexico City', 'Milan', 'Seville')
#   sf::st_write(metropolisPolygons,"cityGeojson/metropolis2.gpkg")
# }