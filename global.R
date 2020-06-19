library(dplyr)
# library(WikidataQueryServiceR)
library(leaflet.extras)

###
# metropolis
###
# probably this website could be a good started point https://www.metropolis.org/members-list
# the list of metropolis is taken from wikidata
# WikiData query for obtain the list of the cities with millions of inhabitants (https://www.wikidata.org/wiki/Q1637706)
tableMetropolisWikiData <- WikidataQueryServiceR::query_wikidata(
  # add population
  'SELECT DISTINCT ?city ?cityLabel ?gps ?OSM_relation_ID ?population WHERE {
  #?city wdt:P31 wd:Q1637706;
  ?city wdt:P31 wd:Q515;
    wdt:P625 ?gps;
    wdt:P402 ?OSM_relation_ID;
    wdt:P1082 ?population.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}'
)

osmdata::set_overpass_url("https://overpass-api.de/api/interpreter")

  # queryCityBoundary <- '
  # [out:json][timeout:25];
  # (
  #   relation(1224652);
  # );
  # out body;
  # >;
  # out skel qt;'
  # 
  # cityBoundary <- osmdata_sf(queryCityBoundary)
  # names(cityBoundary$osm_multipolygons$geometry) <- NULL
  # cityBoundary$osm_multipolygons %>%
  #   head(1) %>%
  #   leaflet %>%
  #   addTiles %>%
  #   addPolygons()
  

  
cityBoundaryResult <- osmdata::opq_osm_id(id = 1224652, type = 'relation', open_url = FALSE) %>%
  osmdata::opq_string() %>%
  osmdata::osmdata_sf()

names(cityBoundaryResult$osm_multipolygons$geometry[[1]][[1]]) <- NULL

tableMetropolis <- tableMetropolisWikiData %>% filter(cityLabel %in% c('Buenos Aires', 'Guadalajara', 'Seville', 'Milan'))
tableMetropolis <- tableMetropolis %>% 
  add_row(city = '', cityLabel = 'All metropolis', gps = 'Point(0, 0)', OSM_relation_ID = 0, .before = 1) %>% 
  add_row(city = 'https://www.wikidata.org/wiki/Q1010', cityLabel = 'Maribor', gps = 'Point(46.55472 15.64667)', OSM_relation_ID = 8154489) %>% 
  add_row(city = 'https://www.wikidata.org/wiki/Q1489', cityLabel = 'Mexico City', gps = 'Point(19.42847 -99.12766)', OSM_relation_ID = 1376330)

leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addPolygons(
    data = cityBoundaryResult$osm_multipolygons, 
    group = 'metropolis',
    weight = 2,
    col = 'red', 
    fillColor = 'red')

