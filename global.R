library(dplyr)
# library(WikidataQueryServiceR)

###
# metropolis
###
### Cities points
# probably this website could be a good started point https://www.metropolis.org/members-list
# the list of metropolis is taken from wikidata
# WikiData query for obtain the list of the cities with millions of inhabitants (https://www.wikidata.org/wiki/Q1637706)
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
  add_row(city = '', cityLabel = 'All metropolis', gps = 'Point(0, 0)', OSM_relation_ID = 0, .before = 1) %>% 
  add_row(city = 'https://www.wikidata.org/wiki/Q1010', cityLabel = 'Maribor', gps = 'Point(15.64667 46.55472)', OSM_relation_ID = 8154489) %>%
  add_row(city = 'https://www.wikidata.org/wiki/Q1489', cityLabel = 'Mexico City', gps = 'Point(-99.12766 19.42847)', OSM_relation_ID = 1376330)
citiesGPS <- sf::st_as_sf(dt_Metropolis, wkt = 'gps')
sf::st_crs(citiesGPS) = 4326

### City multipolygons
# osmdata::set_overpass_url("https://overpass-api.de/api/interpreter")
# cityBoundaryResultMilan <- osmdata::opq_osm_id(id = dt_Metropolis[1,4], type = 'relation', open_url = FALSE) %>%
#     osmdata::opq_string() %>%
#     osmdata::osmdata_sf()
# names(cityBoundaryResultMilan$osm_multipolygons$geometry[[1]][[1]]) <- NULL
# cityBoundaryResultBuenoAires <- osmdata::opq_osm_id(id = dt_Metropolis[1,4], type = 'relation', open_url = FALSE) %>%
#     osmdata::opq_string() %>%
#     osmdata::osmdata_sf()
# names(cityBoundaryResultBuenoAires$osm_multipolygons$geometry[[1]][[1]]) <- NULL
# cityBoundaryResultSeville <- osmdata::opq_osm_id(id = dt_Metropolis[1,4], type = 'relation', open_url = FALSE) %>%
#     osmdata::opq_string() %>%
#     osmdata::osmdata_sf()
# names(cityBoundaryResultSeville$osm_multipolygons$geometry[[1]][[1]]) <- NULL
# cityBoundaryResultGuadajara <- osmdata::opq_osm_id(id = dt_Metropolis[1,4], type = 'relation', open_url = FALSE) %>%
#     osmdata::opq_string() %>%
#     osmdata::osmdata_sf()
# names(cityBoundaryResultGuadajara$osm_multipolygons$geometry[[1]][[1]]) <- NULL
# cityBoundaryResultMaribor <- osmdata::opq_osm_id(id = dt_Metropolis[1,4], type = 'relation', open_url = FALSE) %>%
#     osmdata::opq_string() %>%
#     osmdata::osmdata_sf()
# names(cityBoundaryResultMaribor$osm_multipolygons$geometry[[1]][[1]]) <- NULL
# cityBoundaryResultMexicoCity <- osmdata::opq_osm_id(id = dt_Metropolis[1,4], type = 'relation', open_url = FALSE) %>%
#     osmdata::opq_string() %>%
#     osmdata::osmdata_sf()
# names(cityBoundaryResultMexicoCity$osm_multipolygons$geometry[[1]][[1]]) <- NULL
metropolis  <- sf::st_read(
  "cityGeojson/metropolis.shp")





