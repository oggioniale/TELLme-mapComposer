library(dplyr)
#library(tidyverse)
library(sf)

source("accounts_private.R")

dt_Metropolis<-readRDS("cityGeojson/dt_Metropolis.rds")
citiesGPS<-sf::st_read("cityGeojson/citiesGPS.gpkg")
metropolisPolygons  <- sf::st_read("cityGeojson/metropolis2.gpkg")

warning("read metropolis spatial data from files")

#metropolisPolygons %>% dplyr::filter(tellmeCityLabel == "Milan")

source("lazyLoadGlossary.R")
glossary<-getTELLmeGlossary(read_online=TRUE) #set this to true in production

source("TELLmeHub.R")
hub<-getTELLmeHub(
  read_online = TRUE, #set this to true in production
  TELLMEHUB_URL=TELLMEHUB_URL, 
  TELLME_HUB_USER = TELLME_HUB_USER, 
  TELLME_HUB_PASSWORD = TELLME_HUB_PASSWORD, 
  exportUtilsFunctions = TRUE,
  writeJJtoDisk = FALSE)


