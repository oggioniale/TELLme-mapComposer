library(dplyr)
#library(tidyverse)
library(sf)

tryCatch(source("_private.settings.R"), error=function(e){warning("not found _private.settings.R file. Looking for environment variables...")})
# if you run the application locally,
# you can create your own use _private.settings.R 
# and create there the required environment variable usually passed to a docker-container 
# with an "env" file.
# Required variables are: 
# GETIT_ADMIN_USER, GETIT_ADMIN_PASSWORD, GEOSERVER_ADMIN_USER, GEOSERVER_ADMIN_PASSWORD)
# with Sys.setenv function
# in Docker you should pass these variables with env-file parameters when running the container

### account TELLme MGIP Glossary software
# this is needed to obtain read permission on the json dump of the glossary
# and to retrieve further information (namely: dynamics and perspectives) 
# which are not exposed through json dump
TELLME_GLOSSARY_PASSWORD<-Sys.getenv("TELLME_GLOSSARY_PASSWORD")
TELLME_GLOSSARY_USER<-Sys.getenv("TELLME_GLOSSARY_USER")
TELLME_GLOSSARY_URL<-Sys.getenv("TELLME_GLOSSARY_URL", unset="http://www.tellme.polimi.it/tellme_apps/tellme/export")
# note: the endpoint needs authentication
TELLME_GLOSSARY_READONLINE<-as.logical(Sys.getenv("TELLME_GLOSSARY_READONLINE", unset=FALSE))


### account TELLme Hub
# even if the app does not write to TELLme-HUB,
# in order to retrieve remote services (WMS) urls from tellmehub an administrative account is needed
TELLME_HUB_USER<-Sys.getenv("TELLME_HUB_USER")
TELLME_HUB_PASSWORD<-Sys.getenv("TELLME_HUB_PASSWORD")
TELLMEHUB_URL<-Sys.getenv("TELLMEHUB_URL", unset="http://tellmehub.get-it.it/")
TELLME_HUB_READONLINE<-as.logical(Sys.getenv("TELLME_GLOSSARY_READONLINE", unset=FALSE))


dt_Metropolis<-readRDS("cityGeojson/dt_Metropolis.rds")
citiesGPS<-sf::st_read("cityGeojson/citiesGPS.gpkg")
metropolisPolygons  <- sf::st_read("cityGeojson/metropolis2.gpkg")

warning("read metropolis spatial data from files")

#metropolisPolygons %>% dplyr::filter(tellmeCityLabel == "Milan")

source("lazyLoadGlossary.R")
glossary<-getTELLmeGlossary(read_online=TELLME_GLOSSARY_READONLINE) 

source("TELLmeHub.R")
hub<-getTELLmeHub(
  read_online = TELLME_HUB_READONLINE, 
  TELLMEHUB_URL=TELLMEHUB_URL, 
  TELLME_HUB_USER = TELLME_HUB_USER, 
  TELLME_HUB_PASSWORD = TELLME_HUB_PASSWORD, 
  exportUtilsFunctions = TRUE,
  writeJJtoDisk = TRUE)


