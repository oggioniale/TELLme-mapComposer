library(leaflet.extras)

TELLmeMapL <- leaflet() %>%
  addTiles() %>%
  addMouseCoordinates() %>%
  setView(lng = 9.32, lat = 45.32, zoom = 10) %>%
  addWMSTiles(
    "http://140.164.11.125:8080/geoserver/tellme/wms",
    layers = c(
      "tellme:Hillshade_milano",
      "tellme:Hansen_GFC-2018-v1.6_treecover2000_50N_000E",
      "tellme:floodMapGL_rp100y",
      "tellme:aree_di_degrado_poligonale",
      "tellme:administrative_boundaries",
      "tellme:Bacini_idrografici_secondari_0607",
      "tellme:HydroLAKES_polys_v10",
      "tellme:gis_osm_waterways_free_1_NO",
      "tellme:gis_osm_waterways_free_1_NE",
      "tellme:Rischio_areale",
      "tellme:gis_osm_roads_free_1_NO",
      "tellme:gis_osm_roads_free_1_NE",
      "tellme:gis_osm_railways_free_1_NO",
      "tellme:gis_osm_railways_free_1_NE",
      "tellme:global_oil_pipelines_7z9",
      "tellme:GLOBAL_NATURAL_GAS_PIPELINES_WM",
      "tellme:gis_osm_buildings_a_free_1_NO",
      "tellme:gis_osm_buildings_a_free_1_NE",
      "tellme:gis_osm_water_a_free_1_NO",
      "tellme:gis_osm_water_a_free_1_NE",
      "tellme:WDPA_Feb2019-shapefile-polygons",
      "tellme:airports",
      "tellme:punti_panoramici",
      "tellme:siti_archeologici_puntuali"
    ),
    options = WMSTileOptions(format = "image/png", transparent = T)
  ) 

TELLmeMapL

htmlwidgets::saveWidget(TELLmeMapL, "TELLmeMapL.html")
