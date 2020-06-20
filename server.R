function(input, output, session) {

  ## Interactive Map ##########################################
  # Create the map
  observeEvent(input$cities, {
    if (input$cities == 'All metropolis') {
      shinyjs::disable('semPackage')
      updateSelectizeInput(
        session,
        'semPackage',
        choices = dt_Metropolis[1,]['cityLabel'],
        server = TRUE
      )
    } else if (input$cities != 'All metropolis') {
      shinyjs::enable('semPackage')
      updateSelectizeInput(
        session,
        'semPackage',
        choices = dt_Metropolis[1,]['cityLabel'],
        server = TRUE
      )
    }
  })
  
  output$map <- leaflet::renderLeaflet({
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Please wait", value = 0)
    
    # add icons to the map
    tellmeIcons <- leaflet::makeIcon(
      iconUrl = 'www/icons/logo-tellme-1.jpg',
      iconWidth = 18, iconHeight = 18
    )
    
    map <- leaflet::leaflet(citiesGPS) %>%
      leaflet::addTiles() %>%
      leaflet::setView(0, 0, 2) %>%
      leaflet::addMarkers(
        popup = paste0(
          "City: <b>", citiesGPS$cityLabel, "</b><br/>",
          "<a href = '", citiesGPS$city, "'>Wikidata link</a>"
        ),
        icon = tellmeIcons,
        group = 'Metropolis'
      ) %>% 
      leaflet::addLayersControl(position = 'bottomright',
                                overlayGroups = c('Metropolis'),
                                options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>% 
      leaflet::addPolygons(
        data = metropolis, 
        group = 'Metropolis',
        weight = 2,
        col = 'red', 
        fillColor = 'red')
    
    # fitBound of phtos selected
    # observeEvent(input$cities, {
    #   maxLong = max(selectedCities()$GPSLongitude)
    #   maxLat = max(selectedCities()$GPSLatitude)
    #   minLong = min(selectedCities()$GPSLongitude)
    #   minLat = min(selectedCities()$GPSLatitude)
    #   mapProxy <- leaflet::leafletProxy("map")
    #   mapProxy %>%
    #     leaflet::fitBounds(minLong, minLat, maxLong, maxLat)
    # })
    return(map)
  })
}
