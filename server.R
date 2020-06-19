function(input, output, session) {

  ## Interactive Map ##########################################
  
  output$map <- leaflet::renderLeaflet({
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Please wait", value = 0)
    
    map <- leaflet::leaflet() %>% 
      leaflet::addTiles() %>% 
      leaflet::addLayersControl(position = 'bottomright',
                                #overlayGroups = c("Cities"),
                                options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    
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
