function(input, output, session) {
  RV <- reactiveValues(
    selectedMetropolisID = '',
    selectedPackageID = ''
  )
  
  ## Interactive Map ##########################################
  # Create the map
  observe({
    if (input$cities == 'All metropolis') {
      shinyjs::disable('dynamics')
      updateSelectizeInput(
        session,
        'dynamics',
        choices = 'Select dynamics',
        server = TRUE
      )
      
      # TODO
      # map <- leaflet::leafletProxy("map", session) %>%
      #   leaflet::clearBounds()
      
    } else if (input$cities != 'All metropolis') {
      shinyjs::enable('dynamics')
      b <- glossary$dynamicsByMetropolisName(input$cities)
      updateSelectizeInput(
        session,
        'dynamics',
        choices = glossary$listForDynamicsSelectize(b),
        server = TRUE,
        selected = NULL
      )
      
      output$textRecord <- renderUI({
        record <- glossary$mm2mm_DynamicsSemanticPackagesIssues() %>%
          as_tibble() %>%
          dplyr::filter(dynamic_id == input$dynamics)
        if (length(record) != 0) {
          HTML(paste0(
            "<hr><h4>Metropolis:</h4><h4>",
            record %>% dplyr::select(package_metropolis) %>% as.character(),
            "</h4><br><h4>Issue:</h4><h4>",
            record %>% dplyr::select(issue_title) %>% as.character(),
            "</h4><br><h4>Scale:</h4><h4>",
            record %>% dplyr::select(package_scale) %>% as.character(),
            "</h4><br><h4>Dynamic:</h4><h4>",
            record %>% dplyr::select(dynamic_title) %>% as.character(),
            "</h4>"
          ))
        } else {
          HTML(
            "<hr><h4>Metropolis:</h4><h4> - </h4><br><h4>Issue:</h4><h4> - </h4><br><h4>Scale:</h4><h4> - </h4><br><h4>Dynamic:</h4><h4> - </h4>"
          )
        }
      })
      
      # fitBound of city selected
      selectedCityPolygon <- metropolisPolygons %>% dplyr::filter(tellmeCityLabel == input$cities)
      bbox <- sf::st_bbox(selectedCityPolygon) %>% as.vector()
      map <- leaflet::leafletProxy("map", session) %>% 
        clearMarkers() %>%
        leaflet::addPolygons(
          data = selectedCityPolygon, 
          group = 'Metropolis',
          weight = 2,
          col = 'red', 
          fillColor = 'red'
        ) %>%
        leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
    }
  })
  
  output$map <- leaflet::renderLeaflet({
    # # Create a Progress object
    # progress <- shiny::Progress$new()
    # # Make sure it closes when we exit this reactive, even if there's an error
    # on.exit(progress$close())
    # progress$set(message = "Please wait", value = 0)
    
    # add icons to the map
    tellmeIcons <- leaflet::makeIcon(
      iconUrl = 'www/icons/logo-tellme-1.jpg',
      iconWidth = 18, iconHeight = 18
    )
    
    map <- leaflet::leaflet(citiesGPS) %>%
      leaflet::addTiles() %>%
      leaflet::setView(0, 30, zoom = 3) %>% 
      leaflet::addLayersControl(position = 'bottomright',
                                overlayGroups = c('Metropolis')#,
                                # options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
      leaflet::addMarkers(
        popup = paste0(
          "City: <b>", citiesGPS$cityLabel, "</b><br/>",
          "<a href = '", citiesGPS$city, "'>Wikidata link</a>"
        ),
        icon = tellmeIcons,
        group = 'Metropolis'
      )
    
    return(map)
  })
  
}
