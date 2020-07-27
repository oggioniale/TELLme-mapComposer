function(input, output, session) {
  RV <- reactiveValues(
    selectedMetropolis = '',
    selectedPackageID = '',
    issueName = '',
    scale = '',
    dynamicName = '',
    dynamicConceptCount = '',
    metropolisHasDynamics = FALSE,
    selectedDynamicID = '',
    perspectives = list(),
    bean = '',
    beanLayers = ''
  )
  
  observeEvent(input$cities,{
    if(input$cities != 'All metropolis'){
      RV$selectedMetropolis <- input$cities
    } 
    else {
      RV$selectedMetropolis <- ''
    }
  })
  
  observeEvent(RV$selectedMetropolis,{
    if (RV$selectedMetropolis == '') {
      shinyjs::disable('dynamics')
      updateSelectizeInput(
        session,
        'dynamics',
        choices = list('Select dynamics' = ""),
        server = TRUE
      )
    } 
    else {
      shinyjs::enable('dynamics')
      b <- glossary$dynamicsByMetropolisName(RV$selectedMetropolis)
      updateSelectizeInput(
        session,
        'dynamics',
        choices = glossary$listForDynamicsSelectize(b),
        server = TRUE,
        selected = NULL
      )
      RV$metropolisHasDynamics <- (b %>% as_tibble() %>% count() %>% pull() > 0)
    }
  })
  
  observeEvent(input$dynamics,{
    if(input$dynamics != '' && input$dynamics != 'Select dynamics'){
      RV$selectedDynamicID <- input$dynamics
    } 
    else {
      RV$selectedDynamicID <- ''
    }
  })
  
  observeEvent(RV$selectedDynamicID, {
    if(RV$selectedDynamicID == ""){
      RV$selectedPackageID <- ""
      RV$issueName <- ""
      RV$scale <- ""
      RV$dynamicName <- ""
      RV$dynamicConceptCount <- ""
      RV$bean <- ""
      RV$beanLayers <- ""
    }
    else {
      record <- glossary$mm2mm_DynamicsSemanticPackagesIssues() %>%
        dplyr::filter(dynamic_id == !!RV$selectedDynamicID) %>% 
        as_tibble()
      RV$selectedPackageID <- record %>% dplyr::select(package_id) %>% pull()
      RV$issueName <- record %>% dplyr::select(issue_title) %>% pull()
      RV$scale <- record %>% dplyr::select(package_scale) %>% pull()
      RV$dynamicName <- record %>% dplyr::select(dynamic_title) %>% pull()
      RV$dynamicConceptCount <- record %>% dplyr::select(dynamic_conceptCount) %>% pull()
      RV$bean <- glossary$beanWithPerspectivesByDynamicId_tibble(RV$selectedDynamicID)
      RV$beanLayers <- hub$layersInBean(
        RV$bean,
        scale = RV$scale
      )
    }
  })
  
  output$textRecord <- renderUI({
    HTML(paste0(
      "<hr><h4>Metropolis:</h4><h4>",
      RV$selectedMetropolis,
      "</h4><br><h4>Issue:</h4><h4>",
      RV$issueName,
      "</h4><br><h4>Scale:</h4><h4>",
      RV$scale,
      "</h4><br><h4>Dynamic:</h4><h4>",
      RV$dynamicName,
      "</h4>"
    ))
  })
  
  output$warning <- renderUI({
    messageTellMe = ''
    # if (RV$dynamicConceptCount == '0') {
    #   messageTellMe = "No semantic package"
    # }
    if (!RV$metropolisHasDynamics) {
      messageTellMe = "No dynamics available for the selected metropolis"
    }
    HTML(paste0(
      "<hr><h4>",
      messageTellMe,
      "</h4>"
    ))
  })
  
  output$bean <- renderUI({
    req(RV$bean)
    #add progress here
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Loading semantic package from TELLme Glossary software...", value = 0)

    gg <- RV$bean %>%
      dplyr::group_by(keyword_id, keyword_title) %>%
      tidyr::nest()
    # print(gg)
    htmlDivTellmePanelContainer<-htmltools::tags$div(id="tellme_panelContainer")
    htmlUlKeywords <- htmltools::tags$ul(
      id = "ul_tellme_semantics"
    )
    htmlUlConcept <- htmltools::tags$ul(
      class = "ul_tellme_concepts"
    )
    for (i in 1:nrow(gg)) {
      for (j in 1:nrow(gg[i,]$data[[1]])) {
        htmlUlConcept <- htmltools::tagAppendChildren(
          htmlUlConcept,
          htmltools::tags$li(
            id = gg[i,]$data[[1]][j,]$concept_id,
            class =
              if (gg[i,]$data[[1]][j,]$is_selected == 0) {
                beanConceptLiClass = "conceptToggle"
              } else {
                beanConceptLiClass = "conceptToggle active"
              },
            htmltools::tags$a(
              href = paste0("http://rdfdata.get-it.it/TELLmeGlossary/concept_", gg[i,]$data[[1]][j,]$concept_id),
              target = "blank",
              gg[i,]$data[[1]][j,]$concept_title
            )
          )
        )
        # htmlUlConcept
      }
      htmlUlKeywords <- htmltools::tagAppendChildren(
        htmlUlKeywords,
        htmltools::tags$li(
          id = gg$keyword_id[i],
          class = "li_tellme_keyword",
          htmltools::tags$a(
            href = paste0("http://rdfdata.get-it.it/TELLmeGlossary/keyword_", gg$keyword_id[i]),
            target = "blank",
            gg$keyword_title[i]
          )
        ),
        htmlUlConcept
      )
      htmlUlConcept <- htmltools::tags$ul(
        class = "ul_tellme_concepts"
      )
    }
    htmlDivTellmePanelContainer<-htmltools::tagAppendChild(
      htmlDivTellmePanelContainer,
      htmlUlKeywords
    )
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
    
    leaflet::leaflet() %>%
      leaflet::addTiles()
  })
  
  ## Interactive Map ##########################################
  # Create the map
  observeEvent(RV$selectedMetropolis, {
    if (RV$selectedMetropolis != '') {
      # fitBound of city selected
      RV$beanLayers
      selectedCityPolygon <- metropolisPolygons %>% dplyr::filter(tellmeCityLabel == !!RV$selectedMetropolis)
      bbox <- sf::st_bbox(selectedCityPolygon) %>% as.vector()
      leaflet::leafletProxy("map", session) %>% 
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
    else {
      tellmeIcons <- leaflet::makeIcon(
        iconUrl = 'www/icons/logo-tellme-1.jpg',
        iconWidth = 18, iconHeight = 18
      )
      leaflet::leafletProxy("map", session) %>%
        leaflet::clearShapes() %>% 
        leaflet::addTiles() %>%
        leaflet::setView(0, 30, zoom = 3) %>%
        leaflet::addLayersControl(position = 'bottomright',
                                  overlayGroups = c('Metropolis')#,
                                  # options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addMarkers(
          data = citiesGPS,
          popup = paste0(
            "City: <b>", citiesGPS$cityLabel, "</b><br/>",
            "<a href = '", citiesGPS$city, "'>Wikidata link</a>"
          ),
          icon = tellmeIcons,
          group = 'Metropolis'
        )
    }
  })
}