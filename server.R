function(input, output, session) {
  ## Leaflet Plugin
  layerTreePlugin <- htmltools::htmlDependency("Leaflet.LayerTreePlugin", "1.0.0",
                               src = c(href = "https://github.com/bambrikii/leaflet-layer-tree-plugin/find/master/src/"),
                               script = "leaflet-layer-tree-control.js"
  )
  
  registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }
  ##
  
  glossary<-getTELLmeGlossary(read_online=F) #set this to true in production
  
  hub<-getTELLmeHub(
    read_online = TRUE, #set this to true in production
    TELLMEHUB_URL=TELLMEHUB_URL, 
    TELLME_HUB_USER = TELLME_HUB_USER, 
    TELLME_HUB_PASSWORD = TELLME_HUB_PASSWORD, 
    exportUtilsFunctions = TRUE,
    writeJJtoDisk = FALSE)
  
  
  
  # output$selectionPrespectives <- renderUI({
  #   pippo <- glossary$listForPerspectivesSelectize(glossary$perspectivesByDynamicId(RV$selectedDynamicID))
  #   selectizeInput(
  #     inputId = 'perspectives',
  #     label = "Select one or more perspective of this dynamic:",
  #     choices = glossary$listForPerspectivesSelectize(RV$perspectives),
  #     multiple = TRUE,
  #     selected = NULL,
  #     options = list(
  #       plugins = list("remove_button")
  #     )
  #   )
  # })
  
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
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Loading data...", value = 0)
      
      shinyjs::disable("cities")
      shinyjs::disable("dynamics")
      record <- glossary$mm2mm_DynamicsSemanticPackagesIssues() %>%
        dplyr::filter(dynamic_id == !!RV$selectedDynamicID) %>% 
        as_tibble()
      RV$selectedPackageID <- record %>% dplyr::select(package_id) %>% pull()
      RV$issueName <- record %>% dplyr::select(issue_title) %>% pull()
      RV$scale <- record %>% dplyr::select(package_scale) %>% pull()
      RV$dynamicName <- record %>% dplyr::select(dynamic_title) %>% pull()
      RV$dynamicConceptCount <- record %>% dplyr::select(dynamic_conceptCount) %>% pull()
      RV$perspectives <- glossary$perspectivesByDynamicId(RV$selectedDynamicID)
      
      updateSelectizeInput(
        session,
        'perspectives',
        choices = glossary$listForPerspectivesSelectize(RV$perspectives)[[1]],
        #selected = RV$perspectives %>% pull(perspective_id),
        server = TRUE
      )
      
      RV$bean <- glossary$beanWithPerspectivesByDynamicId_tibble(RV$selectedDynamicID) %>% 
        mutate(is_selected=0)
      
      # #activePerspectives<-input$perspectives
      # # colsWithDesideredPerspectives <- c('concept_id', 
      # #                            'concept_title', 
      # #                            activePerspectivesColumns, 
      # #                            'is_selected',
      # #                            'keyword_id',
      # #                            'keyword_title')
      # # 
      # # update the bean: put selected perspectives in OR and update selected layers accordingly
      # RV$bean <- glossary$beanWithPerspectivesByDynamicId_tibble(RV$selectedDynamicID)
      # 
      # if(length(input$perspectives)>0){
      #   activePerspectivesColumns<-paste0('selectedByPerspective.',input$perspectives)
      #   warning("selected perspectives:", activePerspectivesColumns)
      #   # #dplyr::select_(.dots = colsWithDesideredPerspectives) 
      #   RV$bean <- RV$bean %>% 
      #     mutate(sum=rowSums(.[activePerspectivesColumns]))  %>% 
      #     mutate(is_selected=as.numeric(sum>0)) %>% dplyr::select(-sum)
      # }
      # 
      # RV$beanLayers <- hub$layersInBean(
      #   RV$bean,
      #   scale = RV$scale
      # )
        
      shinyjs::enable("cities")
      shinyjs::enable("dynamics")
    }
  })
  
  # when perspectives are chosen, the bean model is updated with the selected concepts, 
  # and the set of the available layers for the current selection is computed.
  observeEvent(input$perspectives,{
    #activePerspectives<-input$perspectives
    # colsWithDesideredPerspectives <- c('concept_id', 
    #                            'concept_title', 
    #                            activePerspectivesColumns, 
    #                            'is_selected',
    #                            'keyword_id',
    #                            'keyword_title')
    # 
    # update the bean: put selected perspectives in OR and update selected layers accordingly
    #RV$bean <- glossary$beanWithPerspectivesByDynamicId_tibble(RV$selectedDynamicID)
    
    if(length(input$perspectives)>0){
      activePerspectivesColumns<-paste0('selectedByPerspective.',input$perspectives)
      warning("selected perspectives:", activePerspectivesColumns)
      # #dplyr::select_(.dots = colsWithDesideredPerspectives) 
      RV$bean <- glossary$beanWithPerspectivesByDynamicId_tibble(RV$selectedDynamicID) %>% 
        mutate(sum=rowSums(.[activePerspectivesColumns]))  %>% 
        mutate(is_selected=as.numeric(sum>0)) %>% dplyr::select(-sum)
    }
    else{
      RV$bean <- glossary$beanWithPerspectivesByDynamicId_tibble(RV$selectedDynamicID) %>% mutate(is_selected=0)
    }
    
    RV$beanLayers <- hub$layersInBean(
      RV$bean %>% dplyr::filter(is_selected>0),
      scale = RV$scale
    )
  },
  ignoreNULL  = FALSE,
  ignoreInit = TRUE)
  
  {
  output$vbox1 <- renderValueBox({
    valueBox(
      value = tags$h2(RV$selectedMetropolis),
      subtitle = "Metropolis",
      color = "aqua"
    )
  })
  
  output$vbox2 <- renderValueBox({
    valueBox(
      value = tags$h2(RV$issueName),
      subtitle = "Issue",
      color = "aqua"
    )
  })
  
  output$vbox3 <- renderValueBox({
    valueBox(
      value = tags$h2(RV$scale),
      subtitle = "Scale",
      color = "aqua"
    )
  })
  
  output$vbox4 <- renderValueBox({
    valueBox(
      value = tags$h2(RV$dynamicName, style = "white-space: pre-wrap;"),
      subtitle = "Dynamic",
      color = "aqua"
    )
  })
  
  output$warning <- renderInfoBox({
    messageTellMe = ''
    # if (RV$dynamicConceptCount == '0') {
    #   messageTellMe = "No semantic package"
    # }
    if (!RV$metropolisHasDynamics) {
      messageTellMe = "No dynamics available for the selected metropolis"
    } 
    else {
      messageTellMe = ""
    }
    infoBox(
      "Info message", 
      messageTellMe, 
      icon = icon("bell"),
      color = "red", 
      fill = TRUE
    )
  })
  
  # output$warning <- renderUI({
  #   messageTellMe = ''
  #   # if (RV$dynamicConceptCount == '0') {
  #   #   messageTellMe = "No semantic package"
  #   # }
  #   if (!RV$metropolisHasDynamics) {
  #     messageTellMe = "No dynamics available for the selected metropolis"
  #   }
  #   HTML(paste0(
  #     "<hr><h4>",
  #     messageTellMe,
  #     "</h4>"
  #   ))
  # })
  }

  # when the bean model is updated, the bean panel is updated too.
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
  # Create and center the map on the highlighted selected metropolis polygon.
  observeEvent(RV$selectedMetropolis, {
    if (RV$selectedMetropolis != '') {
      # fitBound of city selected
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
                                  overlayGroups = c('Metropolis', 'Dynamic/Perspectives')#,
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
  
  # the set of the available layers for the current selection in the bean is shown
  observeEvent(RV$beanLayers, {
    #message(RV$beanLayers)
    if (typeof(RV$beanLayers)=="list" && dim(RV$beanLayers)[1] > 0 )  {
      # bbox <- sf::st_bbox(selectedCityPolygon) %>% as.vector()
      # leaflet::leafletProxy("map", session) %>% 
      #   leaflet::addWMSTiles(
      #     "http://140.164.11.125:8080/geoserver/tellme/wms",
      #     layers = ,
      #     options = WMSTileOptions(format = "image/png", transparent = T)
      #   ) #%>%
      #   # leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      
      #RV$beanLayers %>% dplyr::filter(is_selected>0)
      ows_urlsBean <- RV$beanLayers %>% dplyr::filter(is_selected>0) %>% dplyr::select(layer_ows_url) %>% pull()
      typenamesBean <- RV$beanLayers %>% dplyr::filter(is_selected>0) %>% dplyr::select(layer_typename) %>% pull()
      
      for (i in 1:length(typenamesBean)) {
        map <- leaflet::leafletProxy("map", session)
        map %>% leaflet::addWMSTiles(
          ows_urlsBean[i],
          layers = typenamesBean[i],
          options = WMSTileOptions(
            # styles = ,
            format = "image/png", 
            transparent = T
          ),
          group = "Dynamic/Perspectives"
        ) # %>% 
        # Layer Tree Plugin demo http://rawgit.com/bambrikii/leaflet-layer-tree-plugin/master/examples/basic-example.htm
        # some info about Leaflet integration in R
        # registerPlugin(layerTreePlugin) %>%
        #   onRender(...)
      }
    }
    else{
      map <- leaflet::leafletProxy("map", session)
      map %>% leaflet::clearGroup("Dynamic/Perspectives")
    }
  })
}