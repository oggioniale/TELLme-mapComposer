# libraries
library("rjson")

jsonLayersGeoServer <- "http://140.164.11.125:8080/geoserver/rest/layers.json"
jsonLayersGetIt <- "http://tellmehub.get-it.it/api/layers/"
jsonGeoServerData <- fromJSON(file = jsonLayersGeoserver)
jsonGetItData <- fromJSON(file = jsonLayersGetIt)

for (i in length(jsonGetItData$objects)) {
  n <- list()
  n[i] <- jsonGetItData$objects[[i]]$detail_url
}


# ui
ui <- fluidPage(
  
  titlePanel("Related concepts selector"),
  
  fluidRow(
    sidebarPanel(
      uiOutput("select.concepts")
    )
  )
)
# server
server <- function(input, output) {
  
  root <- '~'
  
  output$select.concepts <-
    renderUI(expr = selectInput(inputId = 'concepts.lable',
                                label = 'Related concenpts',
                                choices = list.dirs(path = root,
                                                    full.names = FALSE,
                                                    recursive = FALSE)))
}

# app
shinyApp(ui = ui, server = server)