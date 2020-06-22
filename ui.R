library(shinyjs)
library(plotly)

navbarPage("TELLme Dynamics/Perspective", 
           id = "nav",
           tabPanel("Map",
                    div(class = "outer",
                        useShinyjs(),
                        tags$head(
                          tags$link(rel = "stylesheet",
                                    type = "text/css",
                                    href = "css/style.css")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leaflet::leafletOutput("map", width = "100%", height = "100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Filter metropolis:"),
                                      selectizeInput('cities',
                                                     label = 'Metropolis', 
                                                     choices = dt_Metropolis['cityLabel'],
                                                     multiple = FALSE,
                                                     selected = dt_Metropolis[1,]['cityLabel']
                                      ),
                                      selectizeInput('semPackage',
                                                     label = 'Semantic Package',
                                                     choices = dt_Metropolis['cityLabel'], # listAlbum,
                                                     multiple = FALSE,
                                                     selected = dt_Metropolis[1,]['cityLabel']
                                      )
                        )
                    )
           )
)
