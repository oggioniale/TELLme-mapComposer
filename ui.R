#
# TELLme Erasmus Plus Project
# 
# Shiny app for dynamics and prespetive
#
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(leaflet)
library(dplyr)

apptitle="TELLme Erasmus+ Project - Dynamics and Perspectives map"
appdoi="https://doi.org/10.5281/zenodo.4108649"

panels.about_text<- shiny::tabPanel("About",
  h4("Description"),
  p("Compose maps of metropolitan dynamics according to the perspectives in the ",a("TELLme Glossary and MGIP tool.",href="http://www.tellme.polimi.it/tellme_apps/tellme/", target="_blank")),
  h4("Usage"), 
  div(style="font-size:smaller;",
      p("1. In the Metropolis panel select one of the Metropolis of the TELLme Project."),
      #div(class="leaflet-draw-toolbar leaflet-bar leaflet-draw-toolbar-top",
      #                a(style="float:left; margin-right:2px;", class="leaflet-draw-draw-rectangle")),
      p("2. Select the dynamic you are interested in. <span style=\"font-size:smaller;\">(Dynamics are retrieved dyrectly from the external system MGIP Glossary Software)</span> "),
      p("3. In the Semantic Package panel you can see the keywords and concepts associated to the metropolitan dynamic of your choice. "),
      p("4. Select, if available, one or more perspectives (also called \"zone of reading\") on the metropolitan dynamic. ", 
        "These are selection of TELLme concepts made by users in the", a("TELLme Glossary and MGIP tool.",href="http://www.tellme.polimi.it/tellme_apps/tellme/", target="_blank"),
        "The concepts selected in a perspective are highlighted in the \"Semantic Package\" box. ",
        "The software then perform queries on the TELLme-Hub catalog, ",
        "looks for the available geospatial informative layers pertinent to the the Dynamic and to the chosen Perspectives ",
        "and retrieves the layers from the WMS services referred in the TELLme-Hub.")
      ,h4("Credits"), 
      p("Application developed for the TELLme ERASMUS+ project, O4. See",
        a("source code repository on github",
          href="https://github.com/oggioniale/TELLme-mapComposer", 
          target="_blank")
      ),
      p("Please cite:", 
        a("DOI: 10.5281/zenodo.4108649",
          href=appdoi,
          target="_blank")
      )
  )
)

dashboardPagePlus(
  skin = "black-light",
  collapse_sidebar = FALSE,
  sidebar_fullCollapse=TRUE,
  dashboardHeaderPlus(
    title = tagList(
      tags$div(class = "logo-lg",
               tags$img(src = "http://tellmehub.get-it.it/static/img/logo1_200px.png", width = "80px", height = "40px"),
               tags$span(apptitle)
      )
    )
  ),
  sidebar = dashboardSidebar(
    # id = "left_sidebar",
    collapsed = TRUE,
    disable = TRUE,
    width = 0,
    sidebarMenu(
      menuItem("Elaboration", tabName = "site", icon = icon("map", lib = "font-awesome"))
    )
  ),
  body = dashboardBody(
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
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = FALSE,
          draggable = TRUE, top = 60, left = 60, right = "auto", bottom = "auto",
          width = 500, height = "auto",
          HTML('<br><button data-toggle="collapse" data-target="#selectMetropolis"><b>-</b></button><span>',apptitle,'</span>'),
          tags$div(id = 'selectMetropolis', class="collapse",
                   tabsetPanel(
                     tabPanel(
                       "Metropolis",
                       # h2("Filter metropolis:"),
                       selectizeInput('cities',
                                      label = 'Metropolis', 
                                      choices = dt_Metropolis['cityLabel'],
                                      multiple = FALSE,
                                      selected = dt_Metropolis[1,]['cityLabel']
                       ),
                       selectizeInput('dynamics',
                                      label = 'Dynamic',
                                      choices = 'Select dynamics',
                                      multiple = FALSE,
                                      selected = NULL
                       ),
                       fluidRow(
                         valueBoxOutput("vbox1", width = 5),
                         valueBoxOutput("vbox2", width = 7)
                       ),
                       fluidRow(
                         hidden(valueBoxOutput("warning", width = 12))
                       ),
                       fluidRow(
                         valueBoxOutput("vbox3", width = 3),
                         valueBoxOutput("vbox4", width = 9)
                       )
                     ),
                     tabPanel(
                       "Semantic package",
                       tags$p(),
                       tags$h5("TELLme semantics"),
                       tags$p(),
                       selectizeInput(
                         'perspectives',
                         label = 'Select one or more perspective of this dynamic: ',
                         choices = NULL,
                         multiple = TRUE,
                         selected = NULL,
                         options = list(
                           plugins = list("remove_button")
                         )
                       ),
                       # uiOutput(outputId = 'selectionPrespectives'),
                       tags$p(),
                       htmlOutput(outputId = 'bean')
                     ),
                     panels.about_text
                   )
          )
    )
  ))
)#end dashboardPagePlus
