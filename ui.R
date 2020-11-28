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

dashboardPagePlus(
  skin = "black-light",
  collapse_sidebar = FALSE,
  #sidebar_fullCollapse=TRUE,
  dashboardHeaderPlus(
    title = tagList(
      tags$div(class = "logo-lg",
               tags$img(src = "http://tellmehub.get-it.it/static/img/logo1_200px.png", width = "80px", height = "40px"),
               tags$span("TELLme Erasmus+ Project - Dynamics and Prespective map")
      )
    )
  ),
  sidebar = dashboardSidebar(
    # id = "left_sidebar",
    collapsed = FALSE,
    disable = FALSE,
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
          HTML('<br><button data-toggle="collapse" data-target="#selectMetropolis"><b>-</b></button>'),
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
                     )
                   )
          )
    )
  ))
)#end dashboardPagePlus
