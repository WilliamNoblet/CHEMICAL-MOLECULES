
#install.packages("plotly")
library(shiny)
library(plotly)
library(shinyjs)
library(plotly)

setwd("C:/Users/William/Desktop/CHEMICAL-MOLECULES")

setwd("C:/Users/User/OneDrive/Desktop/CHEMICAL-MOLECULES")


data <- read.csv("PubChemElements_all.csv")

head(data)

grey_color = '606060'

atom_data <- data %>%
  dplyr::mutate(CPKHexColor = ifelse(CPKHexColor == '', grey_color, CPKHexColor)) %>%
  dplyr::mutate(CPKHexColor = ifelse(CPKHexColor == '6985', '95dabd', CPKHexColor))





# Générer les points sur la sphère
generate_sphere_mesh <- function(radius = 1, n = 100) {
  phi <- seq(0, pi, length.out = n)
  theta <- seq(0, 2 * pi, length.out = n)
  
  x <- outer(sin(phi), cos(theta)) * radius
  y <- outer(sin(phi), sin(theta)) * radius
  z <- outer(cos(phi), rep(1, n)) * radius
  
  list(x = x, y = y, z = z)
}

# Générer les points pour une sphère de rayon 1
sphere_mesh <- generate_sphere_mesh(radius = 1, n = 100)






fig <- plot_ly(
  x = ~sphere_mesh$x, 
  y = ~sphere_mesh$y, 
  z = ~sphere_mesh$z, 
  type = 'surface',
  colorscale = list(c(0, 'red'), c(1, 'red')),  # Spécifier une couleur unique
  showscale = FALSE  # Désactiver l'affichage de l'échelle de couleurs
) %>%
  layout(
    title = "3D Sphere",
    scene = list(
      xaxis = list(
        title = '',
        showgrid = FALSE,
        zeroline = FALSE,
        visible = FALSE
      ),
      yaxis = list(
        title = '',
        showgrid = FALSE,
        zeroline = FALSE,
        visible = FALSE
      ),
      zaxis = list(
        title = '',
        showgrid = FALSE,
        zeroline = FALSE,
        visible = FALSE
      )
    )
  )

# Afficher le graphique
fig



colnames(data)





atom_name = atom_data$Name[1]

atom_color = paste0('#', atom_data$CPKHexColor[1])


fig <- plot_ly(
  x = ~sphere_mesh$x, 
  y = ~sphere_mesh$y, 
  z = ~sphere_mesh$z, 
  type = 'surface',
  colorscale = list(c(0, atom_color), c(1, atom_color)),
  showscale = FALSE
) %>%
  layout(
    title = atom_name,
    scene = list(
      xaxis = list(
        title = '',
        showgrid = FALSE,
        zeroline = FALSE,
        visible = FALSE
      ),
      yaxis = list(
        title = '',
        showgrid = FALSE,
        zeroline = FALSE,
        visible = FALSE
      ),
      zaxis = list(
        title = '',
        showgrid = FALSE,
        zeroline = FALSE,
        visible = FALSE
      )
    )
  )

# Afficher le graphique
fig




generate_atom_row <- function(index) {
  fluidRow(
    column(2, class = "center-content", atom_data$AtomicNumber[index]),
    column(2, class = "center-content", atom_data$Name[index]),
    column(2, class = "center-content", atom_data$Symbol[index]),
    column(2, class = "center-content", atom_data$Symbol[index]),
    column(4, plotlyOutput(paste0('fig_atom_', index)))
  )
}










###Rshiny Set Pieces

#https://github.com/georgemirandajr/career-pathfinder/blob/master/ui.R#L25
#https://shiny.posit.co/r/gallery/finance-banking/career-pathfinder/

library(dplyr)
library(plotly)
library(StatsBombR)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)

ui <- shinyUI(navbarPage(id = 'navBar',
                         theme = 'paper.css',
                         collapsible = TRUE,
                         inverse = TRUE,
                         windowTitle = 'Chemical molecules',
                         position = 'fixed-top',
                         #footer = includeHTML("./www/include_footer.html"),
                         header = tags$style(
                           ".navbar-right {
                       float: right !important;
                       }",
                           "body {padding-top: 75px;}"),
                         
                         tags$head(
                           tags$style(
                             HTML('
                              body {
                                background-color: #333333; /* To change the background color. */
                                color: #ffffff; /* To change the font color. */
                              }
                              
                              #comparaison_corner_off {
                                min-width: 500px;
                                min-height: 1000px;
                                max-width: 500px;
                                max-height: 1000px;
                                overflow-x: auto;
                              }
                              
                              .center-content {
        display: flex;
        justify-content: center;
        align-items: center;
        text-align: center;
        font-family: "Arial", sans-serif;
        font-size: 50px;
      }
                                  
                                  '
                             ))
                         ),
                         
                         tabPanel('HOME', value = 'home',
                                  
                                  fluidRow(
                                    column(3),
                                    column(6,
                                           shiny::HTML("<br><br><center> <h1>Chemical Molecules"),
                                           shiny::HTML("<h3>.</h3>")
                                    ),
                                    column(3)
                                  )
                         ),
                         
                         tabPanel('Atom',
                                  
                                  fluidRow(column(3),
                                           column(6,
                                                  shiny::HTML("<br><br><center> <h1>Visualization of atoms"),
                                                  
                                                  shiny::HTML("<h3>.</h3>"),
                                                  
                                                  shiny::HTML("<h5>.</h5>")
                                           ),
                                           column(3)
                                           ),
                                
                                  
                                  fluidRow(column(2, class = "center-content", atom_data$AtomicNumber[1]),
                                           column(2, class = "center-content", atom_data$Name[1]),
                                           column(2, class = "center-content", atom_data$Symbol[1]),
                                           column(2, class = "center-content", atom_data$Symbol[1]),
                                           column(4, plotlyOutput('fig_atom'))
                                           ),
                                  
                                  #lapply(1:nrow(atom_data), generate_atom_row)
                                  lapply(1:nrow(atom_data), generate_atom_row)
                                  
                                  
                         ),
                         
                         tabPanel('Chemical molecules', 
                         ),
                         
                         tabPanel('2', 
                                  
                         ),
                         
                         tabPanel('3',
                                  
                         )
                         
                         
))





server <- function(input, output, session){
  
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  if (!require("plotly")) install.packages("plotly")
  library(plotly)
  if (!require("openxlsx")) install.packages("openxlsx")
  library(openxlsx)
  library(StatsBombR)
  
  
  
  fig <- plot_ly(
    x = ~sphere_mesh$x, 
    y = ~sphere_mesh$y, 
    z = ~sphere_mesh$z, 
    type = 'surface',
    colorscale = list(c(0, paste0('#', atom_data$CPKHexColor[1])), c(1, paste0('#', atom_data$CPKHexColor[1]))),
    showscale = FALSE
  ) %>%
    layout(
      title = '',
      scene = list(
        xaxis = list(
          title = '',
          showgrid = FALSE,
          zeroline = FALSE,
          visible = FALSE
        ),
        yaxis = list(
          title = '',
          showgrid = FALSE,
          zeroline = FALSE,
          visible = FALSE
        ),
        zaxis = list(
          title = '',
          showgrid = FALSE,
          zeroline = FALSE,
          visible = FALSE
        )
      )
    )
  
  
  output[['fig_atom']] <- renderPlotly(fig)
  
  for (i in 1:nrow(atom_data)) {
      local({
        my_i <- i
        output[[paste0('fig_atom_', my_i)]] <- renderPlotly({
          plot_ly(
            x = ~sphere_mesh$x, 
            y = ~sphere_mesh$y, 
            z = ~sphere_mesh$z, 
            type = 'surface',
            colorscale = list(c(0, paste0('#', atom_data$CPKHexColor[my_i])), c(1, paste0('#', atom_data$CPKHexColor[my_i]))),
            showscale = FALSE
          ) %>%
            layout(
              title = '',
              scene = list(
                xaxis = list(
                  title = '',
                  showgrid = FALSE,
                  zeroline = FALSE,
                  visible = FALSE
                ),
                yaxis = list(
                  title = '',
                  showgrid = FALSE,
                  zeroline = FALSE,
                  visible = FALSE
                ),
                zaxis = list(
                  title = '',
                  showgrid = FALSE,
                  zeroline = FALSE,
                  visible = FALSE
                )
              )
            )
        })
      })

  }
  
  
}


##shinyApp(ui, server)









#library(shiny)
#library(shinyjs)
#
#ui <- fluidPage(
#  useShinyjs(),
#  
#  tags$head(
#    tags$style(HTML('
#      body.light-mode {
#        background-color: #ffffff; /* Couleur de fond en mode clair */
#        color: #333333; /* Couleur du texte en mode clair */
#      }
#      body.dark-mode {
#        background-color: #333333; /* Couleur de fond en mode sombre */
#        color: #ffffff; /* Couleur du texte en mode sombre */
#      }
#    '))
#  ),
#  
#  h1("Ma Application Shiny"),
#  
#  actionButton("toggleMode", "Changer de mode"),
#  
#  # Autres éléments Shiny ici
#)
#
#server <- function(input, output, session) {
#  
#  observeEvent(input$toggleMode, {
#    shinyjs::runjs('
#      var body = document.body;
#      if (body.classList.contains("light-mode")) {
#        body.classList.remove("light-mode");
#        body.classList.add("dark-mode");
#      } else {
#        body.classList.remove("dark-mode");
#        body.classList.add("light-mode");
#      }
#    ')
#  })
#  
#  # Code du serveur
#}
#
#shinyApp(ui, server)














