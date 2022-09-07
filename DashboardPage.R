#Sichern, dass StringsAsFactors gleich FALSE
options(stringsAsFactors = F)

# Überprüfen, ob die Packages bereits installiert sin
if(!require(readr)){
  # wenn es nicht installiert ist, wird es nun installiert
  install.packages("readr", dependencies = TRUE)
  library(readr)
}


if(!require("tidyverse")){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require("plotly")){
  install.packages("plotly")
  library(plotly)
}

if(!require("shiny")){
  install.packages("shiny")
  library(shiny)
}
if(!require("modelr")){
  install.packages("modelr")
  library(modelr)
}

if(!require("lubridate")){
  install.packages("lubridate")
  library(lubridate)
}

if(!require("shinydashboard")){
  install.packages("shinydashboard")
  library(shinydashboard)
}

ui <- dashboardPage(
  
  skin = "blue",
  dashboardHeader(
    
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 60px}"),
            tags$style(".main-header .logo {height: 60px}")
    ),
    
    title = div(column(width = 2, tags$img(src = "qw_gross_trimmed.png", height = 60, width = 100)),
                column(width = 9, h3("IDA Study Case Group 07"))),
    titleWidth = 500),
  dashboardSidebar(
    width = 200,
    tags$style(".left-side, .main-sidebar {padding-top: 60px}"),
    
    sidebarMenu(
      
      menuItem("Analyse fehlerhafter Sitze",
               tabName = "rev",
               icon = icon("briefcase"))
    )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "rev",
        
        fluidRow(
          align="center",
          fluidRow(
            align="center",
            box(
              title = "Liste aller betroffenen zugelassenen Fahrzeuge",
              status = "success",
              solidHeader = TRUE,
              dataTableOutput('data'),
              
              width=12
            ),
            box(
              title = "Verteilung der betroffenen Fahrzeuge in den Gemeinden",
              status = "success",
              solidHeader = TRUE,
              plotlyOutput('mapplot', height = "900px"),
              
              width=12
            ),
            
            box(
              title = "Zulassungsverlauf über Gemeinde",
              status = "success",
              solidHeader = TRUE,
              plotlyOutput('heatplot', height = "900px"),
              
              width=12
            )
            
          )
          
        )
      )
      
      
    )
  )
)

server <- function(input,output){
  
}

shinyApp(ui = ui, server = server)