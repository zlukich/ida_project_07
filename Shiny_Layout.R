# Check libraries

if(!require(readr)){
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

if(!require("shinythemes")){
  install.packages("shinythemes")
  library(shinythemes)
}

if(!require("shinydashboard")){
  install.packages("shinydashboard")
  library(shinydashboard)
}

ui <- fluidPage(
  theme = shinytheme("cerulean"),

  titlePanel(div(column(width = 5, h2("IDA Study Case Group 07")),
                 column(width = 1, tags$img(src = "qw_gross_trimmed.png", height = 40, width = 150))),
             windowTitle="TitlePage"),
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("View", tableOutput("view")),
                tabPanel("Plot",
                         fluidRow(
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("Gemeinden","Gemeinden: ",c(""),multiple = TRUE),
                               dateRangeInput("daterange","Date range:",start = "2014-01-01",end="2016-12-30",min = "2014-01-01",max="2016-12-30")
                             ),
                             mainPanel(
                               plotOutput(outputId ="plot")
                             ),
                             position = "left")
                         )
                )
    )
  )

  
)

server <- function(input,output){
  
}

shinyApp(ui = ui, server = server)