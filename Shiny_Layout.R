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

# HEAD
if(!require("lubridate")){
  install.packages("lubridate")
  library(lubridate)
}

final_dataset <- read_delim("Final_dataset_group_07.csv",delim = ",")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  titlePanel(div(column(width = 7, h2("IDA Study Case Group 07")),
                 column(width = 1, tags$img(src = "qw_gross_trimmed.png", height = 70, width = 125))),
             windowTitle="TitlePage"),
  
  mainPanel(
    
    tabsetPanel(
      type = "tabs",
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("View", 
               fluidRow(
                 numericInput(inputId = "obs",
                              label = "Number of observations to view:",
                              value = 10)),
               fluidRow(
                 tableOutput(outputId = "view")),
      ),
      
      tabPanel("Plot",
               fluidRow(
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("Gemeinden","Gemeinden: ",c(""),multiple = TRUE),
                     dateRangeInput("daterange","Date range:",
                                    start = min(final_dataset$Zulassung),
                                    end=max(final_dataset$Zulassung),
                                    min = min(final_dataset$Zulassung),
                                    max=max(final_dataset$Zulassung))
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