#Sichern, dass StringsAsFactors gleich FALSE
options(stringsAsFactors = F)

# Überprüfen, ob die Packages bereits installiert sin
if(!require(readr)){
  # wenn es nicht installiert ist, wird es nun installiert
  install.packages("readr", dependencies = TRUE)
  library(readr)
}


if(!require(tidyverse)){
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

final_dataset <- read_delim("Additional files/Final_dataset_group_07.csv",delim = ",")

ui <- fluidPage(
  #Styling with css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  # App title ----
  titlePanel("IDA Case Study. Group# 07"),
  img(src = "qw_gross.png",width = 125, height = 125),
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("View", tableOutput("view")),
                  tabPanel("Plot", plotOutput("plot")
      )
      )
      # Output: Verbatim text for data summary ----
      
      
      # Output: HTML table with requested number of observations ----
      
      
    )
  )
)

server <- function(input,output){
  # Return the requested dataset ----
  # datasetInput <- reactive({
  #   switch(input$dataset,
  #          "rock" = rock,
  #          "pressure" = pressure,
  #          "cars" = cars)
  # })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- final_dataset
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(final_dataset, n = input$obs)
  })
  
  output$plot <- renderPlot({
    final_dataset_year = final_dataset %>% mutate(date_year =(year(Zulassung))) %>%
      rename(anzahl = 'Number of Vehicle') %>%
      group_by(date_year,Herstellernummer,Gemeinden) %>% 
      summarize(n = sum(anzahl))
    ggplot(final_dataset_year)+
      geom_line(aes(x = date_year,y = n, colour = Gemeinden))+
      facet_wrap(~Herstellernummer)
    
    })
}

shinyApp(ui = ui, server = server)