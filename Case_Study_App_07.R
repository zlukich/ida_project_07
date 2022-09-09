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
if(!require("shinythemes")){
  install.packages("shinythemes")
  library(shinythemes)
}

final_dataset <- read_delim("Final_dataset_group_07.csv",delim = ",")


# Data used for modelling

oem1 = final_dataset %>% filter(Herstellernummer == 1) %>% rename(vehicles_sold = 'Number of Vehicle')
oem2 = final_dataset %>% filter(Herstellernummer == 2) %>% rename(vehicles_sold = 'Number of Vehicle')


#Daily modelling
oem1_daily = oem1 %>% group_by(Zulassung, Herstellernummer) %>% summarize(vehicles_sold = sum(vehicles_sold)) %>% mutate(type = "train") %>%
  rename(date = Zulassung)
oem2_daily = oem2 %>% group_by(Zulassung, Herstellernummer) %>% summarize(vehicles_sold = sum(vehicles_sold)) %>% mutate(type = "train")%>%
  rename(date = Zulassung)

lmmodel1_daily = lm(vehicles_sold ~ date, data = oem1_daily)
lmmodel2_daily = lm(vehicles_sold ~ date, data = oem2_daily)

#For weeks we use week calender to find Date that represent week (we take 1 week)
week_calender = as.Date(seq(ISOdate(2014,1,1),ISOdate(2016,12,31),by="week"))
week_calender = data.frame(date = week_calender)
week_calender = week_calender %>% mutate(date_week = paste0(year(date),"-",isoweek(date)))

#Weekly modelling
oem1_weekly= oem1 %>% 
  mutate(date_week = paste0(year(Zulassung),"-",isoweek(Zulassung)))%>%
  group_by(date_week, Herstellernummer) %>% 
  summarize(vehicles_sold = sum(vehicles_sold)) %>% 
  mutate(type = "train") %>% inner_join(week_calender,by = "date_week")

oem2_weekly = oem2 %>% mutate(date_week = paste0(year(Zulassung),"-",isoweek(Zulassung)))%>%
  group_by(date_week, Herstellernummer) %>% 
  summarize(vehicles_sold = sum(vehicles_sold)) %>% 
  mutate(type = "train") %>% inner_join(week_calender,by = "date_week")


lmmodel1_weekly = lm(vehicles_sold ~ date, data = oem1_weekly)
lmmodel2_weekly = lm(vehicles_sold ~ date, data = oem2_weekly)


#Monthly modelling
oem1_monthly = oem1 %>% mutate(date_month = paste0(year(Zulassung),"-",month(Zulassung))) %>%
  group_by(date_month, Herstellernummer) %>% 
  summarize(vehicles_sold = sum(vehicles_sold)) %>% 
  mutate(type = "train") %>% 
  mutate(date_month = as.Date(paste0(date_month,"-","1"),"%Y-%m-%d")) %>% rename(date = date_month)


oem2_monthly = oem2 %>% mutate(date_month = paste0(year(Zulassung),"-",month(Zulassung))) %>%
  group_by(date_month, Herstellernummer) %>% 
  summarize(vehicles_sold = sum(vehicles_sold)) %>% 
  mutate(type = "train") %>% 
  mutate(date_month = as.Date(paste0(date_month,"-","1"),"%Y-%m-%d")) %>% rename(date = date_month)

lmmodel1_monthly = lm(vehicles_sold~date,data = oem1_monthly)
lmmodel2_monthly = lm(vehicles_sold ~ date,data = oem2_monthly)



ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  #Styling with css
#  tags$head(
#    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
#  ),
  # App title ----
  titlePanel(div(column(width = 7, h2("IDA Study Case Group 07")),
                 column(width = 1, tags$img(src = "qw_gross_trimmed.png", height = 70, width = 125))),
             windowTitle="TitlePage"),

  # Main panel for displaying outputs ----
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
               ),
      
      tabPanel("4.c",
               fluidRow(
                 sidebarLayout(
                   sidebarPanel(
                     radioButtons("date_selection","Select scale by: ",
                                  choices = c("Daily","Weekly","Monthly")),
                   ),
                   mainPanel(
                     plotlyOutput(outputId ="regression_plot")
                   ),
                   position = "left")
               )
      )
      
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
  
  output$plot <-renderPlot({
    #final_dataset%>%group_by(Gemeiden)%>%summarise(total_sell = sum(`Number of Vehicle`))
    final_dataset%>%filter(Gemeinden %in% input$Gemeinden )%>%filter(Zulassung >= input$daterange[1] & Zulassung <=input$daterange[2] )%>%
      ggplot(aes(x=Zulassung ,y=`Number of Vehicle`,color = Gemeinden))+
      geom_line()
  })
  
  output$regression_plot<- renderPlotly({
    if(input$date_selection == "Daily"){
      
      lmmodel1 = lmmodel1_daily
      lmmodel2 = lmmodel2_daily
      oem1_data = oem1_daily
      oem2_data = oem2_daily
      
      
    }else if(input$date_selection == "Weekly"){
      lmmodel1 = lmmodel1_weekly
      lmmodel2 = lmmodel2_weekly
      oem1_data = oem1_weekly
      oem2_data = oem2_weekly
    }
    else{
      lmmodel1 = lmmodel1_monthly
      lmmodel2 = lmmodel2_monthly
      oem1_data = oem1_monthly
      oem2_data = oem2_monthly
    }
    
    #Generate days for the Q1 of 2017 and make predictions
    dates2017 = seq(ISOdate(2017,1,1),ISOdate(2017,4,1),by = "day")
    #Make a df from it for OEM1
    df2017_1 = data.frame(date = as.Date(dates2017))
    df2017_1 = df2017_1 %>% add_predictions(lmmodel1) %>% mutate(Herstellernummer = 1,type = "linear_model") %>% rename(vehicles_sold = pred) 
    
    #Make a df from it for OEM2
    df2017_2 = data.frame(date = as.Date(dates2017))
    df2017_2 = df2017_2 %>% add_predictions(lmmodel2) %>% mutate(Herstellernummer = 2,type = "linear_model") %>% rename(vehicles_sold = pred) 
    
    
    daily_result = rbind(oem1_data,oem2_data,df2017_1,df2017_2)
    
    #To draw lines
    predictions_1 = oem1_data  %>% add_predictions(lmmodel1) %>% select(-vehicles_sold) %>% rename(vehicles_sold = pred) %>%
      mutate(type = "predict")
    predictions_2 = oem2_data  %>% add_predictions(lmmodel2) %>% select(-vehicles_sold) %>% rename(vehicles_sold = pred) %>%
      mutate(type = "predict")
    
    daily_result = rbind(daily_result,predictions_1,predictions_2)
    
    ggplotly(ggplot(data = daily_result)+
      geom_point(aes(x = date,y = vehicles_sold,color = type),alpha = 0.5)+
      geom_vline(aes(xintercept = as.numeric(as.Date(ISOdate(2017,1,1)))))+
      facet_grid(Herstellernummer~.)+
      ggtitle(paste0("Linear regression for OEM1 and OEM2 based on ", input$date_selection ," basis")))
  })
}

shinyApp(ui = ui, server = server)