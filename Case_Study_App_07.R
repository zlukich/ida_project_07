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
if(!require("forecast")){
  install.packages("forecast")
  library(forecast)
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
  #theme = shinytheme("cosmo"),
  
  #Styling with css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  # App title ----
  titlePanel(div(column(width = 7, h2("IDA Study Case Group 07")),
                 column(width = 1, tags$img(src = "qw_gross_trimmed.png", height = 70, width = 125))),
             windowTitle="TitlePage"),

  # Main panel for displaying outputs ----
  mainPanel(
    
    tabsetPanel(
      type = "tabs",
      
               
      tabPanel("4.a Temporal Course",
               fluidRow(
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("Gemeinden","Gemeinden: ",choices = unique(final_dataset$Gemeinden),selected = c("BOCHUM"),multiple = TRUE),
                     dateRangeInput("daterange","Date range:",start = min(final_dataset$Zulassung),end=max(final_dataset$Zulassung),min = min(final_dataset$Zulassung),max=max(final_dataset$Zulassung))
                   ),
                   mainPanel(
                     plotlyOutput(outputId ="plot")
                   ),
                   position = "left")
               )
      ),
      tabPanel("4.b Defective",
               fluidRow(
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("Gemeinden1","Gemeinden: ",c(""),multiple = TRUE),
                     dateRangeInput("daterange1","Date range:",start = min(final_dataset$Zulassung),end=max(final_dataset$Zulassung),min = min(final_dataset$Zulassung),max=max(final_dataset$Zulassung))
                   ),
                   mainPanel(
                     #plotOutput(outputId = "DefectiveVehicle"),
                     #plotOutput(outputId = "DefectiveComp"),
                     #plotOutput(outputId = "DefectiveParts"),
                     plotlyOutput(outputId = "DefectiveTotal"),        
                     tableOutput('table'),
                     tableOutput('pivot')
                   ),
                   position = "left")
               )
      ),
      
      tabPanel("4.c Forecast",
               fluidRow(
                 sidebarLayout(
                   sidebarPanel(
                     radioButtons("date_selection","Select scale by: ",
                                  choices = c("Daily","Weekly","Monthly","Arima")),
                   ),
                   mainPanel(
                     plotlyOutput(outputId ="regression_plot")
                   ),
                   position = "left")
               )
      ),
      tabPanel("4.d View",
               fluidRow(
                 dataTableOutput(outputId = "view")),
      ),
      
      )
  )

)


server <- function(input,output,session){
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
  output$view <- renderDataTable({
    final_dataset
  })
  
  updateSelectInput(session,inputId = "Gemeinden",choices=unique(final_dataset$Gemeinden),selected = "BOCHUM")
  
  #plot function 
  subset4a<<-reactive(final_dataset%>%filter(Gemeinden %in% input$Gemeinden )%>%filter(Zulassung >= input$daterange[1] & Zulassung <=input$daterange[2] ))
  output$plot <-renderPlotly({
    #final_dataset%>%group_by(Gemeiden)%>%summarise(total_sell = sum(`Number of Vehicle`))
    validate(
      need(input$Gemeinden, "To render a plot please select a Gemeinde")
    )
    ggplotly(ggplot(data= subset4a(),aes(x=Zulassung ,y=`Number of Vehicle`,color = Gemeinden))+
      geom_line()+
        facet_grid(Herstellernummer~.))
  })
  #fehlter plot
  updateSelectInput(session,inputId = "Gemeinden1",choices=unique(final_dataset$Gemeinden),selected = "BOCHUM")
  fehlerVehicle<<-reactive(final_dataset%>%filter(Gemeinden %in% input$Gemeinden1 )%>%group_by(Herstellernummer)%>%filter(Zulassung >= input$daterange1[1] & Zulassung <=input$daterange1[2] )%>%summarize(TotalVehicle=sum(`Number of Vehicle`),TotalDefectiveVehicle=sum(`Defective Vehicle`),TotalComponents=sum(`Number of Components`),TotalDefectiveComponents=sum(`Defective Components`),TotalParts=sum(`Number of Parts`),TotalDefectiveParts=sum(`Defective Parts`))%>%mutate(rateVehicle=TotalDefectiveVehicle*100/TotalVehicle)%>%mutate(rateComponents=TotalDefectiveComponents*100/TotalComponents)%>%mutate(rateParts=TotalDefectiveParts*100/TotalParts))
  
  output$table <- renderTable(fehlerVehicle())
  
  df_pivoted <-reactive(pivot_longer(fehlerVehicle(),cols = c('rateVehicle','rateComponents','rateParts')))
  output$pivot <- renderTable(df_pivoted())
  
  output$DefectiveTotal <-renderPlotly({
    validate(
      need(input$Gemeinden1, "To render a plot please select a Gemeinde")
    )
    ggplotly(ggplot(data=df_pivoted(),aes(x=Herstellernummer,y=value,fill=name))+
               geom_bar(stat="identity",position = "dodge"))
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
    else if(input$date_selection == "Monthly"){
      lmmodel1 = lmmodel1_monthly
      lmmodel2 = lmmodel2_monthly
      oem1_data = oem1_monthly
      oem2_data = oem2_monthly
    }else{
      dat_ts = ts(oem1_monthly$vehicles_sold,start = c(2014,1),end = c(2016,12),frequency = 12)
      arimamodel = auto.arima(dat_ts)
      fore_arima = forecast::forecast(arimamodel,h = 12)
      df_arima = as.data.frame(fore_arima)
      df_arima$date = as.Date(seq(ISOdate(2017,1,1),ISOdate(2017,12,1),by = "month"))
      df_arima = df_arima %>% rename(vehicles_sold ='Point Forecast')
      
      return(
        ggplotly(
        ggplot()+
          geom_line(data = oem1_monthly,aes(date,vehicles_sold),color = "blue")+
          geom_line(data = df_arima,aes(date,vehicles_sold),color = "red") +
          ggtitle(paste0("Linear regression for OEM1 and OEM2 based on ", input$date_selection ," basis"))+
          geom_vline(aes(xintercept = as.numeric(as.Date(ISOdate(2017,1,1)))))
          
      )
      )
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