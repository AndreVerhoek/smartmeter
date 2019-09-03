library(shiny)
library(ggplot2)
library(lubridate)
library(highcharter)
library(tidyverse)
library(quantmod)

retrieveLatestData <- function(){
  current_folder <- "Location of file"
  new_folder <- "location of server files"
  list_of_files <- list.files(current_folder, ".txt") 
  file.copy(file.path(current_folder,list_of_files), new_folder)
}

updateEveryFiveMinutes <- function(){
  while(TRUE)
  {
    retrieveLatestData()
    Sys.sleep(300)
  }
}

getData <- function(){
  data <- read.table("EnergyVerbruik.txt", 
                     sep="\t", header = FALSE, stringsAsFactors = FALSE)
  names(data) <- data[1,]
  data <- data[-1,]
  data[,c(2:7,9)] <- sapply(c(2:7,9), function(x) as.numeric(data[,x]))
  data[,1] <- as.POSIXct(data[,1])
  return(data)
}

getGasData <- function(data){
  gas.data <- data.frame(LaatsteMeting = data$`Laatste gasmeting`,data$Gasstand)
  gas.data$LaatsteMeting <- as.POSIXct(paste(as.Date(substring(data$`Laatste gasmeting`,1,6), "%y%m%d")," ",substring(data$`Laatste gasmeting`,7,8),":00:00",sep=''))
  gas.data <- unique(gas.data)
  gas.data$Verbruik <- c(NA,diff(gas.data$data.Gasstand))
  gas.data <- gas.data[complete.cases(gas.data),]
  return(gas.data)
}

createGasChart <- function(data, input, output){
  gas.data <- getGasData(data)
  output$gasChart <- renderHighchart({
    gas <- xts(gas.data$Verbruik, order.by = gas.data$LaatsteMeting)
    highchart(type = "stock") %>% 
      hc_add_series(gas, name="Verbruikt gas") %>% 
      hc_rangeSelector( buttons = list(
        list(type = 'all', text = 'Alles'),
        list(type = 'day', count = 7, text = '1w'),
        list(type = 'day', count = 1, text = '1d'),
        list(type = 'hour', count = 1, text = '1h'),
        list(type = 'minute', count = 1, text = '1m')
      ))
  })
}


createElectricityChart <- function(data, input, output){
  output$electricityChart <- renderHighchart({
    huidigVerbruik <- aggregate(`Verbruik nu` ~ round_date(data$Datum,unit="minute",week_start = 7), data, sum)
    names(huidigVerbruik) <- c("Datum","Verbruik")
    huidigTeruglever <- aggregate(`Teruglever nu` ~ round_date(data$Datum,unit="minute",week_start = 7), data, sum)
    names(huidigTeruglever) <- c("Datum","Teruglever")
    a <- xts(huidigVerbruik$Verbruik/360, order.by = huidigVerbruik$Datum)
    b <- xts(huidigTeruglever$Teruglever/360, order.by = huidigTeruglever$Datum)
    print("Data created")
    highchart(type = "stock") %>% 
      hc_add_series(a, name="Verbruikt") %>% 
      hc_add_series(b, name="Teruggeleverd") %>%
      hc_rangeSelector( buttons = list(
        list(type = 'all', text = 'Alles'),
        list(type = 'day', count = 7, text = '1w'),
        list(type = 'day', count = 1, text = '1d'),
        list(type = 'hour', count = 1, text = '1h'),
        list(type = 'minute', count = 1, text = '1m')
      ))
  })
}

createElectricitySummary <- function(data, input, output){
  output$electricitySummary <- renderUI({
    huidigVerbruik <- aggregate(`Verbruik nu` ~ round_date(data$Datum,unit="day",week_start = 7), data, sum)
    names(huidigVerbruik) <- c("Datum","Verbruik")
    huidigVerbruik$Verbruik <- huidigVerbruik$Verbruik / (1000*360)
    huidigTeruglever <- aggregate(`Teruglever nu` ~ round_date(data$Datum,unit="day",week_start = 7), data, sum)
    names(huidigTeruglever) <- c("Datum","Teruglever")
    huidigTeruglever$Teruglever <- huidigTeruglever$Teruglever / (1000*360)
    verbruikMinVandaag <- huidigVerbruik[-nrow(huidigVerbruik),]
    terugleverMinVandaag <- huidigTeruglever[-nrow(huidigTeruglever),]
    HTML(paste("&nbsp;&nbsp;&nbsp;&nbsp;Gemiddeld dagelijks verbruik:",round(mean(huidigVerbruik$Verbruik),1),"kWh.</br> ",
                   "&nbsp;&nbsp;&nbsp;&nbsp;Laagste verbruik was",round(min(verbruikMinVandaag$Verbruik),1),"kWh op",
                   format(verbruikMinVandaag[verbruikMinVandaag$Verbruik == min(verbruikMinVandaag$Verbruik),]$Datum[1], 
                          "%d %B %Y"), ".</br>","&nbsp;&nbsp;&nbsp;&nbsp;Het hoogste verbruik was",
               round(max(huidigVerbruik$Verbruik),1),"kWh op", 
                   format(huidigVerbruik[huidigVerbruik$Verbruik == max(huidigVerbruik$Verbruik),]$Datum[1], 
                          "%d %B %Y"),".</br>",
         "&nbsp;&nbsp;&nbsp;&nbsp;Gemiddeld dagelijks teruglevering:",round(mean(huidigTeruglever$Teruglever),1),"kWh.</br> ",
         "&nbsp;&nbsp;&nbsp;&nbsp;Het minst teruggeleverd is",round(min(terugleverMinVandaag$Teruglever),1),"kWh op",
         format(terugleverMinVandaag[terugleverMinVandaag$Teruglever == min(terugleverMinVandaag$Teruglever),]$Datum[1], 
                "%d %B %Y"), ".</br>","&nbsp;&nbsp;&nbsp;&nbsp;Het meeste teruggeleverd is ",
         round(max(huidigTeruglever$Teruglever),1),"kWh op", 
         format(huidigTeruglever[huidigTeruglever$Teruglever == max(huidigTeruglever$Teruglever),]$Datum[1], 
                "%d %B %Y"),".</br>"))
  })
}

createGasSummary <- function(data, input, output){
  output$gasSummary <- renderUI({
    gas.df <- getGasData(data)
    
    huidigVerbruik <- aggregate(Verbruik ~ round_date(gas.df$LaatsteMeting,unit="day",week_start = 7), gas.df, sum)
    names(huidigVerbruik)[1] <- "Datum"
    verbruikMinVandaag <- huidigVerbruik[-nrow(huidigVerbruik),]
    HTML(paste("&nbsp;&nbsp;&nbsp;&nbsp;Gemiddeld dagelijks verbruik:",round(mean(huidigVerbruik$Verbruik),3),"m3.</br> ",
               "&nbsp;&nbsp;&nbsp;&nbsp;Laagste verbruik was",round(min(verbruikMinVandaag$Verbruik),3),"m3 op",
               format(verbruikMinVandaag[verbruikMinVandaag$Verbruik == min(verbruikMinVandaag$Verbruik),]$Datum[1], 
                      "%d %B %Y"), ".</br>","&nbsp;&nbsp;&nbsp;&nbsp;Het hoogste verbruik was",
               round(max(huidigVerbruik$Verbruik),3),"m3 op", 
               format(huidigVerbruik[huidigVerbruik$Verbruik == max(huidigVerbruik$Verbruik),]$Datum[1], 
                      "%d %B %Y"),".</br>"))
  })
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  values <- reactiveValues()
  values$data <- getData()
  
  observeEvent(TRUE,once = TRUE,{
    createElectricityChart(values$data, input, output)
    createGasChart(values$data, input, output)
    createElectricitySummary(data, input, output)
    createGasSummary(data, input,output)
  })
  
  observeEvent(input$refresh, {
    values$data <- getData()
    createElectricityChart(values$data, input, output)
    createGasChart(values$data, input, output)
  })
})