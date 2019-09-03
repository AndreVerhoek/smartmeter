#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

stroomVerbruik <- tabPanel("EnergieVerbruik",
           tabsetPanel(
              tabPanel("Electriciteit gebruik",
                fluidRow(
                  highchartOutput("electricityChart")
                ),
                fluidRow(
                  htmlOutput("electricitySummary")
                ),
                fluidRow(column(5,""), column(6, actionButton("refresh","Vernieuw",icon = icon("refresh"))))
             ), 
             tabPanel("Gas verbruik",
              fluidRow(
                highchartOutput("gasChart")
              ),
              fluidRow(
                htmlOutput("gasSummary")
              ),
              fluidRow(column(5,""), column(6, actionButton("refresh","Vernieuw",icon = icon("refresh"))))
             )
           )
)


shinyUI(fluidPage(
  tags$head(
    tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
  ),
  stroomVerbruik
))

