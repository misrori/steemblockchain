library(shiny)
library(plotly)
library(data.table)
library(DT)
library(leaflet)
library(lubridate)
navbarPage(
  title="Steemblockchain explorer",theme = "journal",fluid = TRUE,
  tabPanel("trransferee",
           sidebarLayout(
             sidebarPanel(
              
               shiny::textInput('name', 'Name',value = "")
               
               
               ),
             mainPanel(
               
               HTML('fejlesztés')
               
             )
               )),
  tabPanel("Előrejelzés"
  ),
  tabPanel("Térkép"
  )
           
           

           
           
  )
  

