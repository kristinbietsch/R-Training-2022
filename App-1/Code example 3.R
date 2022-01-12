# Shiny App 
# Kristin Bietsch
# Avenir Health
# Jan 2021

library(dplyr)
library(tidyr)
library(ggplot2)
library(rsconnect)
library(shiny)

data <- read.csv("data/PopulationGrowth.csv")

ui <- fluidPage(
  titlePanel( title=div(img(height = 200, width = 109, src="PopulationPyramidPicture.png"), "Projecting Regional Population Growth")),
  sidebarLayout(
    sidebarPanel(
                 h3("Choose Region")) ,
    mainPanel(  
                h2("Data"),
                fluidRow(
                  column(5, 
                         h4("Growth Rate (R)")) ,
                  column(5, 
                         h4("Last Year of Projection"))), 
                h2("Results"),
                column(5, 
                       h4("Graphical Results")) ,
                column(5, 
                       h4("Tabular Results")))
  )
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui = ui, server = server)