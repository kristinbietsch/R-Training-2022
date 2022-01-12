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
)

server <- function(input, output, session) {
  
}

shinyApp(ui = ui, server = server)