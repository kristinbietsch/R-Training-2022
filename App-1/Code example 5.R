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
    sidebarPanel( h3("Choose Region"),
                  selectInput("ChoRegion", 
                              label = "Choose a Region",
                              choices = unique(data$Region),
                              selected = "")) ,
    mainPanel(  
                h2("Data"),
                fluidRow(
                  column(5, 
                         h4("Growth Rate (R)")) ,
                  column(5, 
                         h4("Last Year of Projection"),
                         sliderInput("LastYear", h5("Choose Year"),
                                     min = 2021, max = 2030, value = 2030, sep = ""))), 
                h2("Results"),
                column(5, 
                       h4("Graphical Results")) ,
                column(5, 
                       h4("Tabular Results")))
  )
  
)

server <- function(input, output, session) {
  
  dat<-reactive({
    
    
    population <- data %>% mutate(Pop2021=Pop2020*exp((R/100) * 1),
                                  Pop2022=Pop2020*exp((R/100) * 2),
                                  Pop2023=Pop2020*exp((R/100) * 3),
                                  Pop2024=Pop2020*exp((R/100) * 4),
                                  Pop2025=Pop2020*exp((R/100) * 5),
                                  Pop2026=Pop2020*exp((R/100) * 6),
                                  Pop2027=Pop2020*exp((R/100) * 7),
                                  Pop2028=Pop2020*exp((R/100) * 8),
                                  Pop2029=Pop2020*exp((R/100) * 9),
                                  Pop2030=Pop2020*exp((R/100) * 10)) %>% 
      gather(Year, Population, Pop2020:Pop2030) %>%
      mutate(Year=as.numeric(substr(Year, 4, 7)))
    
    
    choosendata <- population %>% filter(Year<=input$LastYear) %>% filter(Region==input$ChoRegion) %>% select(-Region, -R)
    
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)