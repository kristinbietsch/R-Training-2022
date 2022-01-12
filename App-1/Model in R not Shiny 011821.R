# Kristin Bietsch
# Avenir Health
# Jan 18, 2021

library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Users/KristinBietsch/files/R Code")
data <- read.csv("PopulationGrowth.csv")


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


choosendata <- population %>% filter(Year<=2030) %>% filter(Region=="Asia") %>% select(-Region, -R)

ggplot(choosendata, aes(x=as.factor(Year), y=Population, fill=Year)) +
  geom_bar(stat = "identity") +
  labs( title="Population", y="", x="") +
  theme_bw() +
  theme(legend.position = "none")
  