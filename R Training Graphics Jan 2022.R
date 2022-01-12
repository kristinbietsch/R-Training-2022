# GGPlot Introduction for Avenir Health
# Janurary 2022
# Kristin Bietsch, PhD, Avenir Health


# https://www.rstudio.com/resources/cheatsheets/

library(tidyverse)
library(viridis)

setwd("C:/Users/KristinBietsch/files/R Code/2022 Training")

# Load Data
data <- read.csv("CPR Trends.csv")

# Prepare data- it helps if everythig is long (like a Pivot Table)
data_long <- data %>% gather(Type, Value, Modern:Traditional)

# Build basic graphics (point, line, stacked bar)

ggplot(data_long, aes(x=Year, y=Value, color=Country, shape=Type)) +
  geom_point(size=2) +
  theme_bw()

ggplot(data_long, aes(x=Year, y=Value, color=Country, shape=Type)) +
  geom_point(size=2) +
  geom_line() +
  theme_bw()

# Facet
ggplot(data_long, aes(x=Year, y=Value, color=Type)) +
  geom_point(size=2) +
  geom_line() +
  facet_wrap(~ Country) +
  theme_bw()


ggplot(data_long, aes(x=Year, y=Value, fill=Type)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~ Country) +
  theme_bw()


# Subset

ggplot(subset(data_long, Country=="Bangladesh"), aes(x=Year, y=Value, color=Country, shape=Type)) +
  geom_point(size=2) +
  geom_line() +
  theme_bw()


# Change colors
ggplot(data_long, aes(x=Year, y=Value, fill=Type)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~ Country) +
  scale_fill_viridis(discrete=TRUE) +
  theme_bw()


ggplot(data_long, aes(x=Year, y=Value, fill=Type)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~ Country) +
  scale_fill_manual(
    values = c('#3892C6','#8DC645'),
  breaks = c("Modern", "Traditional")) +
  theme_bw()


# Save file

# If you assign an object name to a graph, you can recall it to save it- otherwise you will be saving the most recent graphic in the viewer

# I can tell it the location, the file type, and the dimensions
ggsave("C:/Users/KristinBietsch/files/R Code/2022 Training/CPR Trends.png", width=8, height=8, units = "in")




graph1 <- ggplot(subset(data_long, Country=="Bangladesh"), aes(x=Year, y=Value, color=Country, shape=Type)) +
  geom_point(size=2) +
  geom_line() +
  theme_bw()

ggsave( "C:/Users/KristinBietsch/files/R Code/2022 Training/Bangladesh CPR Trends.png", graph1, width=8, height=8, units = "in")




