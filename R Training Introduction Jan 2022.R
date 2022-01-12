# R Introduction for Avenir Health
# Janurary 2022
# Kristin Bietsch, PhD, Avenir Health


#################################################
# QUICK TIPS
# First, using the "#" makes everything a comment!
# To run a line of code, click Crtl+Enter
# To run multiple lines of code, highlight the code then click Crtl+Enter
############################################


# To start, some vocabulary
## you can have objects, vectors, data, etc.


# object
object1 <- 3
object2 <- "hello"
object3 <- 2+4
object4 <- 2 + object3
object5 <- 2 + object2


# vector
vector1 <- c(2,4,6)
vector2 <- c(object1, object2)
vector3 <- c(seq(1,5,2))
vector4 <- c(seq(1,7,2), 2000)

# to figure out how many objects are in your vector:
length(vector1)



# Data frames- this is like an excel sheet- rows and columns of data
df1 <- data.frame(vector1, vector3)



#########################################################################
# But now lets have some real fun


# Setting working directory- this is how you refer to a folder on your computer
# Session
# Set working directory
# Choose Directory...

# After you do the drop down option once, you can copy and paste the location from the Console
setwd("C:/Users/KristinBietsch/files/R Code/2022 Training")

#######################################################################
# Loading data
######################################################################
# Read in data- I am telling it what kind of file it is reading
# A .csv file is a common file type- you can save your excel documents as comma seperated values if you wish
df <- read.csv("RIntroData.csv")

# save a CSV
#write.csv(df, file = "MyData.csv")


################################################################################
# Analysis with DPLYR
###########################################################################
# Packages- these are user written programs that help you do things in R
# First you have to install- you all did this as part of your homework
# Then call in using library()

library(tidyverse)

# There are multiple ways to do the same thing
# what percentage difference are the countries' IMR's from the global average of 32 (WHO 2015)?
# The "normal way"
# you have to tell R which dataframe you are refering to, and which variable in that data frame
df$imrdif <- ((df$IMR-32)/32)*100
# Using DPLYR (a part of the tidyverse)
df <- df %>% mutate(imrdif1= ((IMR-32)/32)*100) 

# if else, conditional statements
# using dyplr's case_when
df <- df %>% mutate(imrabove= case_when(IMR>32 ~ 1,
                                        IMR <=32 ~ 0),
                    imrcat = case_when(imrdif>20 ~ "Above", 
                                       imrdif< -20 ~ "Below",
                                       imrdif >= -20 & imrdif<= 20 ~ "Near"))

# one way to count how many countries are in each category using DPLYR
imrcount <- df  %>% count(imrcat)  %>% mutate(prop = prop.table(n))

