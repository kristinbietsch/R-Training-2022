# Reading a PDF Table in R 

# Kristin Bietsch, PhD
# Track20 Project, Avenir Health

# We use 3 libraries to request and clean data

library(tidyverse)
library(tabulizer)
library(staplr)

options(scipen=999)
memory.limit(size = 2e6) 


#############################################################################
# In this PDF, the pages I want need to be rotated:
input_path <- file.path('C:/Users/KristinBietsch/files/R Code/2022 Training/Mali 2015 MICS Final Report_French.pdf')
output_path <-  file.path('C:/Users/KristinBietsch/files/R Code/2022 Training/Mali 2015 MICS Final Report_French_Rotated.pdf')
rotate_pages(rotatepages = c(181,182), page_rotation = 90,  input_path, output_path)
#############################################################################


# Location of  pdf file
location <- 'C:/Users/KristinBietsch/files/R Code/2022 Training/Mali 2015 MICS Final Report_French_Rotated.pdf'


# look up the extents for each table here
txt1 <- locate_areas(location, pages=181)
# Click on txt1 in right hand side to get extents- want to be outside the extents, so smaller than the first two numbers and larger than the last two
txt2 <- locate_areas(location, pages=185)

table1 <- extract_tables(
  location,
  output = "data.frame",
  pages = c(181), 
  area = list(
    c(170, 40, 750, 800) ),
  guess = FALSE
)

table2 <- extract_tables(
  location,
  output = "data.frame",
  pages = c(185), 
  area = list(
    c(300, 50, 800, 550) ),
  guess = FALSE
)



table1_clean <- reduce(table1, bind_rows) %>% 
  rename(Group=1, None=2, FSter=3, MSter=4, IUD=5, Implant=6, Injectable=7, Pill=8,
         MCondom=9, FCondom=10, Diaphragm=11, LAM=12, Rhythm=13, Withdrawal=14,
         Other=15, Missing=16, Modern=17, Traditional=18, CPR=19) %>% # rename columns
  filter(Group!="") %>% # getting rid of extra lines
  filter(None!="") %>% # getting rid of extra lines 
  select(-actuellement) %>% # getting rid of column with N
  mutate(Group=case_when(Group=="15-19"  ~ "Age: 15-19" ,
                         Group==  "20-24" ~  "Age: 20-24",
                         Group=="25-29"  ~ "Age: 25-29" ,
                         Group== "30-34"  ~  "Age: 30-34" ,
                         Group== "35-39" ~  "Age: 35-39",
                         Group== "40-44" ~  "Age: 40-44",
                         Group== "45-49"   ~ "Age: 45-49"   ,
                         Group==  "Bamako"  ~  "Region: Bamako"  ,
                         Group== "Gao"   ~  "Region: Gao"  ,
                         Group== "Kayes"  ~  "Region: Kayes" ,
                         Group== "Koulikoro"~ "Region: Koulikoro",
                         Group== "Mopti" ~  "Region: Mopti" ,
                         Group==  "Rural" ~  "Residence: Rural" ,
                         Group==   "Ségou" ~   "Region: Segou" ,
                         Group==  "Sikasso" ~  "Region: Sikasso" ,
                         Group== "Tombouctou" ~ "Region: Tombouctou" ,
                         Group==  "Total"  ~  " Total"  ,
                         Group== "Urbain" ~  "Residence: Urbain")) %>% # Cleaning up group names
  gather(Variable, Value, None:CPR) %>% # the next few lines are to swap the "," with "." and have read as numeric
  mutate(Value=as.numeric(as.character(str_replace_all(Value, "[,]", ".")))) %>%
  spread(Variable, Value)

#write.csv(table1_clean, "C:/Users/KristinBietsch/files/Track20/Reading MICS Tables in R/Mali 2015 CPR Data.csv", row.names = F, na="")

# Use this to find the names of the group
levels(as.factor(table1_clean$Group))

table2_clean <- reduce(table2, bind_rows) %>%
  rename(Group=1, Sat_SpaceLim=2, Sat_Total=3, UN_Space=5, UN_Limit=6, UN_Total=7, DS=9) %>% # rename columns
  filter(Sat_SpaceLim!="") %>%
  separate(Sat_SpaceLim, c("Sat_Space","Sat_Limit"), " ") %>%
  select(Group, Sat_Space, Sat_Limit, Sat_Total, UN_Space, UN_Limit, UN_Total, DS) %>%
  mutate(Group=case_when(Group=="15-19"  ~ "Age: 15-19" ,
                         Group==  "20-24" ~  "Age: 20-24",
                         Group=="25-29"  ~ "Age: 25-29" ,
                         Group== "30-34"  ~  "Age: 30-34" ,
                         Group== "35-39" ~  "Age: 35-39",
                         Group== "40-44" ~  "Age: 40-44",
                         Group== "45-49"   ~ "Age: 45-49"   ,
                         Group==  "Bamako"  ~  "Region: Bamako"  ,
                         Group== "Gao"   ~  "Region: Gao"  ,
                         Group== "Kayes"  ~  "Region: Kayes" ,
                         Group== "Koulikoro"~ "Region: Koulikoro",
                         Group== "Mopti" ~  "Region: Mopti" ,
                         Group==  "Rural" ~  "Residence: Rural" ,
                         Group==   "Ségou" ~   "Region: Segou" ,
                         Group==  "Sikasso" ~  "Region: Sikasso" ,
                         Group== "Tombouctou" ~ "Region: Tombouctou" ,
                         Group==  "Total"  ~  " Total"  ,
                         Group== "Urbain" ~  "Residence: Urbain")) %>% # Cleaning up group names
  filter(Group!="")  %>% # getting rid of extra lines
  gather(Variable, Value, Sat_Space:DS) %>% # the next few lines are to swap the "," with "." and have read as numeric
  mutate(Value=as.numeric(as.character(str_replace_all(Value, "[,]", ".")))) %>%
  spread(Variable, Value)


#write.csv(table2_clean, "C:/Users/KristinBietsch/files/Track20/Reading MICS Tables in R/Mali 2015 Unmet Need Data.csv", row.names = F, na="")
