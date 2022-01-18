# Using APIs in R (DHS, UN Sustainable Development Goals, World Bank)

# Kristin Bietsch, PhD
# Track20 Project, Avenir Health

# We use 3 libraries to request and clean data
library(jsonlite) 
library(data.table)
library(tidyverse)
library(wbstats)
library(xlsx)

setwd("C:/Users/KristinBietsch/files/R Code/2022 Training")

countryiso <- read.csv("Country Regions.csv")

surveys <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                      colClasses = c("character", "character", "character", "numeric",
                                     "character", "character", "character", "character", "numeric",
                                     "character", "numeric", "character", "character", "character", "character"));


#####################################################################################################################################
##########################
### DHS                ###
##########################

# The DHS API is a quick and useful way to access many DHS indicators.  It contains similar information as StatCompiler (https://www.statcompiler.com/en/)  


###########################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# For this exercise, we will be requesting a large amount of information.  
# You are going to request a large amount of data, 
# you can register for a "key" by emailing api@dhsprogram.com
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
###########################################################

# To access data, you will need an indicator ID (https://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition&f=html)

# First, we set up an object called "url"
# For a specific indicator, we are changing the indicatorID, in the example below "FP_CUSA_W_MOD"
# You will need to add your key in the blank space after "APIkey="
# We are requesting all surveys and all breakdowns 
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=FP_CUSA_W_MOD&surveyid=all&breakdown=all&perpage=20000&APIkey=")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)

# Now in our dataframe "dta" we can select the columns of interest.  For this example, we want mCP by wealth quintile
dta_small<- dta %>% select( SurveyId, CountryName, SurveyYear, Value,   CharacteristicCategory, CharacteristicLabel)  %>% # Keep the columns of interest
  filter(CharacteristicCategory=="Wealth quintile")

# If you want your data to be in wide format (each wealth quintile as its own column)
# want to add ISO Number
dta_wide <- dta_small %>%
  spread(CharacteristicLabel, Value) %>%
  group_by(CountryName) %>%
  mutate(recent=max(SurveyYear)) %>% ungroup() %>%
  filter(SurveyYear==recent) %>%
  rename(API_ID=SurveyId) %>%
  left_join(surveys, by="API_ID") %>%
  select(CountryName, SurveyYear, ISONum, Lowest, Second, Middle, Fourth, Highest)


names(dta_wide)
#####################################################################################################################################
##########################
### UN SDGs            ###
##########################

# Find indicators IDs: https://unstats.un.org/sdgs/unsdg
# Click on "Select indicators and country or area"
# Once you get to the actual indicator, the API ID is in bold


# SDG 5.6.2 SRHRR legal and regulatory environment	Section 4 (HIV and HPV)
url <- c("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=SH_LGR_ACSRHES4&pageSize=5000")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$data)

# When cleaning the data, I want to select the most recent year
SH_LGR_ACSRHES4 <-select(dta, geoAreaCode, value , timePeriodStart) %>% group_by(geoAreaCode) %>% mutate(Recent=max(timePeriodStart)) %>% 
  filter(timePeriodStart==Recent) %>% select(-Recent) %>%
  mutate(value=as.numeric(as.character(value))) %>% 
  rename(ISONum=geoAreaCode, SH_LGR_ACSRHES4=value, SH_LGR_ACSRHES4_Year=timePeriodStart) %>%
  mutate(ISONum=as.numeric(as.character(ISONum))) 

#####################################################################################################################################
##########################
### World Bank         ###
##########################

# You can download a full dataset of indicators in R:
wb_ind <- wb_cachelist$indicators

# gwp1_n_5	Physical Points of Service	Access to a mobile phone (% ages 15-34)

gwp1_n_5_full <- wb_data(
  indicator = "gwp1_n_5",
  start_date = 2010, end_date = 2020) 

gwp1_n_5 <- gwp1_n_5_full %>%
  filter(!is.na(gwp1_n_5)) %>% 
  group_by(iso3c) %>%
  mutate(recent=max(date)) %>% ungroup() %>% 
  filter(date==recent) %>% select(iso3c, date, gwp1_n_5) %>%
  rename(ISOAlph=iso3c) %>% 
  full_join(countryiso, by="ISOAlph") %>%
  select(ISONum, gwp1_n_5, date) %>% filter(!is.na(gwp1_n_5)) %>%
  rename(gwp1_n_5_Year=date)
