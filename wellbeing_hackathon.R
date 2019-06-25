#Name: Saeesh Mangwani
#Date: 21 June 2019
#Desc: Community Wellbeing Index Hackathon

#Importing libraries 
library(readr)
library(readxl)
library(tidyverse)
library(modelr)
library(skimr)

#Cleaning
cwb <- read.csv("PODS/datasets/cwb2016.csv") #DV 
names(cwb) <- c("CSDUID", 
                "csd_name",
                "census_pop",
                "income",
                "education",
                "housing",
                "labour_force",
                "cwb",
                "community_type")

csd <- read.csv("PODS/datasets/census_subdivision_canada.csv")

#join 
new.data <- left_join(cwb, csd, by = "CSDUID")

#Exploratory Analysis 


#Exploring IV 


yeet_teamt_TableToExcel.xls")
View(map_data)

wellbeing <- wellbeing %>%
  mutate("CSDUID" = CSUID, CSUID = NULL)

#joining the dataset
map_data$CSDUID <- as.numeric(map_data$CSDUID)
cwb <- left_join(map_data, wellbeing, by = "CSDUID")
View(cwb)


