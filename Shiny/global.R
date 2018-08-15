library(shiny)
library(dplyr)
# library(tidyverse)
library(stringr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)
library(shinythemes)
library(extrafont) 

#Fix font coding

#windowsFonts("Arial"=windowsFont("TT Arial"))

# Load data
# Assumes you're in the top level of Economic Roundtable's homeless_LA github repository:
# https://github.com/economicroundtable/homeless_LA

# Multi Year Data
multi_year <- read_csv("Final Data and Codebooks/Multi-Year/Homelessness_Multi_Year_Final.csv") %>%
  select(-X1) %>% 
  mutate(Sheltered_Unsheltered = str_split_fixed(string = Survey_Year,
                                                 pattern = " ",
                                                 n = 2)[ , 1],
         Year = str_split_fixed(string = Survey_Year, 
                                pattern = " ", n = 2)[ , 2]) %>% 
  mutate(Year = as.integer(Year)) %>% 
  mutate_if(is.character, as.factor)

# 2016 Unsheltered Data
unsheltered_2016 <- read_csv("Final Data and Codebooks/Unsheltered/Demo_2016_Final.csv") %>% 
  select(-X1)

#data info column
unsheltered_2016$Data <- rep('2016 Unsheltered', nrow(unsheltered_2016))

#categorical age
unsheltered_2016$Age_Categorical <- cut(unsheltered_2016$Age, c(0,17,24,40,50,60,Inf), 
                                        labels = c("0-18", "18-24", "25-40", "41-50","51-60", "60+"))
#Times homeless
unsheltered_2016$Times_Homeless_3yrs <- recode(unsheltered_2016$Times_Homeless_3yrs, "0 times" = "1 Time",
                                               "1 time" = "1 Time", "2 to 3 times" = "2 to 3 Times", 
                                               "4 or more times" = "4 or More Times", "unknown" = "Unknown")
#Current stint
unsheltered_2016$Current_Stint_Duration <- fct_relevel(as.factor(unsheltered_2016$Current_Stint_Duration), 
                                                       "Up to 1 Month", "1-3 Months", "4-5 Months",
                                                       "6-11 Months", "12+ Months", "Unknown")
#Replace NAs
unsheltered_2016$Ethnicity[is.na(unsheltered_2016$Ethnicity)] <- "Unknown"
unsheltered_2016$Gender[is.na(unsheltered_2016$Gender)] <- "Unknown"

#Location Recode
unsheltered_2016$Outside_Majority_Last_Month <- recode(unsheltered_2016$Outside_Majority_Last_Month,
                                                       "Car or truck" = "Car", 
                                                       "Nonhabitable building or under bridge" = "Under bridge",
                                                       "Other outdoor location" = "Outdoor- unspecified",
                                                       "Park, beach, riverbed, woods" = "Park/Beach",
                                                       "Street, sidewalk, or alley" = "Street",
                                                       "Outdoor encampment or tent or makeshift shelter" = "Tent")

#Employment variables rename
unsheltered_2016 <- unsheltered_2016 %>% rename(Job_Unemployed_Looking = Unemployed_Looking,
                                                Job_Unemployed_Not_Looking = Unemployed_Not_Looking,
                                                Job_Full_Time = Full_Time,
                                                Job_Part_Time = Part_Time,
                                                Job_Seasonal = Seasonal,
                                                Job_Panhandling = Panhandling,
                                                Job_Recycling = Recycling,
                                                Job_Day_Labor = Day_Labor)

#2017 Unsheltered Data
unsheltered_2017 <- read_csv("Final Data and Codebooks/Unsheltered/Demo_2017_Final.csv") %>% 
  select(-X1)

#data info column
unsheltered_2017$Data <- rep('2017 Unsheltered', nrow(unsheltered_2017))

#categorical age
unsheltered_2017$Age_Categorical <- cut(unsheltered_2017$Age, c(0,17,24,40,50,60,Inf), 
                                        labels = c("0-18", "18-24", "25-40", "41-50","51-60", "60+"))
#Current stint
unsheltered_2017$Current_Stint_Duration <- fct_relevel(as.factor(unsheltered_2017$Current_Stint_Duration), 
                                                       "Up to 1 Month", "1-3 Months", "4-5 Months",
                                                       "6-11 Months", "12+ Months", "Unknown")
unsheltered_2017$Current_Stint_Duration[is.na(unsheltered_2017$Current_Stint_Duration)] <- "Unknown"

#Location Recode
unsheltered_2017$Outside_Majority_Last_Month <- recode(unsheltered_2017$Outside_Majority_Last_Month,
                                                       "Car or truck" = "Car", 
                                                       "Nonhabitable building or under bridge" = "Under bridge",
                                                       "Other outdoor location" = "Outdoor- unspecified",
                                                       "Park, beach, riverbed, woods" = "Park/Beach",
                                                       "Street, sidewalk, or alley" = "Street",
                                                       "Outdoor encampment or tent or makeshift shelter" = "Tent")
#Employment variables rename
unsheltered_2017 <- unsheltered_2017 %>% rename( Job_Unemployed_Looking = Unemployed_Looking,
                                                 Job_Unemployed_Not_Looking = Unemployed_Not_Looking,
                                                 Job_Full_Time = Full_Time,
                                                 Job_Part_Time = Part_Time,
                                                 Job_Seasonal = Seasonal,
                                                 Job_Unable_To_Work_Disability = Unable_To_Work_Disability,
                                                 Job_Temp_Work = Temp_Work,
                                                 Job_Retired = Retired,
                                                 Job_Student = Student_Vocational,
                                                 Job_Panhandling = Panhandling,
                                                 Job_Recycling = Recycling,
                                                 Job_Day_Labor = Day_Labor,
                                                 Job_Performance = Performance,
                                                 Job_Sex = Sex_Worker)

#2016 Sheltered Data 
sheltered_2016 <- read_csv("Final Data and Codebooks/Sheltered/HMIS_2016_Final.csv")

#data info column
sheltered_2016$Data <- rep('2016 Sheltered', nrow(sheltered_2016))

#categorical age
sheltered_2016$Age_Categorical <- cut(sheltered_2016$Age, c(0,17,24,40,50,60,Inf), 
                                      labels = c("0-18", "18-24", "25-40", "41-50","51-60", "60+"))
#Current Stint
sheltered_2016$Current_Stint_Duration <- recode(sheltered_2016$Current_Stint_Duration, 
                                                "Less than 12 months- unspecified" = "Less than 12 months")
sheltered_2016$Current_Stint_Duration <- fct_relevel(as.factor(sheltered_2016$Current_Stint_Duration), 
                                                               "Less than 12 months")
#2017 Sheltered Data
sheltered_2017 <- read_csv("Final Data and Codebooks/Sheltered/HMIS_2017_Final.csv")

#data info column
sheltered_2017$Data <- rep('2017 Sheltered', nrow(sheltered_2017))

#categorical age
sheltered_2017$Age_Categorical <- cut(sheltered_2017$Age, c(0,17,24,40,50,60,Inf), 
                                        labels = c("0-18", "18-24", "25-40", "41-50","51-60", "60+"))
#Current Stint
sheltered_2017$Current_Stint_Duration <- recode(sheltered_2017$Current_Stint_Duration, "12+ months" = "12+ Months")
sheltered_2017$Current_Stint_Duration <- fct_relevel(as.factor(sheltered_2017$Current_Stint_Duration),  
                                                     "Up to 1 Month", "1-3 Months", "4-5 Months",
                                                     "6-11 Months", "12+ Months", "Unknown")

#Color Palette

ERT_palette <- c(rgb(155,187,89, maxColorValue=255), 
                 rgb(79,129,189, maxColorValue=255), 
                 rgb(238,236,225, maxColorValue=255),
                 rgb(247,150,70, maxColorValue=255),
                 rgb(75,172,198, maxColorValue=255),  
                 rgb(192,80,77, maxColorValue=255),
                 rgb(128,100,62, maxColorValue=255))


ERT_palette_2 <- c(rgb(155,187,89, maxColorValue=255), 
                 rgb(79,129,189, maxColorValue=255), 
                 rgb(75,172,198, maxColorValue=255),
                 rgb(238,236,225, maxColorValue=255),
                 rgb(247,150,70, maxColorValue=255),
                 rgb(192,80,77, maxColorValue=255),
                 rgb(128,100,62, maxColorValue=255))