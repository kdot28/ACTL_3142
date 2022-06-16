#Loading all the necessary packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(tidyselect)
library(usethis)

Commercial <- read.csv("ACTL3142Data.csv") #import the dataset


Insurance_by_ID <- Commercial %>% 
  group_by(policy_id) %>% 
  summarise(PolicyDuration_Months = n())

max(Insurance_by_ID$PolicyDuration_Months)
min(Insurance_by_ID$PolicyDuration_Months)
mean(Insurance_by_ID$PolicyDuration_Months)
median(Insurance_by_ID$PolicyDuration_Months)

Claims_cost_byID <- Commercial %>%
  group_by(policy_id) %>% filter(total_claims_cost, na.rm()) %>%
  summarise(Total_cost = sum(total_claims_cost))#trying to get amnt of claims each ID

#heloooooooo

#Karan's pushing

#THIS IS A SIMULATION. WE ARE NOT REAL. 

#trying to puussshhhs

#Trial

#Comments to self - Getting accident count could be useful, 
#therefore sorting by policy id could be useful (how many accidents each vehicle has had)
# --> see patterns? idk need to understand the data properly!

#could compare postcodes as well? e.g. a certain postcode in a certain state could have more accidents 

#could also compare number of accidents given the year of production of each vehicle 

#could also conduct state wise analysis (i.e. NSW vehicles are more prone to VIC vehicles 
#i.e. have higher claims)

#can look ar externals as well (e.g. inflation rate, and thus relate it to insurance amount and frequency
#note: both are to be considered as mentioned in the assignment brief)
