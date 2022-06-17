#Loading all the necessary packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(tidyselect)
library(usethis)

#import the dataset
Commercial <- read.csv("ACTL3142Data.csv") 
attach(Commercial)

#changing qualitative variables to factors
vehicle_class<-as.character(vehicle_class)
risk_state_name<-as.factor(risk_state_name)

Insurance_by_ID <- Commercial %>% 
  group_by(policy_id) %>% 
  summarise(PolicyDuration_Months = n())

max(Insurance_by_ID$PolicyDuration_Months)
min(Insurance_by_ID$PolicyDuration_Months)
mean(Insurance_by_ID$PolicyDuration_Months)
median(Insurance_by_ID$PolicyDuration_Months)

#EDA

Claims_by_class <- Commercial %>%
  group_by(vehicle_class) %>%
  summarise(No_claims = sum(!is.na(total_claims_cost)),
            Ave_Claim_Amt = mean(na.omit(total_claims_cost)))

 
No_claims_vs_Class <- ggplot(Claims_by_class, aes(x=vehicle_class, y= No_claims)) + 
  geom_bar(stat = "identity") #Barplot of number of claims/class (add title etc.)

AveCost_vs_Class <- ggplot(Claims_by_class, aes(x=vehicle_class, y=Ave_Claim_Amt)) + 
  geom_bar(stat = "identity") #Barplot of Avg claim cost/class (add title etc.)





#could compare postcodes as well? e.g. a certain postcode in a certain state could have more accidents 

#could also compare number of accidents given the year of production of each vehicle 

#could also conduct state wise analysis (i.e. NSW vehicles are more prone to VIC vehicles 
#i.e. have higher claims)

#can look ar externals as well (e.g. inflation rate, and thus relate it to insurance amount and frequency
#note: both are to be considered as mentioned in the assignment brief)
