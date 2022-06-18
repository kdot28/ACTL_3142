#Loading all the necessary packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(tidyselect)
library(usethis)
library(skimr)

#import the dataset
Commercial <- read.csv("ACTL3142Data.csv") 
attach(Commercial)

#changing qualitative variables to factors
vehicle_class<-as.character(vehicle_class)
risk_state_name<-as.factor(risk_state_name)
claim_loss_date<-as.Date(claim_loss_date)
term_start_date<-as.Date(term_start_date)
term_expiry_date<-as.Date(term_expiry_date)

#Conducting basic data quality checking using skimr package

Commercial %>% skim()

#EDA (created new datasets and data visualisation)

Insurance_by_ID <- Commercial %>% 
  group_by(policy_id) %>% 
  summarise(PolicyDuration_Months = n(), State = unique(risk_state_name)) 

By_manufacture_year <- Commercial %>% 
  group_by(year_of_manufacture) %>%
  summarise(No_vehicles = sum(1+0*unique(policy_id)))

max(Insurance_by_ID$PolicyDuration_Months)
min(Insurance_by_ID$PolicyDuration_Months)
mean(Insurance_by_ID$PolicyDuration_Months)
median(Insurance_by_ID$PolicyDuration_Months)

Claims_by_class <- Commercial %>%
  group_by(vehicle_class) %>%
  summarise(No_claims = sum(!is.na(total_claims_cost)),
            Ave_Claim_Amt = mean(na.omit(total_claims_cost)))

#Data Visualisation

#Barplot of number of claims/class (add title etc.)
No_claims_vs_Class <- ggplot(Claims_by_class, aes(x=vehicle_class, y= No_claims)) + 
  geom_bar(stat = "identity") 

#Barplot of Avg claim cost/class (add title etc.)
AveCost_vs_Class <- ggplot(Claims_by_class, aes(x=vehicle_class, y=Ave_Claim_Amt)) + 
  geom_bar(stat = "identity") 

#Barplot of No.cars per manufature year
barplot(By_manufacture_year$No_vehicles, names.arg = By_manufacture_year$year_of_manufacture)

min(Commercial$year_of_manufacture)
max(Commercial$year_of_manufacture)
mean(Commercial$year_of_manufacture)


# POLICY ID THAT HAS TWO STATES
TWOSTATEENTRY <- Commercial[policy_id == 32422,]

#Number of policyholders per state
PH_per_state <- table(Insurance_by_ID$State)
PH_per_state

#could compare postcodes as well? e.g. a certain postcode in a certain state could have more accidents 

#could also compare number of accidents given the year of production of each vehicle 

#could also conduct state wise analysis (i.e. NSW vehicles are more prone to VIC vehicles 
#i.e. have higher claims)

#can look ar externals as well (e.g. inflation rate, and thus relate it to insurance amount and frequency
#note: both are to be considered as mentioned in the assignment brief)
