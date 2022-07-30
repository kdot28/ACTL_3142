# ------ MILESTONE 1 -------

rm(list = ls())

#Loading all the necessary packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(tidyselect)
library(usethis)
library(skimr)
library(zoo)
library(caTools)
library(caret)
library(boot)
library(MASS)
library(pscl)
library(AER)
library(VGAM)
#import the dataset
Commercial <- read.csv("ACTL3142Data.csv") 

#changing qualitative variables to factors
Commercial$vehicle_class<-as.character(Commercial$vehicle_class)
Commercial$risk_state_name<-as.factor(Commercial$risk_state_name)
Commercial$claim_loss_date<-as.Date(Commercial$claim_loss_date)
Commercial$term_start_date<-as.Date(Commercial$term_start_date)
Commercial$term_expiry_date<-as.Date(Commercial$term_expiry_date)
Commercial$accident_month <- as.Date(Commercial$accident_month)

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

x = c("class1", "class2", "class3", 
                  "class4", "class5", "class6", 
                  "class7", "class8", "class9", 
                  "class10", "class11", "class12",
                  "class13","class14", "class15")

Claims_by_class <- Commercial %>%
  group_by(vehicle_class) %>%
  summarise(No_claims = sum(!is.na(total_claims_cost)),
            Ave_Claim_Amt = mean(na.omit(total_claims_cost)))


#Average sum insured/month
Sum_insured_Monthly <- Commercial %>%
  group_by(accident_month) %>% 
  summarise(Total_sum_insured = sum(sum_insured),
            No_of_vehicles = sum((sum_insured)*0+1))

Sum_insured_Monthly <- Sum_insured_Monthly %>%
  mutate(Sum_insured_month = as.yearmon(accident_month, format = "%Y-%m-%d"))

Sum_insured_Monthly <- Sum_insured_Monthly %>%
  group_by(Sum_insured_month) %>%
  summarise(Total_Qsuminsured = (sum(Total_sum_insured)),
            No_of_vehiclesQ = sum(No_of_vehicles), 
            Avg_sum_insured = Total_Qsuminsured/No_of_vehiclesQ)

#plotting total claims cost/ per month - Claim Severity
Claims_per_month <- Commercial %>% 
  group_by(accident_month) %>%
  summarise(Claims_every_AccMonth = sum(na.omit(total_claims_cost)),
            No_claims = sum(na.omit(total_claims_cost)*0 +1))


Claims_per_month <- Commercial %>%
  na.omit(total_claims_cost) %>%
  group_by(accident_month) %>%
  summarise(Number_of_claims = n(), Average_claim_size = mean(total_claims_cost),
            Total_claims = sum(total_claims_cost))
Claims_per_month$accident_month<-as.Date(Claims_per_month$accident_month)

Monthly_claims <- Claims_per_month %>% 
  group_by(accident_month) %>%
  mutate(Accident_Month0 = as.yearmon(accident_month, format = "%Y-%m-%d")) %>%
  summarise(Total_QClaim = sum(Total_claims),
            No_claims = sum(Number_of_claims), Average_claim = Total_QClaim/No_claims) %>%
 mutate(Accident_Month0 = as.yearmon(accident_month, format = "%Y-%m-%d")) 

Monthly_claims <- Monthly_claims %>%
  group_by(Accident_Month0) %>% 
  summarise(Average_claim_month = sum(Average_claim))

#Actual plot for claims severity 
ggplot(Monthly_claims, aes(x = Accident_Month0, y = Average_claim_month)) + 
  geom_line() 

#Claims each month (Not claims frequency)
Monthly_claim_count<- Commercial %>% 
  group_by(accident_month) %>%
  summarise(Claims_no = sum(!is.na(total_claims_cost)))

Monthly_claim_count <- Monthly_claim_count %>% 
  mutate(Accident_Month1 = as.yearmon(accident_month, format = "%Y-%m-%d"))

Monthly_claim_count <- Monthly_claim_count %>%
  group_by(Accident_Month1) %>%
  summarise(Total_Q_claim_no = sum(Claims_no))

#Actual plot --> can be compared to the movement 
ggplot(Monthly_claim_count, aes(x = Accident_Month1, y = Total_Q_claim_no)) + 
  geom_line()                                                                      

#Costliest States 

States <- Commercial %>%
  group_by(risk_state_name) %>% 
  summarise(Sum_per_vehicle = (sum(na.omit(total_claims_cost)))/sum(!is.na(total_claims_cost)))

State_Claims <- Commercial %>% 
  group_by(risk_state_name) %>% 
  summarise(Claims_per_state = sum(!is.na(total_claims_cost)))

# number of vehicles in each class
Number_in_each_class <- Commercial %>%
  group_by(vehicle_class) %>%
  summarise(Each_class = sum(unique(policy_id)) )


# ----- MILESTONE 2 -----
#done on google docs

# https://docplayer.net/1238431-Assessing-inflation-risk-in-non-life-insurance.html




# Data for > $0 claims (coz $0 claims are kind of irrelevant since the task is
# predict claims inflation)
Commercial_new <- Commercial %>%
  na.omit(Commercial$total_claims_cost) %>% filter(total_claims_cost > 0) %>%
  mutate(Vehicle_age = 2022 - year_of_manufacture) %>% filter(Vehicle_age >= 0)



# tranport CPI source : 
# https://www.fxempire.com/macro/australia/cpi-transportation


# CPI + fuel movement source: 
# https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release





# VEHICLE CLASS INCLUSION
#IF we wanna included the fattest vehicles into claims severity
vehicle_class <- Commercial_new$vehicle_class
vehicle_level <- seq(1:length(vehicle_class))

for (i in 1:length(vehicle_class)) {
  if (vehicle_class[i] == "Class 10"|vehicle_class[i] == "Class 11"|vehicle_class[i] == "Class 8") {
    vehicle_level[i] <- 1
  } else {
    vehicle_level[i] <- 0
  }
}

Commercial_new <- Commercial_new %>%
  cbind(vehicle_level)

#  Frequency 

# NUMBER of policies active per month
df <- data.frame(month = Claims_freq_1$Accident_Month2, no_active_policies = 0)
df$no_active_policies<- table(Commercial$accident_month)













