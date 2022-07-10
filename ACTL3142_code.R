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


#Average sum insured/quarter 
Sum_insured_total <- Commercial %>%
  group_by(accident_month) %>% 
  summarise(Total_sum_insured = sum(sum_insured),
            No_of_vehicles = sum((sum_insured)*0+1))

Sum_insured_quarterly_permonth <- Sum_insured_total %>%
  mutate(Sum_insured_quarter = as.yearqtr(accident_month, format = "%Y-%m-%d"))

Sum_insured_quarterly <- Sum_insured_quarterly_permonth %>%
  group_by(Sum_insured_quarter) %>%
  summarise(Total_Qsuminsured = (sum(Total_sum_insured)),
            No_of_vehiclesQ = sum(No_of_vehicles), 
            Avg_sum_insured = Total_Qsuminsured/No_of_vehiclesQ)

#Trying to plot total claims cost/ per quarter (compare it with Australia's inflationary data)
total_claims_perMonth <- Commercial %>% 
  group_by(accident_month) %>%
  summarise(Claims_every_AccMonth = sum(na.omit(total_claims_cost)),
            No_claims = sum(na.omit(total_claims_cost)*0 +1))


Claims_per_month <- Commercial %>%
  na.omit(total_claims_cost) %>%
  group_by(accident_month) %>%
  summarise(Number_of_claims = n(), Average_claim_size = mean(total_claims_cost),
            Total_claims = sum(total_claims_cost))
Claims_per_month$accident_month<-as.Date(Claims_per_month$accident_month)

Quarterly_claims <- Claims_per_month %>% 
  group_by(accident_month) %>%
  mutate(Accident_Quarter = as.yearqtr(accident_month, format = "%Y-%m-%d")) %>%
  summarise(Total_QClaim = sum(Total_claims),
            No_claims = sum(Number_of_claims), Average_claim = Total_QClaim/No_claims) %>%
 mutate(Accident_Quarter = as.yearqtr(accident_month, format = "%Y-%m-%d")) 

Quarterly_claims <- Quarterly_claims %>%
  group_by(Accident_Quarter) %>% 
  summarise(Average_claim_quarter = sum(Average_claim))

#Actual plot --> can be compared to the CPI/ inflation per quarter and show a similar trend
ggplot(Quarterly_claims, aes(x = accident_month, y = Total_QClaim)) + 
  geom_line() 

#Claims each Quarter
Quarterly_claim_Freq <- Commercial %>% 
  group_by(accident_month) %>%
  summarise(Claims_no = sum(!is.na(total_claims_cost)))

Claim_freq_Quarter <- Quarterly_claim_Freq %>% 
  mutate(Accident_Quarter = as.yearqtr(accident_month, format = "%Y-%m-%d"))

Quarterly_claim_freq1 <- Claim_freq_Quarter %>%
  group_by(Accident_Quarter) %>%
  summarise(Total_Q_claim_no = sum(Claims_no))

#Actual plot --> can be compared to the movement 
ggplot(Quarterly_claim_freq1, aes(x = Accident_Quarter, y = Total_Q_claim_no)) + 
  geom_line()                                                                      

#Costliest States 

States <- Commercial %>%
  group_by(risk_state_name) %>% 
  summarise(Sum_per_vehicle = (sum(na.omit(total_claims_cost)))/sum(!is.na(total_claims_cost)))

State_Claims <- Commercial %>% 
  group_by(risk_state_name) %>% 
  summarise(Claims_per_state = sum(!is.na(total_claims_cost)))

Number_in_each_class <- Commercial %>%
  group_by(vehicle_class) %>%
  summarise(Each_class = sum(unique(policy_id)) )


# ----- MILESTONE 2 -----

#gonna use new data frame coz other one too big (use select and mutate ig)
#Gonna use CPI coz all papers say that (for claims severity)
#for claims severity <- CPI, wage inflation, CPI medical services too (indices)
#also CPI related to vehicles (indices)
#internal factors <- class, sum insured...

#Can use stepwise regression (multiple regression thing) to determine the main 
#driving factors (i.e. forward selection and backward elimination)
#fit the model on an AIC (week 5 content?) and choose the model that minimises AIC
#The above analysis focuses on attributes that best characterises the cost of the 
#respective line of business (comprehensive commercial vehicle insurnace for us)
#We arent gonna do stochastic stuff to fully predict ig?
#we can do GLM instead of multiple linear as well, with gamma as the dist for residuals

#Need to look into claims freq now :)

# https://docplayer.net/1238431-Assessing-inflation-risk-in-non-life-insurance.html


# ----- MILESTONE 3 -----


Commercial_new <- Commercial %>%
  na.omit(Commercial$total_claims_cost) %>% filter(total_claims_cost > 0)



# tranport CPI source : https://www.fxempire.com/macro/australia/cpi-transportation
# CPI + fuel movement source: 
# https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release
#Importing various CPI indices

CPI <- read.csv("CPI_1.csv", header = T)
Fuel_movement <- read.csv("Automotive fuel Quarterly movement.csv", header = T)
Transport_CPI <- read.csv("Transport CPI.csv", header = TRUE)
JPY_AUD <- read.csv("JPY_AUD.csv", header = T)

# Attaching them to Quarterly Claims severity and frequency (Quarterly Claims and 
# Quarterly claim freq 1) --> for easy GLM later on

GLM_data1 <- cbind(Quarterly_claims, CPI, Fuel_movement, Transport_CPI, JPY_AUD, Avg_sum_insured = Sum_insured_quarterly$Avg_sum_insured)
colnames(GLM_data) <- c("Accident_Quarter", "Average_claim_quarter", "CPI", "Quarterly.Change", "Transport.CPI", "Exchange.Rate", "Avg_sum_insured")

pls_work <- glm(Average_claim_quarter ~ Avg_sum_insured + CPI + Quarterly.Change + Transport.CPI, data = GLM_data,
                family = gaussian(link = "log"))
summary(pls_work)


#JUST INTERNAL VARIABLES
toink <- glm(total_claims_cost ~ sum_insured,
              data = train, family = Gamma(link = "log"))
summary(toink)

