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
Sum_insured_total <- Commercial %>%
  group_by(accident_month) %>% 
  summarise(Total_sum_insured = sum(sum_insured),
            No_of_vehicles = sum((sum_insured)*0+1))

Sum_insured_permonth <- Sum_insured_total %>%
  mutate(Sum_insured_month = as.yearmon(accident_month, format = "%Y-%m-%d"))

Sum_insured_Monthly <- Sum_insured_permonth %>%
  group_by(Sum_insured_month) %>%
  summarise(Total_Qsuminsured = (sum(Total_sum_insured)),
            No_of_vehiclesQ = sum(No_of_vehicles), 
            Avg_sum_insured = Total_Qsuminsured/No_of_vehiclesQ)

#plotting total claims cost/ per month - Claim Severity
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

Monthly_claims <- Claims_per_month %>% 
  group_by(accident_month) %>%
  mutate(Accident_Month0 = as.yearmon(accident_month, format = "%Y-%m-%d")) %>%
  summarise(Total_QClaim = sum(Total_claims),
            No_claims = sum(Number_of_claims), Average_claim = Total_QClaim/No_claims) %>%
 mutate(Accident_Month0 = as.yearmon(accident_month, format = "%Y-%m-%d")) 

Monthly_claims <- Monthly_claims %>%
  group_by(Accident_Month0) %>% 
  summarise(Average_claim_month = sum(Average_claim))

#Actual plot --> can be compared to the CPI/ inflation per quarter and show a similar trend
ggplot(Monthly_claims, aes(x = Accident_Month0, y = Average_claim_month)) + 
  geom_line() 

#Claims each month (Not claims frequency)
Monthly_claim_Freq <- Commercial %>% 
  group_by(accident_month) %>%
  summarise(Claims_no = sum(!is.na(total_claims_cost)))

Claim_freq_Monthly <- Monthly_claim_Freq %>% 
  mutate(Accident_Month1 = as.yearmon(accident_month, format = "%Y-%m-%d"))

Monthly_claim_freq1 <- Claim_freq_Monthly %>%
  group_by(Accident_Month1) %>%
  summarise(Total_Q_claim_no = sum(Claims_no))

#Actual plot --> can be compared to the movement 
ggplot(Monthly_claim_freq1, aes(x = Accident_Month1, y = Total_Q_claim_no)) + 
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
#we can do GLM instead of multiple linear as well, with gamma as the dist for residuals

#Need to look into claims freq now :)

# https://docplayer.net/1238431-Assessing-inflation-risk-in-non-life-insurance.html


# ----- MILESTONE 3 -----


Commercial_new <- Commercial %>%
  na.omit(Commercial$total_claims_cost) %>% filter(total_claims_cost > 0) %>%
  mutate(Vehicle_age = 2022 - year_of_manufacture) %>% filter(Vehicle_age > 0)



# tranport CPI source : https://www.fxempire.com/macro/australia/cpi-transportation
# CPI + fuel movement source: 
# https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release
#Importing various CPI indices

Iron_steel_Imports <- read.csv("Iron Steel Import.csv", header = T)
Oil_production <- read.csv("Crude Oil Production.csv", header = T)
Transport_Parts_Imports <- read.csv("Transport Parts Import.csv", header = T)
Transport_equip_machinery <- read.csv("Machinery TransEquip Imports.csv", header = T)

# Attaching them to Monthly Claims severity and frequency (Monthly Claims and 
# Monthly claim freq 1) --> for easy GLM later on


# ---- SEVERITY ----
#gotta split the data ig

#JUST INTERNAL VARIABLES (maybe Externals -> so have data frame ready)

GLM_data1 <- cbind(Quarterly_claims, CPI, Fuel_movement, Transport_CPI, JPY_AUD, 
                   Avg_sum_insured = Sum_insured_quarterly$Avg_sum_insured)
colnames(GLM_data1) <- c("Accident_Quarter", "Average_claim_quarter", "CPI", 
                        "Quarterly.Change", "Transport.CPI", "Exchange.Rate", 
                        "Avg_sum_insured")

#Splitting the data (from Commercial_new) -> For validation testing? 

#Building the Severity Model (Internal Factors only)

############################ VEHICLE CLASS INCLUSION
#IF we wanna included the fattest vehicles into claims severity
fat_vehicle <- Commercial_new$vehicle_class
fat_vehicle2 <- seq(1:length(fat_vehicle))

for (i in 1:length(fat_vehicle)) {
  if (fat_vehicle[i] == "Class 10"|fat_vehicle[i] == "Class 11"|fat_vehicle[i] == "Class 8") {
    fat_vehicle2[i] <- 1
  } else {
    fat_vehicle2[i] <- 0
  }
}

Commercial_new <- Commercial_new %>%
  cbind(fat_vehicle2)
############################### GLM SEVERITY
glm_sev <- glm(total_claims_cost ~ sum_insured + Vehicle_age + policy_tenure + fat_vehicle2, 
                  family = Gamma(link = "log"), data = Commercial_new)



summary(glm_sev)
set.seed(1010)
kfold_error_10 <- rep(0,10)
for (i in 1:10) {glm_sev
  kfold_error_10[i] <- cv.glm(Commercial_new, glm_sev, K = 10)$delta[1]
}
kfold_error_10
mean((Commercial_new$total_claims_cost - predict.glm(glm_sev))^2)/10

# ---- Frequency ----
#Creating Claim frequency thing
Claims_freq_1 <- Commercial_new %>% 
  group_by(accident_month) %>%
  summarise(Claims_no = sum(!is.na(total_claims_cost)), exposure_1 = sum(exposure))

Claims_freq_2 <- Claims_freq_1 %>% 
  mutate(Accident_Month2 = as.yearmon(accident_month, format = "%Y-%m-%d"))

Claims_freq_3 <- Claims_freq_2 %>%
  group_by(Accident_Month2) %>%
  summarise(Claims_Frequency = sum(Claims_no), expo = sum(exposure_1))

Claims_freq_4 <- Claims_freq_3 %>% 
  group_by(Accident_Month2) %>%
  summarise(A_claims_freq = Claims_Frequency/expo)

#Graph for claims frequency
Actual_claims_freq <- ggplot(data = Claims_freq_4, 
             aes(Accident_Month2, A_claims_freq))+ geom_line() + labs(x = "Accident Month",
                                                                      y = "Claims Freq")
Actual_claims_freq 

GLM_data2 <- cbind(Claims_freq_4,Iron_steel_Imports,Oil_production,
                   Transport_Parts_Imports, Transport_equip_machinery,
                   Avg_sum_insured = Sum_insured_Monthly$Avg_sum_insured)
GLM_data3 <- subset(GLM_data2, select = -c(X, X.1, X.2, X.3, X.4, X.5))
colnames(GLM_data3) <- c("Accident Month", "Claims_Freq", "Iron_Steel_Import", 
                         "Oil_Production", "Transport_Parts_Import", 
                         "Transport_Machinery_Import", "Average_Sum_Insured")
GLM_data3$Claims_Freq <- as.integer(GLM_data3$Claims_Freq)
GLM_data3$Iron_Steel_Import <- as.integer(GLM_data3$Iron_Steel_Import)
GLM_data3$Oil_Production <- as.integer(GLM_data3$Oil_Production)
GLM_data3$Transport_Parts_Import <- as.integer(GLM_data3$Transport_Parts_Import)
GLM_data3$Transport_Machinery_Import <- as.integer(GLM_data3$Transport_Machinery_Import)
GLM_data3$Average_Sum_Insured <- as.integer(GLM_data3$Average_Sum_Insured)



glm_freq <- glm(Claims_Freq ~  Iron_Steel_Import + Oil_Production +
                  Transport_Parts_Import + Transport_Machinery_Import +
                  Average_Sum_Insured, 
                 data = GLM_data3, 
                family = poisson(link = "log"),
                offset = (Claims_freq_3$expo))
summary(glm_freq)

set.seed(10101)
kfold_error_10_a <- rep(0,10)
for (j in 1:10) {glm_freq
  kfold_error_10_a[j] <- cv.glm(GLM_data3, glm_freq, K = 10)$delta[1]
}
freq_k_fold_error <- mean((GLM_data3$Claims_Freq - predict.glm(glm_freq))^2)/10
freq_k_fold_error

freq_fit <- 



#