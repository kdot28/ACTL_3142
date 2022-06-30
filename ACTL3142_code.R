#Loading all the necessary packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(tidyselect)
library(usethis)
library(skimr)
library(zoo)

#import the dataset
Commercial <- read.csv("ACTL3142Data.csv") 
attach(Commercial) 

Inflation <- read.csv("Inflation.csv", header = TRUE)
attach(Inflation)


Inflation$Quarter <- as.yearqtr(as.Date(Inflation$Quarter))
Inflation$Percentage.Change <- as.numeric(sub("%", "", Inflation$Percentage.Change))
plot(Inflation$Quarter, Inflation$Percentage.Change)

Inflation$Quarter <- as.Date(Inflation$Quarter, format = "%d-%m-%Y")
Inflation$Percentage.Change <- as.numeric(Percentage.Change)
plot(Inflation$Quarter, Inflation$Percentage.Change)


#changing qualitative variables to factors
vehicle_class<-as.character(vehicle_class)
risk_state_name<-as.factor(risk_state_name)
claim_loss_date<-as.Date(claim_loss_date)
term_start_date<-as.Date(term_start_date)
term_expiry_date<-as.Date(term_expiry_date)
accident_month <- as.Date(accident_month)

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



#Data Visualisation

#Barplot of number of claims/class (add title etc.)
No_claims_vs_Class <- ggplot(Claims_by_class, aes(x=vehicle_class, y= No_claims)) + 
  geom_bar(stat = "identity") 
No_claims_vs_Class

#Barplot of Avg claim cost/class (add title etc.)
AveCost_vs_Class <- ggplot(Claims_by_class, aes(x=vehicle_class, y=Ave_Claim_Amt)) + 
  geom_bar(stat = "identity") 
AveCost_vs_Class

#Barplot of No.cars per manufature year
No_cars_manufacture_year <- ggplot(By_manufacture_year, aes(x= year_of_manufacture, y = No_vehicles)) + 
  geom_bar(stat = "identity")
No_cars_manufacture_year

min(Commercial$year_of_manufacture)
max(Commercial$year_of_manufacture)
mean(Commercial$year_of_manufacture)


# POLICY ID THAT HAS TWO STATES
TWOSTATEENTRY <- Commercial[policy_id == 32422,]

#Number of policyholders per state
PH_per_state <- table(Insurance_by_ID$State)
PH_per_state

#Trying to plot total claims cost/ per quarter (compare it with Australia's inflationary data)

#has claims as a log amount (so values aren't too far apart)
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
  summarise(Total_QClaim = sum(Claims_every_AccMonth),
            No_claims = sum(No_claims))

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


#could compare postcodes as well? e.g. a certain postcode in a certain state could have more accidents 

#could also compare number of accidents given the year of production of each vehicle 

#could also conduct state wise analysis (i.e. NSW vehicles are more prone to VIC vehicles 
#i.e. have higher claims)

#can look ar externals as well (e.g. inflation rate, and thus relate it to insurance amount and frequency
#note: both are to be considered as mentioned in the assignment brief)



#UNEMPLOYMENT
#https://www.abs.gov.au/articles/historical-charts-august-1966-may-2022

Unemployment <- read.csv("Unemployment.csv")
Unemployment$Quarter <- as.yearqtr(Unemployment$Quarter)
plot(Unemployment$Quarter, Unemployment$Rate)
lines(Unemployment$Quarter, Unemployment$Rate)
cor.test(Unemployment$Rate, Quarterly_claim_freq1$Total_Q_claim_no)


p <- ggplot(obs, aes(x = Timestamp))
p <- p + geom_line(aes(y = air_temp, colour = "Temperature"))

#MAYBE
Violin <- Commercial[!is.na(total_claims_cost),]
ggplot(Violin, aes(x = vehicle_class, y=log(total_claims_cost)))+
  geom_violin(width = 2)+
  stat_summary(fun = "mean",
               geom = "crossbar",
               color = "red", width = 0.5)+
  labs(x = "Vehicle Class", y = "log of Number of Claims",
       title = "Log of claims per Vehicle Class")

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




avg.claim.size.per.qtr <- (Quarterly_claims$Total_QClaim/Quarterly_claims$No_claims)
Quarterly_claims$avg.claim.size.per.qtr <- avg.claim.size.per.qtr

Inflation[, "cum_inflation"] <- cumsum(Inflation$Percentage.Change)
  
set.seed(1)
train <- sample(1226044,919533)
lm.cum <- lm(Quarterly_claims$avg.claim.size.per.qtr ~ Inflation$cum_inflation, subset=train) 
summary(lm.cum)
mean((Quarterly_claims$avg.claim.size.per.qtr - predict(lm.cum, Quarterly_claims))[-train]^2)
dad <- plot(Quarterly_claims$avg.claim.size.per.qtr ~ Inflation$cum_inflation, subset=train)

