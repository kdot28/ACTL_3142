rm(list = ls())

#Loading all the necessary packages (please install necessary packages)
library(ggplot2)
library(dplyr)
library(class)
library(MASS)
library(caret)
library(devtools)
library(countreg)
library(forcats)
library(AER)
library(pscl)
library(boot)
library(sjPlot)
library(leaps)

#import the dataset
Commercial <- read.csv("ACTL3142Data.csv") 

#Getting rid of 11 duplicate entries
Commercial_2 <- unique(Commercial)

#Sort by policy ID + Cleansing
Commercial_3 = subset(Commercial_2, select = -c(2,3,11))
Commercial_3 <- Commercial_3 %>% group_by(policy_id) %>% 
  na.omit(Commercial_3$total_claims_cost) %>% filter(total_claims_cost > 0) %>%
  mutate(Vehicle_age = 2022 - year_of_manufacture) %>% filter(Vehicle_age >= 0)
                     
Commercial_3 = subset(Commercial_3, select = -7)  

#Making States and postcodes factors, making accident month appear as month

Commercial_3$risk_state_name<-as.factor(Commercial_3$risk_state_name)
Commercial_3$risk_postcode <- as.factor(Commercial_3$risk_postcode)
Commercial_3$vehicle_class <- as.factor(Commercial_3$vehicle_class)

Commercial_3 <- Commercial_3 %>% 
  mutate(claim_month = as.yearmon(accident_month, format = "%Y-%m-%d"))

Commercial_3 = subset(Commercial_3, select = -1)

col_order <- c("policy_id", "claim_month", "policy_tenure",
               "sum_insured", "risk_state_name", "risk_postcode",
               "vehicle_class", "exposure", "total_claims_cost",
               "Vehicle_age")
Commercial_3 <- Commercial_3[, col_order]

#Attaching external data

Iron_steel_Imports <- read.csv("Iron Steel Import.csv", header = T)
Oil_production <- read.csv("Crude Oil Production.csv", header = T)
Transport_Parts_Imports <- read.csv("Transport Parts Import.csv", header = T)
Transport_equip_machinery <- read.csv("Machinery TransEquip Imports.csv", header = T)
Gold_Price <- read.csv("gold.price.per.ounce.csv", header = T)

External_data <- cbind(Iron_steel_Imports, Oil_production, 
                       Transport_equip_machinery, Transport_Parts_Imports,
                       Gold_Price)[-c(61:72),]
External_data_lagged <- cbind(Iron_steel_Imports, Oil_production, 
                              Transport_equip_machinery, Transport_Parts_Imports,
                              Gold_Price)[-c(1:60),]
#Sorting external data by month
External_data_1 <- cbind(claim_month = unique(Commercial_3$claim_month), External_data)
External_data_1 <- External_data_1 %>% group_by(claim_month) %>% 
  arrange(., claim_month) 

External_data_lagged_1 <- cbind(claim_month = External_data_1[-c(1:48), 1],
                                External_data_lagged)

#Average Sum Insured
Sum_insured_Monthly <- Commercial_3 %>%
  group_by(claim_month) %>% 
  summarise(Total_sum_insured = sum(sum_insured),
            No_of_vehicles = sum((sum_insured)*0+1),
            Avg_sum_insured = Total_sum_insured/No_of_vehicles)

#An initial veiwing of relevant variables

#freq
train.control<-trainControl(method = "cv", number = 8)
step.model <- train(Claim_Count~., data = GLM_frequency_3[,-c(8,10)] ,
                    method = "leapBackward",
                    tuneGrid = data.frame(nvmax = 1:8),
                    trControl = train.control)

step.model$results
step.model$bestTune

summary(step.model$finalModel)

step.model$finalModel

summary(step.model)


# ------- Claims Severity Modelling --------

Claims_Severity <- Commercial_3 %>% group_by(claim_month) %>%
  summarise(Claim_sum = sum(total_claims_cost), 
            No_of_claims = sum((total_claims_cost)*0 +1))

Claims_Severity_1 <- Claims_Severity %>% group_by(claim_month) %>%
  summarise(claims_sev = Claim_sum/No_of_claims)

GLM_severity <- cbind(Claims_Severity_1, External_data_1[,-1], Sum_insured_Monthly[,4])
GLM_severity_1 <- cbind(External_data_lagged_1, GLM_severity[c(49:60), c(2,8)])
col_order_1 <- c("claim_month", "claims_sev", "Iron_Steel_Import",
               "Oil.Production", "Imports_Machinery_TranspEquip", "Transport_Parts_import",
               "gold.price", "Avg_sum_insured")
GLM_severity_1 <- GLM_severity_1[, col_order_1]

GLM_severity_2 <- rbind(GLM_severity, GLM_severity_1)


#set.seed(1)
#train_index <- sample(1:nrow(GLM_severity), round(0.75*nrow(GLM_severity)))
#train_sev <- rep(FALSE, nrow(GLM_severity))
#train_sev[train_index] <- TRUE
#test_sev <- !train_sev

#Gamma GLM
gamma_sev <- glm(claims_sev ~., data = GLM_severity_2[c(1:60),-1],
              family = Gamma(link = "log"))

summary(gamma_sev)

gamma_sev_pred <- predict.glm(gamma_sev, newdata = GLM_severity_2[-c(49:60),])
sum((gamma_sev_pred - GLM_severity_2[-c(49:60),]$claims_sev)^2)

set.seed(1010)
kfold_error_5 <- rep(0,5)
for (i in 1:5) {gamma_sev
  kfold_error_5[i] <- cv.glm(GLM_severity_2[-c(49:60),], gamma_sev, K = 5)$delta[1]
}
kfold_error_5
mean(kfold_error_5)

#Guassian GLM
gaus_sev <- glm(claims_sev ~., data = GLM_severity_2[c(1:60),-1],
                family = gaussian(link = "log"))
summary(gaus_sev)

gaus_sev_pred <- predict.glm(gaus_sev, newdata = GLM_severity_2[-c(49:60),])
sum((gaus_sev_pred - GLM_severity_2[-c(49:60),]$claims_sev)^2)

set.seed(110)
kfold_error_5x <- rep(0,5)
for (i in 1:5) {gaus_sev
  kfold_error_5x[i] <- cv.glm(GLM_severity_2[-c(49:60),], gaus_sev, K = 5)$delta[1]
}
kfold_error_5x
mean(kfold_error_5x)

#Graph for Gamma 
results_sev <- data.frame(predicted = exp(gamma_sev_pred), 
                          actual = (GLM_severity_2$claims_sev[-c(49:60)]), 
                          Accident_Month = GLM_severity_2$claim_month[-c(49:60)])


Actual_vs_pred_sev <- ggplot(results_sev, aes(Accident_Month)) + 
  geom_line(aes(y = (predicted), colour = "predicted")) + 
  geom_line(aes(y = (actual), colour = "actual")) + labs(x = "Accident Month",
                                                         y = "Claims Severity",
                                                         title = "Gamma (Actual vs Predicted)")

Actual_vs_pred_sev

#Graph for Gaussian
results_sev1 <- data.frame(predicted = exp(gaus_sev_pred), 
                                          actual = (GLM_severity_2$claims_sev[-c(49:60)]), 
                                          Accident_Month = GLM_severity_2$claim_month[-c(49:60)])


Actual_vs_pred_sev1 <- ggplot(results_sev1, aes(Accident_Month)) + 
  geom_line(aes(y = (predicted), colour = "predicted")) + 
  geom_line(aes(y = (actual), colour = "actual")) + labs(x = "Accident Month",
                                                         y = "Claims Severity",
                                                         title = "Gaussian (Actual vs Predicted)")

Actual_vs_pred_sev1

# --------------- Claims Frequency Modelling ------------------
Claims_frequency <- Commercial_3 %>% group_by(claim_month) %>% 
  summarise(Claim_Count = sum((total_claims_cost)*0 +1), exposure_1 = sum(exposure))

GLM_frequency <- cbind(Claims_frequency, External_data_1[,-1], Sum_insured_Monthly[,4])
GLM_frequency_1 <- GLM_frequency %>% group_by(claim_month) %>%
  mutate(frequency = Claim_Count/exposure_1)

GLM_frequency_2 <- cbind(External_data_lagged_1, GLM_frequency_1[c(49:60), c(2,3,9,10)])
col_order_2 <- c("claim_month", "Claim_Count", "exposure_1",
                 "Iron_Steel_Import", "Oil.Production","Imports_Machinery_TranspEquip",
                 "Transport_Parts_import", "gold.price", "Avg_sum_insured", "frequency")
GLM_frequency_2 <- GLM_frequency_2[, col_order_2]

GLM_frequency_3 <- rbind(GLM_frequency_1,GLM_frequency_2,)


#Poisson
pois_freq <- glm(as.integer(frequency) ~., data = GLM_frequency_3[c(1:60) ,-c(1,2,3)],
                 family = poisson(link = "log"))

summary(pois_freq)

pois_freq_pred <- predict.glm(pois_freq, newdata = GLM_frequency_3[-c(49:60),])
sum((pois_freq_pred - GLM_frequency_3[-c(49:60),]$frequency)^2)

#K-FOLD pois
set.seed(1010)
kfold_error_5a <- rep(0,5)
for (i in 1:5) {pois_freq
  kfold_error_5a[i] <- cv.glm(GLM_frequency_3[-c(49:60),], pois_freq, K = 5)$delta[1]
}
kfold_error_5a
mean(kfold_error_5a)

#dispersion test
dispersiontest(pois_freq)

# Quasi Poisson
quasi_freq <- glm(as.integer(frequency) ~., data = GLM_frequency_3[c(1:60) ,-c(1,2,3,5)],
                 family = quasipoisson(link = "log"))

summary(quasi_freq)

quasi_freq_pred <- predict.glm(quasi_freq, newdata = GLM_frequency_3[-c(49:60),])
sum((quasi_freq_pred - GLM_frequency_3[-c(49:60),]$frequency)^2)

#K-Fold quasi
set.seed(2020)
kfold_error_5b <- rep(0,5)
for (i in 1:5) {quasi_freq
  kfold_error_5b[i] <- cv.glm(GLM_frequency_3[-c(49:60),], quasi_freq, K = 5)$delta[1]
}
kfold_error_5b
mean(kfold_error_5b)


# Negative Binomial
nb_freq <- glm.nb((frequency) ~., data = GLM_frequency_3[c(1:60) ,-c(1,2,3)],
                  control = glm.control(maxit = 10000))

summary(nb_freq)

nb_freq_pred <- predict.glm(nb_freq, newdata = GLM_frequency_3[-c(49:60),])
(sum((nb_freq_pred - GLM_frequency_3[-c(49:60),]$frequency)^2))/60

# K fold not working
set.seed(3030)
kfold_error_5c <- rep(0,5)
for (i in 1:5) {nb_freq
  kfold_error_5c[i] <- cv.glm(GLM_frequency_3[-c(49:60),], nb_freq, K = 5)$delta[1]
}
kfold_error_5c
mean(kfold_error_5c)

#Since K fold not working (under fits for smaller values and overfits for larger values)
nb_kfold_plot <- plot_kfold_cv(data = GLM_frequency_3[-c(49:60),], nb_freq)
nb_kfold_plot


#Graph for poisson
results_freq <- data.frame(predicted = exp(pois_freq_pred), 
                          actual = (GLM_frequency_3$frequency[-c(49:60)]), 
                          Accident_Month = GLM_frequency_3$claim_month[-c(49:60)])


Actual_vs_pred_freq <- ggplot(results_freq, aes(Accident_Month)) + 
  geom_line(aes(y = (predicted), colour = "predicted")) + 
  geom_line(aes(y = (actual), colour = "actual")) + labs(x = "Accident Month",
                                                         y = "Claims Frequency",
                                                         title = "Poisson (Actual vs Predicted)")

Actual_vs_pred_freq

#Graph for Quasi
results_freq1 <- data.frame(predicted = exp(quasi_freq_pred),
                            actual = (GLM_frequency_3$frequency[-c(49:60)]), 
                            Accident_Month = GLM_frequency_3$claim_month[-c(49:60)])

Actual_vs_pred_freq1 <- ggplot(results_freq1, aes(Accident_Month)) + 
  geom_line(aes(y = (predicted), colour = "predicted")) + 
  geom_line(aes(y = (actual), colour = "actual")) + labs(x = "Accident Month",
                                                         y = "Claims Frequency",
                                                         title = "Quasipoisson (Actual vs Predicted)")

Actual_vs_pred_freq1

#Graph for NB
results_freq2 <- data.frame(predicted = exp(nb_freq_pred),
                            actual = (GLM_frequency_3$frequency[-c(49:60)]), 
                            Accident_Month = GLM_frequency_3$claim_month[-c(49:60)])

Actual_vs_pred_freq2 <- ggplot(results_freq2, aes(Accident_Month)) + 
  geom_line(aes(y = (predicted), colour = "predicted")) + 
  geom_line(aes(y = (actual), colour = "actual")) + 
  labs(x = "Accident Month",y = "Claims Frequency",
       title = "Negative Binomial (Actual vs Predicted)")

Actual_vs_pred_freq2


#Claims Inflation




