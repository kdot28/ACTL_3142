

#CLAIMS PER CLASS
ggplot(Claims_by_class) + 
  geom_bar(aes(x=reorder(vehicle_class, -No_claims), y= No_claims, fill = No_claims), stat = "identity") +
  labs(x = "Vehicle Class", y = "Number of Claims",
       title = "Number of Claims per Vehicle Class")+
  scale_fill_gradient(low = "green", high = "orange")


#AVE COST PER CLASS
ggplot(Claims_by_class) + 
  geom_bar(aes(x=reorder(vehicle_class, -Ave_Claim_Amt), y=Ave_Claim_Amt, fill = Ave_Claim_Amt), 
                  stat = "identity")+
  theme_grey()+
  scale_fill_gradient(low = "green", high = "orange")+
  labs(x = "Vehicle Class", y = "Average Claim Amount",
       title = "Average Claim Amount per Vehicle Class")
  


#POLICYHOLDERS PER STATE (TABLE)
PH_per_state <- table(Insurance_by_ID$State)

library(data.table)
PH_per_state


#QUARTERLY CLAIM AMOUNT
ggplot(Quarterly_claims, aes(x = accident_month, y = Total_QClaim)) + 
  geom_line(colour = "black", size = 0.8 )+
  labs(x = "Quarter", y = "Total Amount of Claims",
       title = "Total Claim Amount per Quarter")+
  theme_gray()

#QUARTERLY NUMBER OF CLAIMS
Q_claim_data <- Quarterly_claims %>%
  mutate(rate = Inflation$Percentage.Change)

ggplot(Quarterly_claims)+
  geom_line(aes(x = accident_month, y=No_claims), 
            stat = "identity") + 
  labs(x = "Quarter", y = "Total Number of Claims",
       title = "Total Number of Claims per Quarter")+
  theme_gray()

#Total Number of Claims per Month
ggplot()+
  geom_line(dat = Claims_per_month, aes(x = accident_month, y=Number_of_claims), 
            stat = "identity") + 
  labs(x = "Accident Month", y = "Total Number of Claims",
       title = "Total Number of Claims per Month")+
  theme_gray()

#Average Claim Size per Month
ggplot()+
  geom_rect(data =Claims_per_month, aes(xmin = as.Date("2016-07-31"), ymin = -Inf, xmax = as.Date("2020-02-29"), ymax = Inf,
                                        fill = "111111111111111111"))+
  geom_rect(data =Claims_per_month, aes(xmin = as.Date("2020-02-29"), ymin = -Inf, xmax = as.Date("2021-06-30"), ymax = Inf,
                fill = "222222222222222222"))+

  geom_line(data = Claims_per_month,aes(x = accident_month, y=Average_claim_size), 
            stat = "identity") + 
  scale_fill_brewer(palette = 'Blues', name ='')+
  labs(x = "Accident Month", y = "Average Claim Size",
       title = "Average Claim Size per Month")

plot(Claims_per_month$accident_month, Claims_per_month$Number_of_claims)

#INFLATION + CLAIMS (QUARTERLY)  +THIS DONT WORK YET+




plot(Inf_claim_data$accident_month, Inf_claim_data$rate)

ggplot(Inf_claim_data) + 
  geom_line(x= accident_month, y = Inf_claim_data$Total_QClaim)+
  geom_line(y = Inf_claim_data$rate)+
  scale_y_continuous("Rate (%)", sec.axis = sec_axis( trans=~.*10, name="Second Axis"))


#SUM INSURED BY MONTH
month <- Commercial%>%
  group_by(accident_month) %>%
  summarise(amt = mean(sum_insured))

month$accident_month<- as.Date(month$accident_month)

ggplot(month)+
  geom_line(aes(x = accident_month, y = amt), stat = "identity")+
  labs(x = "Month", y = "Average Sum Insured",
       title = "Average Sum Insured per Month")

#Average Claim Cost per State

ggplot(States) + 
  geom_bar(aes(x=reorder(risk_state_name, -Sum_per_vehicle), y = Sum_per_vehicle, fill = Sum_per_vehicle),
                stat = "identity") + 
  scale_fill_gradient(low = "green", high = "orange")+
  theme_gray()+
  labs(x = "State Name", y = "Average Claim Cost",
       title = "Average Claim Cost per State", fill = "Average Claim Cost")
  

#### FOR LAGGING DATA




