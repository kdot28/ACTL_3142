

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
ggplot(Inf_claim_data)+
  geom_line(aes(x = accident_month, y=No_claims), 
            stat = "identity") + 
  labs(x = "Quarter", y = "Total Number of Claims",
       title = "Total Number of Claims per Quarter")+
  theme_gray()



#INFLATION + CLAIMS (QUARTERLY)  +THIS DONT WORK YET+
Inf_claim_data <- Quarterly_claims %>%
  mutate(rate = Inflation$Percentage.Change) %>%
  mutate(unem_rate = Unemployment$Rate)



plot(Inf_claim_data$accident_month, Inf_claim_data$Total_QClaim)

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
  



