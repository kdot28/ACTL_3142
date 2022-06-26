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

Commercial_new