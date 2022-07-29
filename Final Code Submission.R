install.packages("leaps")
library("leaps")

train.control<-trainControl(method = "cv", number = 5)
step.model <- train(A_claims_freq~., data = df1,
                    method = "leapBackward",
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control)

step.model$results
step.model$bestTune

summary(step.model$finalModel)

step.model$finalModel


summary(step.model)
