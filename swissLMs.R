library(caret)
library(AppliedPredictiveModeling)
library(dplyr)

set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


il_training = training %>% select(starts_with("IL"))
il_testing = testing%>% select(starts_with("IL"))

il_pca = preProcess(il_training,  method = c("center", "scale", "pca"),  thresh = 0.8)

il_model = train(il_training, training$diagnosis, method ="glm")

sum(predict(il_model, il_testing) == testing$diagnosis)/length(testing$diagnosis)
