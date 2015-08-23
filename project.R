library(caret)
library(dplyr)
library(ROCR)
library(dplyr)
library(lubridate)

setwd("~/Documents/git/datasciencecoursera/Practical Machine Learning/Project/")

source("utils.R")

allData = get_data()
training = allData[["training"]]
validation = allData[["validation"]]
testing = allData[["testing"]]

set.seed(7)

fitControl <- trainControl(method = "cv", number = 10)

glmnet_fit = train(classe~., training, 
                  method = "glmnet",
                  preProc = c("center", "scale"),
                  trControl = fitControl)


gbm_fit = train(classe~., training, 
                method = "gbm",
                preProc = c("center", "scale"),
                trControl = fitControl)

rf_fit = train(classe~., training, 
                method = "rf",
                preProc = c("center", "scale"),
                trControl = fitControl)


svm_fit = train(classe~., training, 
                method = "svmLinear",
                preProc = c("center", "scale"),
                trControl = fitControl)

glmnet_predict = predict(glmnet_fit, validation)
confusionMatrix(glmnet_predict, validation$classe)

gbm_predict = predict(gbm_fit, validation)
confusionMatrix(gbm_predict, validation$classe)

rf_predict = predict(rf_fit, validation)
plot(varImp(rf_fit))
confusionMatrix(rf_predict, validation$classe)

svm_predict = predict(svm_fit, validation)
confusionMatrix(svm_predict, validation$classe)

### Prediction on the test set
setwd("testResults/")
pml_write_files(predict(rf_fit,testing))
