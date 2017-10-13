library(readr)
library(tidyr)
library(caret)
library(dplyr)
library(pROC)
library(rpart)
library(statisticalModeling)

data1= read_delim("C:/Training/17_10_03_Intro_Explod/denguecases.csv",delim=';')

#mutate(data1,new_column=)

data1$training_data<- rnorm(nrow(data1))>0



fit <- rpart(Possibility ~ Temperature + Population + Urbanization, data=subset(data1,training_data),method="class")


pred<-predict(fit,newdata=subset(data1, !training_data))

rmse=sqrt(mean((pred[,2]-subset(data1, !training_data)$Possibility)^2))
print(rmse)


# Get model output with the training data as input
model_output <- evaluate_model(fit, data = subset(data1, !training_data), type = "class")

# Find the error rate
with(data = model_output, mean(model_output!=subset(data1, !training_data)$Possibility, na.rm = TRUE))

print(model_output)


#(conf<-table(subset(data1, !training_data)$Possibility,model_output$model_output))
(conf<-table(subset(data1, !training_data)$Possibility,model_output$model_output))



TP <- conf[1, 1] 
FN <- conf[1, 2] 
FP <- conf[2, 1] 
TN <- conf[2, 2]

(Accuracy=(TP+TN) / (TP+FN+FP+TN))

fit2 <- rpart(Possibility ~ Temperature + Population + Urbanization, data=subset(data1,training_data))


pred2<-predict(fit2,newdata=subset(data1, !training_data))

rmse2=sqrt(mean((pred2-subset(data1, !training_data)$Possibility)^2))
print(rmse2)

ROC <- roc(subset(data1, !training_data)$Possibility, round(pred[,2]))
plot.roc(ROC, col = '#F4B828', print.auc = T, xlab = 'False positive rate (1 - Specificity)', ylab = 'True positive rate (Sensitivity)', legacy.axes = T)

ROC2 <- roc(subset(data1, !training_data)$Possibility, round(pred2)
plot(ROC2, col = '#5E7164', print.auc = T, print.auc.y = 0.4, add = T)
