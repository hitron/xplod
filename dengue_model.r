library(readr)
library(tidyr)
library(caret)
library(dplyr)
library(pROC)

# DATA PREPARATION -------------------

# import data
denguecases <- read_delim("C:/Training/17_10_03_Intro_Explod/denguecases.csv", 
                          ";", escape_double = FALSE, locale = locale(date_names = "nl", 
                                                                      decimal_mark = ","), trim_ws = TRUE)
# rename and select columns
dengue <- denguecases %>%
  rename(country = Country, temp = Temperature, pop = Population, prec = Precipations, urb = Urbanization, dengue = `Possibility`) %>%
  select(c(1:7),-6) # select relevant columns (with dengue probability instead of dengue cases)
dengue$dengue <- dengue$dengue %>%
  factor(labels = c('No', 'Yes'))


# split data in 2 groups: one for modelbuilding (training 75%) and one for testing (25%)
set.seed(42) # for reproducability
N <- nrow(dengue) # define number of rows

shuffle <- dengue[sample(N), ] # shuffle the rows of the data

dengue_mod <- shuffle[1:round(N * .75), ] # 75% of data is for model building
dengue_val <- shuffle[-c(1:round(N * .75)), ] # 25% of data is for validation

# Create dengue occurence as outcome
outcome <- dengue_mod$dengue 


# Create input variables as data.frame
input <- dengue_mod %>%
  select(temp, pop, prec, urb) %>%
  as.data.frame()



# MODEL BUILDING --------------

# Create reusable trainControl object: myControl
myControl <- trainControl(
  method = 'cv', number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE)

# Fit glmnet model: model_glmnet
model_glmnet <- train(x = input, y = outcome,
                      metric = "ROC",
                      method = 'glmnet',
                      tuneGrid = expand.grid(
                        alpha = 0:1,
                        lambda = 0:10/10),
                      trControl = myControl
)

plot(model_glmnet)


# Fit random forest model: model_rf
model_rf <- train(x = input, y = outcome,
                  metric = "ROC",
                  method = 'ranger',
                  trControl = myControl)

plot(model_rf)

# Fit gradient boosting model: model_gb
model_gb <- train(x = input, y = outcome,
                  metric = "ROC",
                  method = 'gbm',
                  trControl = myControl
)

plot(model_gb)


# Fit gam: model_gam
model_gam <- train(x = input, y = outcome,
                   metric = "ROC",
                   method = 'gam',
                   trControl = myControl
)

plot(model_gam)

# Create model_list
model_list <- list(glmnet = model_glmnet, rf = model_rf, gb = model_gb, gam = model_gam)

# Pass model_list to resamples(): resamples
resamples = resamples(model_list)

# Summarize the results
summary(resamples)
bwplot(resamples, metric = 'ROC')
densityplot(resamples, metric = 'ROC')
xyplot(resamples, metric = 'ROC')
dotplot(resamples, metric = 'ROC')

# MODEL VALIDATING ----
# predictions on training data
pred <- predict(model_rf, dengue_mod, type = 'prob')

pred2 <- predict(model_rf, dengue_mod, type = 'raw')
confusionMatrix(pred2, dengue_mod$dengue)

# predictions of testing data
pred3 <- predict(model_rf, dengue_val, type = 'prob')

pred4 <- predict(model_rf, dengue_val, type = 'raw')
confusionMatrix(pred4, dengue_val$dengue)

# ROC curves
ROC <- roc(as.numeric(outcome), pred$Yes)
plot.roc(ROC, col = '#F4B828', print.auc = T, xlab = 'False positive rate (1 - Specificity)', ylab = 'True positive rate (Sensitivity)', legacy.axes = T)

ROC2 <- roc(as.numeric(dengue_val$dengue), pred3$No)
plot(ROC2, col = '#5E7164', print.auc = T, print.auc.y = 0.4, add = T)


