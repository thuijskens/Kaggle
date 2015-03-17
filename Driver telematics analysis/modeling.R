# Kaggle driver telematics challenge
# Model building script

library(ROCR)
library(randomForest)
library(gbm)
library(dplyr)
library(ggplot2)

# Set the working directory
setwd('E:/Kaggle/drivers/')

# Global parameters
nRides <- 200 # Number of rides per driver
nRandomDrivers <- 300
propTraining <- 0.75
propTest <- 1 - propTraining
drivers <- list.files('./drivers/')

# Feature engineering parameters
nDT <- 6 # Delta time used for velocity and accelaration
stationary_dist <- 10 # if the movement in meters in nDT seconds is lower than this, we say the car was stationary
avgTrim <- 0.025 # controls the % data that is trimmed when computing means

# Load in helper functions/run helper scripts
source('preprocessing_helper.R')
source('modeling_helper.R')

# For every driver, we fit a model and predict the labels
drivers = drivers[1:30]
nPredictions <- propTest * (nRandomDrivers + nRides) # number of entries in every test set 
AUCdata <- data.frame(preds1 = numeric(length(drivers)*nPredictions), 
                      preds2 = numeric(length(drivers)*nPredictions),
                      preds3 = numeric(length(drivers)*nPredictions),
                      stackpred = numeric(length(drivers)*nPredictions),
                      obs = factor(x = numeric(length(drivers)*nPredictions), levels = c(0, 1)))
counter <- 0

for(driver in drivers) {
  # Split data of interest in train and test set.
  currentData <- splitData(driver)
  
  # Fit a linear model
  model1 <- glm(target ~ total_duration + total_distance + stationary + norm_accel_50_perc + tang_accel_50_perc + accel_50_perc + speed_50_perc, 
                data = currentData$train, 
                family = binomial(link = "logit"))
  
  # Fit a GBM
  model2 <- gbm(formula = target ~ . - driverID - rideID,
                data = currentData$train, 
                distribution = "adaboost")
  
  # Fit a random forest
  currentData$train$target <- as.factor(currentData$train$target)
  currentData$test$target <- as.factor(currentData$test$target)
  
  model3 <- randomForest(x = select(currentData$train, -driverID, -rideID, -target), 
                         y = currentData$train$target)
  
  # Stacking the models
  stackdf <- data.frame(target = currentData$train$target,
                        pred_glm = predict(model1, type = "response"),
                        pred_gbm = predict(model2, n.trees = 100, type = "response"),
                        pred_rf = predict(model3, type = "prob")[,2])
  
  stack1 <- glm(formula = target ~ pred_glm + pred_rf, 
                data = stackdf, 
                family = binomial(link = "logit"))
  
  # Predict the labels
  preds1 <- predict(model1, newdata = currentData$test, type = "response")
  preds2 <- predict(model2, newdata = currentData$test, n.trees = 100, type = "response") 
  preds3 <- predict(model3, newdata = select(currentData$test, -driverID, -rideID, -target), type = "prob")[,2]
  
  stackdf_pred <- data.frame(target = currentData$test$target,
                             pred_glm = preds1,
                             pred_gbm = preds2,
                             pred_rf = preds3)
  
  stackpred <- predict(stack1, newdata = stackdf_pred, type = "response")  
  obs <- currentData$test$target
  
  # Store the predictions and observations in a data rame
  AUCdata[(1 + counter*nPredictions):(nPredictions + counter*nPredictions), ] <- data.frame(preds1, preds2, preds3, stackpred, obs)
  
  # Increase the counter
  counter <- counter + 1
  
  message("Finished processing driver ", driver)
}

totalPreds1 <- ROCR::prediction(AUCdata$preds1, AUCdata$obs)
totalPreds2 <- ROCR::prediction(AUCdata$preds2, AUCdata$obs)
totalPreds3 <- ROCR::prediction(AUCdata$preds3, AUCdata$obs)
totalStack <- ROCR::prediction(AUCdata$stackpred, AUCdata$obs)

perf1 <- ROCR::performance(totalPreds1, "tpr", "fpr")
perf2 <- ROCR::performance(totalPreds2, "tpr", "fpr")
perf3 <- ROCR::performance(totalPreds3, "tpr", "fpr")
perf4 <- ROCR::performance(totalStack, "tpr", "fpr")

ROCR::performance(totalPreds1, "auc")@y.values
ROCR::performance(totalPreds2, "auc")@y.values
ROCR::performance(totalPreds3, "auc")@y.values
ROCR::performance(totalStack, "auc")@y.values

plot(perf1, col = "green")
plot(perf2, col = "red", add = TRUE)
plot(perf3, col = "blue", add = TRUE)
plot(perf4, col = "yellow", add = TRUE)

