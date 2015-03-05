# Kaggle driver telematics challenge
# sUBMISSION BUILDER SCRIPT

library(randomForest)
library(dplyr)
library(ggplot2)

# Set the working directory
setwd('E:/Kaggle/drivers/')

# Global parameters
nRides <- 200 # Number of rides per driver
drivers <- list.files('./drivers/')

# Feature engineering parameters
nDT <- 6 # Delta time used for velocity and accelaration
stationary_dist <- 10 # if the movement in meters in nDT seconds is lower than this, we say the car was stationary
avgTrim <- 0.025 # controls the % data that is trimmed when computing means

# Load in helper functions/run helper scripts
source('preprocessing_helper.R')
source('modeling_helper.R')

# For every driver, we fit a model and predict the labels
nPredictions <- nRides
AUCdata <- data.frame(d_r = character(length(drivers)*nPredictions),
                      preds = numeric(length(drivers)*nPredictions),
                      stringsAsFactors = FALSE)
counter <- 0

for(driver in drivers) {
  # Create the data
  currentData <- createData(driver)
  
  # Fit a random forest
  currentData$target <- as.factor(currentData$target)
  
  model <- randomForest(target ~ . -driverID - rideID,
                        data = currentData)
  
  # Predict the labels
  preds <- predict(model, 
                   newdata = filter(currentData, driverID == driver), 
                   type = "prob")[,2] 
  
  # Store the predictions and observations in a data rame
  AUCdata[(1 + counter*nPredictions):(nPredictions + counter*nPredictions), ] <- data.frame(d_r = paste0(driver, "_", 1:nRides),
                                                                                            preds = preds, stringsAsFactors = FALSE)
  
  # Increase the counter
  counter <- counter + 1
  
  message("Finished processing driver ", driver)
}

# Form driver_ride column
names(AUCdata) <- c("driver_trip", "prob")
write.csv(AUCdata, 'submission.csv', row.names = FALSE)
