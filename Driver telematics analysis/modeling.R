# Kaggle driver telematics challenge
# Model building script

library(ROCR)
library(randomForest)
library(gbm)

# Global parameters
nRandomDrivers <- 200
propTraining <- 0.75
propTest <- 1 - propTraining

# First we build up the reference set of different rides of different random drivers
# Note: we could make a small error if the driver of interest is sampled in randomDrivers
set.seed(123)
randomDrivers <- sample(drivers, size = nRandomDrivers + 1, replace = FALSE)

# Select one random ride from each driver in randomDrivers
refData <- totaldata %>% 
  filter(driverID %in% randomDrivers) %>%
  group_by(driverID) %>%
  sample_n(1)
  
# Add the labels
refData <- refData %>% mutate(target = 0)

# Create a function that splits the data of a driverID and the reference data in a test and training set
splitData <- function(driverID) {
  # Form training and test set for the driver
  currentData <- data.table::fread(paste0("./aggregated data/", driverID, ".csv")) %>%
    mutate(target = 1)
  
  trainDriver <- currentData %>%
    sample_n(propTraining * nRides) 
  
  testDriver <- currentData %>%
    setdiff(trainDriver)
  
  # Form training and test set for the random drivers
  if(driverID %in% randomDrivers) {
    othersData <- refData[-which(randomDrivers == driver),]
  } else {
    othersData <- refData[-1,]
  }
  
  trainOthers <- othersData %>%
    sample_n(propTraining * nRandomDrivers)
  
  testOthers <- othersData %>%
    setdiff(trainOthers)
  
  train <- rbind(trainDriver, trainOthers)
  test <- rbind(testDriver, testOthers)
  
  return(list(train = train, test = test))
}

# For every driver, we fit a model and predict the labels
drivers <- list.files('./drivers/')
nPredictions <- propTest * (nRandomDrivers + nRides) # number of entries in every test set 
AUCdata <- data.frame(preds = numeric(nPredictions), obs = integer(nPredictions))
counter <- 0

for(driver in drivers[1:100]) {
  # Split data of interest in train and test set.
  currentData <- splitData(driver)
  
  # Fit a model
  #model <- glm(target ~ total_duration + total_distance + stationary + norm_accel_50_perc + tang_accel_50_perc + accel_50_perc + speed_50_perc - driverID - rideID, 
  #             data = currentData$train, 
  #             family = binomial(link = "logit"))
  
  # Fit a GBM
  currentData$train$target <- as.factor(currentData$train$target)
  currentData$test$target <- as.factor(currentData$test$target)
  
  model <- randomForest(x = select(currentData$train, -driverID, -rideID, -target), 
                        y = currentData$train$target)

  # Predict the labels
  preds <- predict(model, 
                   newdata = select(currentData$test, -driverID, -rideID, -target), 
                   type = "prob")[,2]
  obs <- currentData$test$target
  
  # Store the predictions and observations in a data rame
  AUCdata[(1 + counter*nPredictions):(nPredictions + counter*nPredictions), ] <- data.frame(preds, obs)
  
  # Increase the counter
  counter <- counter + 1
  
  message("Finished processing driver ", driver)
}

totalPreds <- ROCR::prediction(AUCdata$preds, AUCdata$obs)
perf <- ROCR::performance(totalPreds, "tpr", "fpr")
ROCR::performance(totalPreds, "auc")
plot(perf)
