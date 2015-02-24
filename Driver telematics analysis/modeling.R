# Kaggle driver telematics challenge
# Model building script

# First we build up the reference set of different rides of different random drivers
# Note: we could make a small error if the driver of interest is sampled in randomDrivers
set.seed(123)
randomDrivers <- sample(drivers, size = nRandomDrivers + 1, replace = FALSE)

# Select one random ride from each driver in randomDrivers
refData <- driverData %>% 
  filter(driverID %in% randomDrivers) %>%
  group_by(driverID) %>%
  sample_n(1)
  
# Add the labels
refData <- refData %>% mutate(target = 0)

# For every driver, we fit a model and predict the labels
drivers <- list.files('./drivers/')
nPredictions <- propTest * (nRandomDrivers + nRides) # number of entries in every test set 
AUCdata <- data.frame(preds = numeric(nPredictions), obs = numeric(nPredictions))
counter <- 0

for(driver in drivers) {
  # Split data of interest in train and test set.
  currentData <- splitData(driver)
  
  # Fit a model
  model <- glm(target ~ total_duration + total_distance + stationary + norm_accel_50_perc + tang_accel_50_perc + accel_50_perc + speed_50_perc - driverID - rideID, 
               data = currentData$train, 
               family = binomial(link = "logit"))
  
  # Predict the labels
  preds <- predict(model, newdata = currentData$test, type = "response")
  obs <- currentData$train$target
  
  # Store the predictions and observations in a data rame
  AUCdata[(1 + counter*nPredictions):(2*counter*nPredictions), ] <- cbind(preds, obs)
  
  # Increase the counter
  counter <- counter + 1
}

splitData <- function(driverID) {
  # Form training and test set for the driver
  currentData <- driverData %>%
    filter(driverID == driver) %>%
    mutate(target = 1)
  
  trainDriver <- currentData %>%
    sample_n(propTraining * nRides) 
  
  testDriver <- currentData %>%
    setdiff(trainDriver)
  
  # Form training and test set for the random drivers
  if(driverID %>% randomDrivers) {
    othersData <- refData[-which(randomDrivers == driver)]
  } else {
    othersData <- refData[-1]
  }
  
  trainOthers <- othersData %>%
    sample_n(propTraining * nRandomDrivers)
  
  testOthers <- refData %>%
    setdiff(trainOthers)
  
  train <- rbind(trainDriver, trainOthers)
  test <- rbind(testDriver, testOthers)
  
  return(list(train, test))
}