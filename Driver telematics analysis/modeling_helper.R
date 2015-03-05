# First we build up the reference set of different rides of different random drivers
# Note: we could make a small error if the driver of interest is sampled in randomDrivers
# Thats why we sample nRandomDrivers + 1 drivers, and remove one of the drivers in this set accordingly
set.seed(123)
refData <- NULL
randomDrivers <- sample(drivers, size = nRandomDrivers + 1, replace = FALSE)

# Select one random ride from each driver in randomDrivers
for(driver in randomDrivers) {
  # Load in the data 
  dir <- paste0('./150303 aggregated data/', driver,'.csv')
  ridesData <- data.table::fread(dir)
  
  # Pick a random ride
  randomRide <- ridesData %>%
    sample_n(1)
  
  # Add the ride to the existing data frame
  refData <- rbind(refData, randomRide)
}

# Add the labels
refData <- refData %>% mutate(target = 0)

# Create a function that splits the data of a driverID and the reference data in a test and training set
splitData <- function(driverID) {
  # Form training and test set for the driver
  currentData <- data.table::fread(paste0("./150303 aggregated data/", driverID, ".csv")) %>%
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

createData <- function(driverID) {
  # Get data for current driver
  currentData <- data.table::fread(paste0("./150303 aggregated data/", driverID, ".csv")) %>%
    mutate(target = 1) 
  
  # Check if current driver is in randomDrivers
  if(driverID %in% randomDrivers) {
    othersData <- refData[-which(randomDrivers == driver),]
  } else {
    othersData <- refData[-1,]
  }
  
  # Form the total data
  
  return(rbind(currentData, othersData))
}