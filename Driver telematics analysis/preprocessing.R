# Kaggle driver telematics challenge
# Data preprocessing script

# Load relevant libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(RSQLite)

# Set the working directory
setwd('E:/Kaggle/drivers/')

# Global parameters
nRides <- 200 # Number of rides per driver

# Feature engineering parameters
nDT <- 6 # Delta time used for velocity and accelaration
stationary_dist <- 10 # if the movement in meters in nDT seconds is lower than this, we say the car was stationary
avgTrim <- 0.025 # controls the % data that is trimmed when computing means

# Load helper functions
source('preprocessing_helper.R')

# Get a list of the drivers
drivers <- list.files('./drivers/')

# Check if the script already processed drivers earlier
if(file.exists('drivers_processed.csv')) {
  driversProcessed <- read.csv('drivers_processed.csv')$driverID
  driversToProcess <- setdiff(drivers, driversProcessed)
} else {
  driversToProcess <- drivers
}

# Format the data (from earlier runs)
driverData <- data.table::fread('150303 drives_total.csv', nrows = nRides)
driverData[,] <- 0

# For every driver, read in every ride and compute ride features
for(driver in driversToProcess) {
  dir <- paste0('./drivers/', driver, '/')
  for(i in seq_len(nRides)) {
    tryCatch({
      # Read in the drive file
      ride <- data.table::fread(paste0(dir, i, '.csv'))
      
      # Summarize the ride data
      aggRide <- summarizeRide(ride) %>% 
        mutate(driverID = as.numeric(driver), rideID = i) 
      
      # Add the summarized data to the total data
      driverData[i, ] <- aggRide
    },
    error = function(cond) {
      message(paste("Error at:", cond))
      message("Could not read ride", i, "for driver", driver)
      message("Proceeding to next ride")
    }
    )
  }
  # Write summarized data to file
  dir <- paste0('./150303 aggregated data/', driver, '.csv')
  write.csv(driverData, dir, row.names = FALSE)
  
  # Clean up
  driverData[,] <- 0
  print(paste("Finished processing driver", driver))
}

# Runtime: 5 hours

# Checks on totaldata
totaldrivers <- list.files('./150303 aggregated data/')
length(totaldrivers) # should be 2736
