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

driverData <- NULL

driversProcessed <- read.csv('drivers_processed.csv')$driverID
driversToProcess <- setdiff(drivers, driversProcessed)

driverData <- as_data_frame(data.table::fread('drives.csv'))

driverData <- driverData[, -1]

# For every driver, read in every ride and compute ride features
for(driver in driversToProcess) {
  dir <- paste0('./drivers/', driver, '/')
  for(i in seq_len(nRides)) {
    # Read in the drive file
    ride <- data.table::fread(paste0(dir, i, '.csv'))
    
    # Summarize the ride data
    aggRide <- summarizeRide(ride) %>%
      mutate(driverID = as.numeric(driver), rideID = i) 
    
    # Add the summarized data to the total data
    driverData <- driverData %>% rbind(aggRide) 
  }
}

# Re-order the columns of the dataset
driverData %>%
  select(driverID, rideID, everything())

# Save the data (SQL)
drv <- dbDriver("SQLite")
tfile <- "drives_partial.db"
con <- dbConnect(drv, dbname = tfile)
dbWriteTable(con, "drives", as.data.frame(driverData), overwrite = TRUE)
dbDisconnect(con)

# Save the data
write.csv(driverData, 'drives.csv')

driverData %>% select(driverID) %>% unique() %>%
  write.csv('drivers_processed.csv')
