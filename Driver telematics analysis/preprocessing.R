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

driversProcessed <- read.csv('drivers_processed.csv')$driverID
driversToProcess <- setdiff(drivers, driversProcessed)

# Thomas @ 25-2:
# Add ride data stepwise for each driver
# get empty data frame with proper format  
driverData <- data.table::fread('drives_total.csv', nrows = nRides)
driverData[,] <- 0

# Execute this only if the drives.csv does not exist
if(!file.exists('drives_total.csv')) {
  driverData %>% filter(1 == 0) %>% # get an empty data frame
    write.table('drives_total.csv', row.name = FALSE, sep = ",") # write only the column names into the .csv file
}

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
      driverData[i,] <- aggRide
    },
    error = function(cond) {
      message(paste("Error at:", cond))
      message("Could not read ride", i, "for driver", driver)
      message("Proceeding to next ride")
    }
    )
  }
  write.table(driverData, 'drives.csv', append = TRUE, row.names = FALSE, sep = ",", col.names = FALSE)
  driverData[,] <- 0
  
  print(paste("Finished processing driver", driver))
}

# Checks on totaldata
totaldata <- data.table::fread('drives_total.csv')
totaldata %>% group_by(driverID) %>% summarise(n = n()) %>% filter(n != 200) # 2525 and 2526
totaldata %>% select(driverID) %>% unique() %>% dim
totaldata %>% select(driverID) %>% unique() %in% drivers # there should be 2736 different drivers

# Save the data (SQL)
drv <- dbDriver("SQLite")
tfile <- "drives_total.db"
con <- dbConnect(drv, dbname = tfile)
dbWriteTable(con, "drives", as.data.frame(totaldata), overwrite = TRUE)
dbDisconnect(con)

# Split the data on a driver level
for(driver in drivers) {
  dir <- paste0('./aggregated data/', driver, '.csv')
  
  totaldata %>% 
    filter(driverID == driver) %>%
    select(driverID, rideID, everything()) %>%
    write.csv(dir, row.names = FALSE)
  
  message('Processed driver', driver)
}


