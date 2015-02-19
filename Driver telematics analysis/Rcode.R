## Kaggle competition: Driver telematic analysis
## THU @ 5-2-2015: TODO:
## 1. Test current code to check if it works for one driver.
## 2. If so, code loop so that the procedure is done for each driver.
## 3. Parallelize the for loop in 2 for better performance with foreach.
## 4. Use a stripped down version of glm to reduce memory problems? (Check if necessary)
library(data.table)
library(dplyr)
library(tidyr)
library(glmnet)
library(ROCR)
library(parallel)
library(doParallel)
library(ggplot2)

setwd('E:/Kaggle/drivers/')

# Initial approach: 
# 1. For each driver, aggregate the date to a ride level with the following characteristics for each ride:
#   a. Average speed.
#   b. Average accelaration.
#   c. Maximum speed.
#   d. Maximum accelaration.
#   e. Total distance.
#   Optional:
#   f. Stationary time.
#   g. Average "curveness" of the drive.
#   h. Quantiles of speed
#   i. Quantiles of accelaration
#   j. normal accelaration = mv^2/r. (use r = ) (http://en.wikipedia.org/wiki/Circumscribed_circle#Cartesian_coordinates)
#   k. tangential accelaration
#   l. Consider splitting accelaration in two vars: positive and negative accelaration. (Maybe says more about driving behaviour)
#   m. Length of trip in time (# (x,y) pairs)
#   n. Use filtering techniques to smooth the (x,y) pairs before preprocessing

# 2. For each driver:
#   a. Form a dataset consisting of the rides for that driver, and the rides of 5 other drivers. (With appropriate labels)
#   b. Fit a logistic regression with the explanatory variables mentioned above.


# For cross-validation:
# 1. Pick 300 rides from 300 different drivers. Aggregate them with summarizeRide() and store them in a data_frame
#    with an extra column specifying the driver ID.
# 2. Split the data for the current driver in 75% training and 25% test set. Do the same with the 300 rides from other drivers.
# 3. Fit on the training set and cross-validate on the test set.
# OR
# Do 10-fold cross-validation on test + training set

# Explore more models:
# 1. Penalized models.
# 2. Random Forests.

# Global parameters
nRides <- 200 # Number of rides per driver

# Feature engineering parameters
nDT <- 1 # Delta time used for velocity and accelaration
stationary_dist <- 5 # if the movement in meters is lower than this, we say the car was stationary
avgTrim <- 0.025 # controls the % data that is trimmed when computing means

# Cross-validation parameters
nRandomDrivers <- 400 # Number of different drivers (rides) for the cross-validation set
propTraining <- 0.75
propTest <- 1 - propTraining

# Define function that aggregates the data for a given ride
# Input: data_frame with columns x, y 
summarizeRide <- function(ride, nlag = nDT, avg_trim = avgTrim) {
  
  augmented_ride <- ride %>% 
    mutate(dx = x - lag(x, nlag),
           dy = y - lag(y, nlag),
           speed = sqrt(dx^2 + dy^2)/nlag,
           tang_accelaration = (speed - lag(speed, nlag))/nlag,
           curvature = abs(dx*((dy - lag(dy, nlag))/nlag) - dy*((dx - lag(dx, nlag))/nlag))/(dx^2 + dy^2)^(3/2),
           normal_accelaration = curvature*speed^2,
           accelaration = sqrt(tang_accelaration^2 + normal_accelaration^2)) 
  # curvature is still not good
  # maybe try delta_distance_y with smaller nlag only for this computation (say 5).
  
  # Compute the quantiles of the features
  tang_accel <- returnQuantiles(augmented_ride$tang_accelaration, "tang_accel")
  speed <- returnQuantiles(augmented_ride$speed, "speed")
  norm_accel <- returnQuantiles(augmented_ride$normal_accelaration, "norm_accel")
  curvature <- returnQuantiles(augmented_ride$curvature, "curvature")
  accel <- returnQuantiles(augmented_ride$accelaration, "accel")
  
  # Aggregate results from the ride
  aggRide <- augmented_ride %>%
    summarize(avg_speed = mean(speed, na.rm = TRUE, trim = avg_trim),
              avg_accel = mean(accelaration, na.rm = TRUE, trim = avg_trim),
              avg_tang_accel = mean(tang_accelaration, na.rm = TRUE, trim = avg_trim),
              avg_norm_accel = mean(normal_accelaration, na.rm = TRUE, trim = avg_trim),
              avg_curvature = mean(curvature, na.rm = TRUE, trim = avg_trim),
              sd_speed = sd(speed, na.rm = TRUE),
              sd_accel = sd(accelaration, na.rm = TRUE),
              sd_tang_accel = sd(tang_accelaration, na.rm = TRUE),
              sd_norm_accel = sd(normal_accelaration, na.rm = TRUE),
              sd_curvature = sd(curvature, na.rm = TRUE),
              total_distance = sum(sqrt(dx^2 + dy^2), na.rm = TRUE),
              stationary = sum(sqrt(dx^2 + dy^2) <= stationary_dist, na.rm = TRUE),
              total_duration = n())
  
  # Join the quantiles with the aggregated ride features (ugly)
  aggRide <- c(aggRide[1,], tang_accel, speed, norm_accel, curvature, accel)

  return(as_data_frame(aggRide))
}

# Input:
# x: feature vectore of which the quantiles are needed.
# name: name of the feature vector.
# qvec: vector containing the needed quantiles.
# std: threshold. Observations further than std standard deviations from the standardized values of the feature vector x are removed.
returnQuantiles <- function(x, name, qvec = seq(from = 0.1, to = 1, by = 0.1), std = 4) {
  # Remove NA values
  x <- x[!is.na(x)]
  
  # Standardize x values
  x_std <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  
  # Compute quantiles
  q <- quantile(x[x_std < std], qvec, na.rm = TRUE)
  
  # Set names
  names(q) <- paste(name, names(q), sep = "_")
  names(q) <- gsub("%", "_perc", names(q))
  
  return(q)
}

plotRides <- function(driver, rides) {
  dir <- paste0('./drivers/', driver, '/')
  rideData <- NULL
  
  for(i in rides) {
    # Read in the ride
    ride <- data.table::fread(paste0(dir, i, '.csv'))
    
    # Add the rideID to the x and y vectors
    ride <- ride %>% mutate(rideID = i)
    
    # Add the data to the other rides
    rideData <- rideData %>% rbind(ride)
  }
  
  rideData$rideID <- as.factor(rideData$rideID)
  p <- ggplot(data = rideData, aes(x = x, y = y, col = rideID)) + geom_point()
  
  return(p)
}

# Get a list of the drivers
drivers <- list.files('./drivers/')

# First we build up the reference set of different rides of different random drivers
# Note: we could make a small error if the driver of interest is sampled in randomDrivers
set.seed(123)
randomDrivers <- sample(drivers, size = nRandomDrivers, replace = FALSE)
refData <- NULL

for(driver in randomDrivers) {
  # Set the directory
  dir <- paste0('./drivers/', driver, '/')
  
  # Read in a random ride for the driver
  rideNr <- sample(nRides, size = 1)
  ride <- data.table::fread(paste0(dir, rideNr, '.csv'))
  
  # Summarize the ride data
  aggRide <- summarizeRide(ride) %>%
    mutate(driverID = as.numeric(driver), rideID = rideNr) %>%
    select(driverID, rideID, everything())
  
  # Add the summarized data to the total CV data
  refData <- refData %>% rbind(aggRide)
}

# Add the labels
refData <- refData %>% mutate(target = 0)

# Split the data in 75% training set and 25% test set
set.seed(12345)
refData_training <- refData[sample(nRandomDrivers, size = nRandomDrivers*propTraining, replace = FALSE), ]
refData_test <- refData %>% setdiff(refData_training)

# Create the data frame with all the agregated rides for the driver of interest
# @ 9-2-2015: when working, this code has to be placed in for loop iterating over each driver

# Select one driver to start with
driver <- drivers[2]

driverData <- NULL

dir <- paste0('./drivers/', driver, '/')
for(i in seq_len(nRides)) {
  # Read in the drive file
  ride <- data.table::fread(paste0(dir, i, '.csv'))
  
  # Summarize the ride data
  aggRide <- summarizeRide(ride) %>%
    mutate(driverID = as.numeric(driver), rideID = i) %>%
    select(driverID, rideID, everything())
  
  # Add the summarized data to the total data
  driverData <- driverData %>% rbind(aggRide) 
}


# Add the labels
# TO DO: make sure target is a factor
driverData <- driverData %>% mutate(target = 1)

# Split the data in 75% training set and 25% test set
set.seed(123456)
driverData_training <- driverData[sample(nRides, size = nRides*propTraining, replace = FALSE), ]
driverData_test <- driverData %>% setdiff(driverData_training)

# Join the reference and current driver sets together
train <- driverData_training %>% rbind(refData_training)
test <- driverData_test %>% rbind(refData_test)
total <- rbind(train, test)

total[is.na(total)] <- 0
total$target <- factor(total$target, levels = c(0, 1))

test[is.na(test)] <- 0
train[is.na(train)] <- 0

test$target <- factor(test$target, levels = c(0,1))
train$target <- factor(test$target, levels = c(0,1))


# Fit the logistic regression
mod <- glm(target ~ . - driverID - rideID, data = train, family = binomial(link = "logit"))

mod <- glm(target ~ total_duration + total_distance + stationary + norm_accel_50_perc + tang_accel_50_perc + accel_50_perc + speed_50_perc, 
           data = train, 
           family = binomial(link = "logit"))
summary(mod)

# Fit a lasso regression with glmnet (10-fold CV)
set.seed(54321)
cvLASSO <- cv.glmnet(x = as.matrix(train[,-c(1:2, 66)]),
                     y = as.matrix(train[,66]),
                     family = "binomial",
                     standardize = TRUE,
                     type.measure = "auc")

modLASSO <- glmnet(x = as.matrix(train[,-c(1:2, 66)]),
                   y = as.matrix(train[,66]),
                   family = "binomial",
                   lambda = cvLASSO$lambda.1se,
                   standardize = TRUE)

# Predict the labels and plot the ROC curve
preds <- ROCR::prediction(predict(mod, newdata = test, type = "response"), test$target)
perf <- ROCR::performance(preds, "tpr", "fpr")
ROCR::performance(preds, "auc")
plot(perf)


preds <- ROCR::prediction(predict(modLASSO, newx = as.matrix(test[,-c(1:2, 66)]), type = "response", s = cvLASSO$lambda.1se), test$target)
perf <- ROCR::performance(preds, "tpr", "fpr")
ROCR::performance(preds, "auc")
plot(perf)






for(driver in drivers) {
  dir <- paste0('./drivers/', driver, '/')
  for(i in seq_len(nRides)) {
    # Read in the drive file
    ride <- data.table::fread(paste0(dir, i, '.csv'))
    
    # Summarize the ride data
    aggRide <- summarizeRide(ride) %>%
      mutate(driverID = as.numeric(driver), rideID = i) %>%
      select(driverID, rideID, everything())
    
    # Add the summarized data to the total data
    driverData <- driverData %>% rbind(aggRide) 
  }
}

# Set up database    
drv <- dbDriver("SQLite")
tfile <- "drives.db"
con <- dbConnect(drv, dbname = tfile)
dbWriteTable(con, "drives", driverData)
dbDisconnect(con)

write.csv(driverData, 'drives.csv')





## Alternatively 

stripGlmLR <- function(cm) {
  cm$y = c()
  cm$model = c()
  
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  
  
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  
  cm
}

mod <- stripGlmR(glm(target ~ ., data = driverData, family = binomial()))








radiusNormalAcc <- function(a, b, c) {
  # a, b, c are all numeric vectors in R^2.
  # They are the (x,y)-locations of each point on the circle
  u <- numeric(2)
  
  D <- 2*(a[1]*(b[2] - c[2]) + b[1]*(c[2] - a[2]) + c[1]*(a[2] - b[2]))
  u[1] <- 2*((a[1]^2 + a[2]^2)*(b[2] - c[2]) + (b[1]^2 + b[2]^2)*(c[2] - a[2]) + (c[1]^2 + c[2]^2)*(a[2] - b[2]))/D 
  u[2] <- 2*((a[1]^2 + a[2]^2)*(c[1] - b[1]) + (b[1]^2 + b[2]^2)*(a[1] - c[1]) + (c[1]^2 + c[2]^2)*(b[1] - a[1]))/D 
  radius <- sqrt((a[1] - u[1])^2 + (a[2] - u[2])^2)
  
  return(radius)
}