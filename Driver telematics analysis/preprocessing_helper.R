# Helper file that contains functions used for preprocessing the data.

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
