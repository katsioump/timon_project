acc_metrics <- function(n_sensor = 5){
  s <- str_pad(n_sensor, 2, pad = "0")
  path <- paste0("data/raw/TLCRP0", s, ".csv")
  tl <- read.csv(file = path, head = TRUE)
  str(tl)
  
  tl$date_time <- as.POSIXct(
    paste(tl$Date, tl$Time),
    format = "%Y-%m-%d %H:%M:%S.%OS",
    tz = "UTC"
  )
  
  tl$Date <- strptime(tl$Date, format = "%Y-%m-%d")
  tl$Date <- as.Date(tl$Date, format = "%Y%m%d")
  
  # convert acceleration metric from g to m/s2 if needed
  tl$X <- tl$X * 9.8
  tl$Y <- tl$Y * 9.8
  tl$Z <- tl$Z * 9.8
  
  # with(tl, plot(date_time, na.approx(TemperatureC), type='l'))
  # with(tl, plot(date_time, na.approx(Humidity), type='l'))
  
  
  # Calculate ODBA and VeDBA with a running mean of e.g., 1 second (k = 11). Should be tested with more interval lengths. 
  
  # Calculation of the static acceleration in each axis by smoothing of the data with a rolling mean
  tl <- tl %>%
    dplyr::mutate(STx = zoo::rollmean(X, k = 11, fill = NA),
                  STy = zoo::rollmean(Y, k = 11, fill = NA),
                  STz = zoo::rollmean(Z, k = 11, fill = NA))
  
  # Calculation of the dynamic body acceleration in each axis (raw acceleration data - static acceleration)
  tl <- tl %>% mutate(DBAx = X - STx,
                      DBAy = Y - STy,
                      DBAz = Z - STz)
  
  # Calculation of the vectorial (VeDBA) and overal (ODBA) dynamic body acceleration
  tl <- tl %>% mutate(VeDBA = sqrt(DBAx^2 + DBAx^2 + DBAy^2))
  tl <- tl %>% mutate(ODBA = abs(DBAx) + abs(DBAy) + abs(DBAz))
  
  
  ## Calculate standard deviation (and other useful metrics) in rolling intervals
  
  
  # tl <- tl %>%
  #   dplyr::mutate(SDx = zoo::rollapply(X, width = 11, FUN = sd, fill = NA),
  #                 SDy = zoo::rollapply(Y, width = 11, FUN = sd, fill = NA),
  #                 SDz = zoo::rollapply(Z, width = 11, FUN = sd, fill = NA))
  
  tl <- tl %>%
    dplyr::mutate(SDdbax = zoo::rollapply(DBAx, width = 11, FUN = sd, fill = NA),
                  SDdbay = zoo::rollapply(DBAy, width = 11, FUN = sd, fill = NA),
                  SDdbaz = zoo::rollapply(DBAz, width = 11, FUN = sd, fill = NA))
  
  tl <- tl %>%
    dplyr::mutate(MAXdbax = zoo::rollapply(DBAx, width = 11, FUN = max, fill = NA),
                  MAXdbay = zoo::rollapply(DBAy, width = 11, FUN = max, fill = NA),
                  MAXdbaz = zoo::rollapply(DBAz, width = 11, FUN = max, fill = NA))
  
return(tl)
}
