interp_micro <- function(metout, soil, shadmet, shadsoil, tl_m){
  
  ##### Approximate metout dataset for every 1 second
  
  if((tl_m[2,1] - tl_m[1,1]) == 1){
    j_add <- 3600 # for model every second
  } else {
    j_add <- 60 # for model every minute
  }
  
  metout_new <- metout[0,]
  for (i in nrow(tl_m)) {
    metout_new[i,] <- NA
  }
  
  metout_new$dates <- tl_m$date_time
  
  
  j <- 1
  for (i in 1:nrow(metout)) {
    for (z in colnames(metout)){
      metout_new[j, z] <- metout[i, z] 
    }
    j <- j + j_add
  }
  
  metout_new <- metout_new[!(metout_new$dates > max(metout$dates)),]
  
  
  for (z in colnames(metout_new)){
    metout_new[, z] <- na.approx(metout_new[,z])
  }
  
  tl_m <- tl_m[!(tl_m$date_time > max(metout[,'dates'])), ]
  metout_new$dates <- tl_m$date_time
  
  if((tl_m[2,1] - tl_m[1,1]) == 1){
    time_met <- rep(seq(0,86399), 4) # for model every second
  } else {
    time_met <- rep(seq(0,1439),4) # for model every minute
  }
  
  
  
  time_met <- head(time_met, -((length(time_met) - nrow(metout_new)))) 
  metout_new$TIME <- time_met
  
  metout_new <- cbind(metout_new, tl_m$TemperatureC)
  metout_new$TAREF <- tl_m$air_temp
  colnames(metout_new)[21] <- 'TASEN'
  
  ###### Approximate soil dataset for every 1 second
  
  soil_new <- soil[0,]
  for (i in nrow(tl_m)) {
    soil_new[i,] <- NA
  }
  
  soil_new$dates <- tl_m$date_time
  
  soil$dates <- strptime(soil$dates, format = "%Y-%m-%d %H:%M:%S")
  soil$dates <- as.POSIXct(soil$dates)
  soil_new$dates <- strptime(soil_new$dates, format = "%Y-%m-%d %H:%M:%S")
  soil_new$dates <- as.POSIXct(soil_new$dates)
  
  j <- 1
  for (i in 1:nrow(soil)) {
    for (z in colnames(soil)){
      soil_new[j, z] <- soil[i, z] 
    }
    j <- j + j_add
  }
  
  # soil_new <- soil_new[!(soil_new$dates > max(soil$dates)),]
  
  
  for (z in colnames(soil_new)){
    soil_new[, z] <- na.approx(soil_new[,z])
  }
  
  soil_new$dates <- tl_m$date_time
  soil_new$TIME <- time_met
  
  ###### Approximate shadmet dataset for every 1 second
  
  shadmet_new <- shadmet[0,]
  for (i in nrow(tl_m)) {
    shadmet_new[i,] <- NA
  }
  
  shadmet_new$dates <- tl_m$date_time
  
  shadmet$dates <- strptime(shadmet$dates, format = "%Y-%m-%d %H:%M:%S")
  shadmet$dates <- as.POSIXct(shadmet$dates)
  shadmet_new$dates <- strptime(shadmet_new$dates, format = "%Y-%m-%d %H:%M:%S")
  shadmet_new$dates <- as.POSIXct(shadmet_new$dates)
  
  j <- 1
  for (i in 1:nrow(shadmet)) {
    for (z in colnames(shadmet)){
      shadmet_new[j, z] <- shadmet[i, z] 
    }
    j <- j + j_add
  }
  
  # shadmet_new <- shadmet_new[!(shadmet_new$dates > max(shadmet$dates)),]
  
  
  for (z in colnames(shadmet_new)){
    shadmet_new[, z] <- na.approx(shadmet_new[,z])
  }
  
  shadmet_new$dates <- tl_m$date_time
  shadmet_new$TIME <- time_met
  
  ###### Approximate shadsoil dataset for every 1 second
  
  shadsoil_new <- shadsoil[0,]
  for (i in nrow(tl_m)) {
    shadsoil_new[i,] <- NA
  }
  
  shadsoil_new$dates <- tl_m$date_time
  
  shadsoil$dates <- strptime(shadsoil$dates, format = "%Y-%m-%d %H:%M:%S")
  shadsoil$dates <- as.POSIXct(shadsoil$dates)
  shadsoil_new$dates <- strptime(shadsoil_new$dates, format = "%Y-%m-%d %H:%M:%S")
  shadsoil_new$dates <- as.POSIXct(shadsoil_new$dates)
  
  j <- 1
  for (i in 1:nrow(shadsoil)) {
    for (z in colnames(shadsoil)){
      shadsoil_new[j, z] <- shadsoil[i, z] 
    }
    j <- j + j_add
  }
  
  # shadsoil_new <- shadsoil_new[!(shadsoil_new$dates > max(shadsoil$dates)),]
  
  
  for (z in colnames(shadsoil_new)){
    shadsoil_new[, z] <- na.approx(shadsoil_new[,z])
  }
  
  shadsoil_new$dates <- tl_m$date_time
  shadsoil_new$TIME <- time_met
  
  
  inter <- list(metout_new, soil_new, shadmet_new, shadsoil_new)
  
  return(inter)
}
