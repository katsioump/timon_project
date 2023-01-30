setwd("C:/Users/Katerina/Documents/Master thesis/obs")


library(deSolve) # note due to some kind of bug in deSolve, it must be loaded before NicheMapR!
library(NicheMapR)
library(zoo)

#load(file='weatherIVILAR12.RData')
#load(file='weatherIVILAR12_hourly.RData')

# get microclimate data
#loc <- c(41.1073, -8.5897) # mesocosm
#Usrhyt <- 0.05 # height of midpoint of animal, m
#micro1 <- micro_global(loc = loc, Usrhyt = Usrhyt) # run the model with specified location and animal height

#source("micro_custom.R")
#micro_custom(41.1073, -8.5897, 230, weather, th=F, habitat='herb', slope=0, azmuth=180)


#metout <- as.data.frame(micro$metout) # above ground microclimatic conditions, min shade
#soil <- as.data.frame(micro$soil) # soil temperatures, minimum shade

#with(weather_h, plot(5/9 * (Temperature - 32) ~ date, type = 'l', col = '1', ylim = c(5, 35)))
#with(weather_h, plot(Precip..Rate. ~ date, type = 'l', col = '1'))




##### Approximate metout dataset for every 1 second

tl_m <- subset(tl, select = c('Date', 'Time', 'date_time', 'TemperatureC'))
tl_m <- na.omit(tl_m)


metout_new <- metout[0,]
for (i in nrow(tl_m)) {
  metout_new[i,] <- NA
}

metout_new$dates <- tl_m$date_time

metout$dates <- strptime(metout$dates, format = "%Y-%m-%d %H:%M:%S")
metout$dates <- as.POSIXct(metout$dates)
metout_new$dates <- strptime(metout_new$dates, format = "%Y-%m-%d %H:%M:%S")
metout_new$dates <- as.POSIXct(metout_new$dates)

j <- 1
for (i in 1:nrow(metout)) {
  for (z in colnames(metout)){
   metout_new[j, z] <- metout[i, z] 
  }
  j <- j + 3600
}

metout_new <- metout_new[!(metout_new$dates > max(metout$dates)),]


for (z in colnames(metout_new)){
  metout_new[, z] <- na.approx(metout_new[,z])
}

tl_m <- tl_m[!(tl_m$date_time > max(metout$dates)),]
metout_new$dates <- tl_m$date_time

vec_time <- rep(seq(0,1380), )

time_met <- rep(seq(0,86399),4)
time_met <- head(time_met, -((length(time_met) - nrow(metout_new)))) 
metout_new$TIME <- time_met

metout_new <- cbind(metout_new, tl_m$TemperatureC)
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
  j <- j + 3600
}

soil_new <- soil_new[!(soil_new$dates > max(soil$dates)),]


for (z in colnames(soil_new)){
  soil_new[, z] <- na.approx(soil_new[,z])
}

soil_new$dates <- tl_m$date_time
soil_new$TIME <- time_met

metout <- metout_new
soil <- soil_new

# append dummy dates
days <- rep(seq(1, 4), 86400)
days <- days[order(days)]
days <- head(days, -((length(days) - nrow(metout)))) 
# dates <- days + metout$TIME / 60 / 24 - 1 # dates for hourly output
# dates2 <- seq(1, 12, 1) # dates for daily output

dates <- days + metout_new$TIME / 1 / 86400 - 1 # dates for output every secon

metout$dates <- dates
soil$dates <- dates


# metout <- cbind(dates, metout)
# soil <- cbind(dates, soil)

# combine relevant input fields
microclimate <- cbind(metout[, 1:5], metout[, 8], soil[, 4], metout[, 13:15], metout[, 6], metout[, 21])
colnames(microclimate) <- c('dates', 'DOY', 'TIME', 'TALOC', 'TA1.2m', 'VLOC', 'TS', 'ZEN', 'SOLR', 'TSKYC', 'RHLOC', 'TASEN')
str(microclimate)


# define animal parameters - here simulating a 1000 g ellipsoid
c_body <- 3762 # specific heat of flesh, J/kg-C
rho_body <- 1000 # animal density, kg/m3
q <- 0 # metabolic rate, W/m3
k_flesh <- 0.5 # thermal conductivity of flesh, W/mK
geom <- 2 # shape, - 2: ellipsoid, 3: lizard
posture <- 'n' # pointing normal 'n' or parallel 'p' to the sun's rays, or average 'a'?
orient <- 1 # does the object orient toward the sun? (0,1)
shape_b <- 1/5 # shape coefficient b, -
shape_c <- 1/5 # shape coefficient c, -
shape_coefs <- c(10.4713, 0.688, 0.425, 0.85, 3.798, 0.683, 0.694, 0.743)
fatosk <- 0.4 # solar configuration factor to sky, -
fatosb <- 0.4 # solar configuration factor to substrate, -
alpha <- 0.9 # animal solar absorptivity, -
emis <- 0.95 # emissivity of skin, -
Ww_g <- 81.1 # weight, g
alpha_sub <- 0.8 # substrate solar absorptivity, -
press <- 101325 # air pressure, Pa
pdif <- 0.1 # proportion of solar energy that is diffuse, -



# use approxfun to create interpolations for the required environmental variables
nhs <- nrow(microclimate)
# time <- seq(0, 60 * nhs, 60) #60 minute intervals from microclimate output
# time <- time * 60 # minutes to seconds

time <- seq(0, nhs)
hours <- time/3600 # seconds to hours

Qsolf <- approxfun(time, c(microclimate[, 9], (microclimate[1, 9] + microclimate[nhs, 9]) /
                             2), rule = 2)
# approximate radiant temperature as the average of sky and substrate temperature
Tradf <- approxfun(time, rowMeans(cbind(c(microclimate[, 7], (microclimate[1, 7] + microclimate[nhs, 7]) / 2), c(microclimate[, 10], (microclimate[1, 10] + microclimate[nhs, 10]) / 2)), na.rm = TRUE), rule = 2)
velf <- approxfun(time, c(microclimate[, 6], (microclimate[1, 6] + microclimate[nhs, 6]) / 2), rule = 2)
Tairf <- approxfun(time, c(microclimate[, 12], (microclimate[1, 12] + microclimate[nhs, 12]) / 2), rule = 2)
Zenf <- approxfun(time, c(microclimate[, 8], 90), rule = 2)

#t = seq(1, 3600 * nhs, 60) # sequence of times for predictions (1 min intervals)
t = seq(1, nhs)
indata<-list(alpha = alpha, emis = emis, alpha_sub = alpha_sub, press = press, Ww_g = Ww_g, c_body = c_body, rho_body = rho_body, q = q, k_flesh = k_flesh, geom = geom, posture = posture, shape_b = shape_b, shape_c = shape_c, shape_coefs = shape_coefs, pdif = pdif, fatosk = fatosk, fatosb = fatosb, Qsolf = Qsolf, Tradf = Tradf, Tairf = Tairf, velf = velf, Zenf = Zenf)

Tc_init<-Tairf(1) # set initial Tc as air temperature

Tbs_ode <- as.data.frame(ode(y = Tc_init, t = time, func = onelump_var, parms = indata))
colnames(Tbs_ode) <- c('time', 'Tc', 'Tcf', 'tau', 'dTdt')
Tbs_ode$time <- Tbs_ode$time / 3600 # convert to hours

with(Tbs_ode, plot(Tc ~ time, type = 'l', col = '1', ylim = c(-10, 80), ylab='Temperature, °C',xlab = 'hour of simulation'))
#with(Tbs_ode, points(Tc ~ time, type = 'l', lwd=2))
with(Tbs_ode, points(Tcf ~ time, type = 'l', col = '2'))
points(Tairf(time) ~ hours, type = 'l', col = 'blue', lty = 2)
legend(100,80, c("Tc", "Tcf", "Tair"), lty = c(1, 1, 2), lwd = c(2.5, 2.5, 2.5), col = c("black", "red", "blue"))
