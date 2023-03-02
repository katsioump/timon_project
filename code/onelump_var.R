setwd("C:/Users/Katerina/Documents/Master thesis/model")

library(deSolve) # note due to some kind of bug in deSolve, it must be loaded before NicheMapR!
library(NicheMapR)
library(zoo)
library(stringr)
library(lubridate)

load(file='weatherIVILAR12_hourly.RData')
weather <- weather_h

n_sensor <- 8
s <- str_pad(n_sensor, 2, pad = "0")
path <- paste0("C:\\Users\\Katerina\\Desktop\\mesocosms\\final\\TLCRP0", s, ".csv")
tl <- read.csv(file = path, head = TRUE)
tl <- subset(tl, select = -c(1))

tl$date_time <- strptime(tl$date_time, format = "%Y-%m-%d %H:%M:%OS")
tl$date_time <- as.POSIXct(tl$date_time)

# weather <- weather[weather$date >= (tl[1, 'date_time'] + 3600) & weather$date <= (tl[nrow(tl), 'date_time'] + 3600), ]


source("micro_custom.R")
micro_out <- micro_custom(41.1073, -8.5898, 230, weather, tl, th=F, habitat='soil', slope=0, azmuth=180)
# source("micro_hourly.R")
# micro_out <- micro_hourly(41.1073, -8.5898, 230, weather, tl)


metout <- as.data.frame(micro_out[1]) 
metout <- metout[metout$dates >= (tl[1, 'date_time'] + 3600) & metout$dates <= (tl[nrow(tl), 'date_time'] + 3600), ]
soil <- as.data.frame(micro_out[2])
soil <- soil[soil$dates >= (tl[1, 'date_time'] + 3600) & soil$dates <= (tl[nrow(tl), 'date_time'] + 3600), ]
shadmet <- as.data.frame(micro_out[3])
shadmet <- shadmet[shadmet$dates >= (tl[1, 'date_time'] + 3600) & shadmet$dates <= (tl[nrow(tl), 'date_time'] + 3600), ]
shadsoil <- as.data.frame(micro_out[4])
shadsoil <- shadsoil[shadsoil$dates >= (tl[1, 'date_time'] + 3600) & shadsoil$dates <= (tl[nrow(tl), 'date_time'] + 3600), ]



with(soil, plot(D0cm ~ dates, type = 'l', col = 'red', ylab='Soil temperature, °C'))
with(shadsoil, points(D0cm ~ dates, type = 'l'))
legend("topleft", inset=c(0.75,0.05), c("Soil", "Soil shadow"), lty = c(1, 1, 2), lwd = c(2.5, 2.5, 2.5), col = c("red", "black"))

with(metout, plot(TSKYC ~ dates, type = 'l', col = 'blue', ylab='Sky temperature, °C', ylim = c(0, 20)))
with(shadmet, points(TSKYC ~ dates, type = 'l'))
legend("topleft", inset=c(0.75,0.05), c("Clear sky", "Sky shadow"), lty = c(1, 1, 2), lwd = c(2.5, 2.5, 2.5), col = c("blue", "black"))

with(metout, plot(SOLR ~ dates, type = 'l', col = 'red', ylab='Solar radiation'))
with(shadmet, points(SOLR ~ dates, type = 'l'))
legend("topleft", inset=c(0.75,0.05), c("Soil", "Soil shadow"), lty = c(1, 1, 2), lwd = c(2.5, 2.5, 2.5), col = c("red", "black"))


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



###################################################################



tl_m <- subset(tl, select = c('date_time', 'TemperatureC', 'air_temp', 'rel_light'))
tl_m <- na.omit(tl_m)
tl_m <- tl_m[!(tl_m$date_time > (max(metout$dates) - 3600)), ]

# tl_min <- tl_m[0,]
# i <- 1
# while(i <= nrow(tl_m)) {
#    tl_min <- rbind(tl_min, tl_m[i,])
#    i <- i + 60
# }
# tl_m <- tl_min


source("interp_micro.R")
inter <- interp_micro(metout, soil, shadmet, shadsoil, tl_m)

metout <- as.data.frame(inter[1])
soil <- as.data.frame(inter[2])
shadmet <- as.data.frame(inter[3])
shadsoil <- as.data.frame(inter[4])

## for every min
# tl_min <- tl_m[0,]
# i <- 1
# for (i in 1:nrow(tl_m)) {
#   tl_min <- rbind(tl_min, tl_m[i,])
#   i <- i + 60
# }

################################################

# append dummy dates

if((tl_m[2,1] - tl_m[1,1]) == 1){
  days <- rep(seq(1, 4), 86400)  # for model every second
  days <- days[order(days)]
  days <- head(days, -((length(days) - nrow(metout)))) 
  dates <- days + metout$TIME / 86400 - 1 # dates for output every second
} else {
  days <- rep(seq(1, 4), 1440) # for model every minute
  days <- days[order(days)]
  days <- head(days, -((length(days) - nrow(metout)))) 
  dates <- days + metout$TIME / 1440 - 1
}


# dates <- days + metout$TIME / 60 / 24 - 1 # dates for hourly output
# dates2 <- seq(1, 12, 1) # dates for daily output


metout$dates <- dates
soil$dates <- dates
shadmet$dates <- dates
shadsoil$dates <- dates

# metout <- cbind(dates, metout)
# soil <- cbind(dates, soil)

# combine relevant input fields
microclimate <- cbind(metout[, 1:5], metout[, 8], soil[, 4], metout[, 13:15], metout[, 6], metout[, 21], shadmet[, 14:15], shadsoil[,4])
colnames(microclimate) <- c('dates', 'DOY', 'TIME', 'TALOC', 'TA1.2m', 'VLOC', 'TS', 'ZEN', 'SOLR', 'TSKYC', 'RHLOC', 'TASEN', 'SOLRsh', 'TSKYCsh', 'TSsh')
str(microclimate)


# define animal parameters
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
emis <- 0.95 # emisivity of skin, -
########### change according to animal
Ww_g <- 78.4 # weight, g
###########
alpha_sub <- 0.8 # substrate solar absorptivity, -
press <- 101325 # air pressure, Pa
pdif <- 0.1 # proportion of solar energy that is diffuse, -



# use approxfun to create interpolations for the required environmental variables
nhs <- nrow(microclimate)

if((tl_m[2,1] - tl_m[1,1]) == 1){
  time <- seq(0, nhs)
  t = seq(1, nhs)
} else {
  time <- seq(0, 60 * nhs, 60) #60 minute (second) intervals from microclimate output
  # time <- time * 60 # minutes to seconds
  t = seq(1, 60 * nhs, 60) # sequence of times for predictions (1 min intervals)
}

hours <- time/3600 # seconds to hours

# tl_m$lux[is.infinite(tl_m$lux) == T] <- NA
tl_m$rel_light[tl_m$rel_light > 100] <- 100 ### !!!!
max_shad <- 100 - min(tl_m[, 'rel_light'])

Tsky_w <- vector()
Qsol_w <- vector()
Ts_w <- vector()

Tsky_w <- (microclimate[, 'TSKYC'] * (max_shad - (100 - tl_m[, 'rel_light'])) / max_shad) + (microclimate[, 'TSKYCsh'] * ((100 - tl_m[, 'rel_light'])) / max_shad)
Qsol_w <- (microclimate[, 'SOLR'] * tl_m[, 'rel_light'] / 100)
Ts_w <- (microclimate[, 'TS'] * (max_shad - (100 - tl_m[, 'rel_light'])) / max_shad) + (microclimate[, 'TSsh'] * ((100 - tl_m[, 'rel_light'])) / max_shad)


microclimate <- cbind(microclimate, Tsky_w, Qsol_w, Ts_w)

with(microclimate, plot(SOLR ~ dates, type = 'l', col = 'blue'))
# with(microclimate, points(TSsh ~ dates, type = 'l'))
with(microclimate, points(Qsol_w ~ dates, type = 'l', col = 'red'))


Qsolf <- approxfun(time, c(microclimate[, 17], (microclimate[1, 17] + microclimate[nhs, 17]) /
                             2), rule = 2)
# approximate radiant temperature as the average of sky and substrate temperature
Tradf <- approxfun(time, rowMeans(cbind(c(microclimate[, 18], (microclimate[1, 18] + microclimate[nhs, 18]) / 2), c(microclimate[, 16], (microclimate[1, 16] + microclimate[nhs, 16]) / 2)), na.rm = TRUE), rule = 2)
velf <- approxfun(time, c(microclimate[, 6], (microclimate[1, 6] + microclimate[nhs, 6]) / 2), rule = 2)
Tairf <- approxfun(time, c(microclimate[, 12], (microclimate[1, 12] + microclimate[nhs, 12]) / 2), rule = 2)
Zenf <- approxfun(time, c(microclimate[, 8], 90), rule = 2)


indata<-list(alpha = alpha, emis = emis, alpha_sub = alpha_sub, press = press, Ww_g = Ww_g, c_body = c_body, rho_body = rho_body, q = q, k_flesh = k_flesh, geom = geom, posture = posture, shape_b = shape_b, shape_c = shape_c, shape_coefs = shape_coefs, pdif = pdif, fatosk = fatosk, fatosb = fatosb, Qsolf = Qsolf, Tradf = Tradf, Tairf = Tairf, velf = velf, Zenf = Zenf)

Tc_init<-Tairf(1) # set initial Tc as air temperature

Tbs_ode <- as.data.frame(ode(y = Tc_init, t = t, func = onelump_var, parms = indata))
colnames(Tbs_ode) <- c('time', 'Tc', 'Tcf', 'tau', 'dTdt')
Tbs_ode$time <- Tbs_ode$time / 3600 # convert to hours

with(Tbs_ode, plot(Tc ~ time, type = 'l', col = '1', ylim = c(0, 60), ylab='Temperature, °C',xlab = 'hour of simulation'))
#with(Tbs_ode, points(Tc ~ time, type = 'l', lwd=2))
# with(Tbs_ode, points(Tcf ~ time, type = 'l', col = 'blue'))
points(Tairf(time) ~ hours, type = 'l', col = 'red', lty = 2)
legend(75,60, c("Tc", "Tair"), lty = c(1, 1, 2), lwd = c(2.5, 2.5, 2.5), col = c("black", "red"))
abline(h = 40)

Tb <- seq(1, nrow(tl))
Tb[1:length(Tb)] <- NA
j <- 1
for (i in 1:nrow(tl_m)){
  Tb[j] <- Tbs_ode[i, 'Tc']
  j <- j + 10
}

tl <- cbind(tl, Tb)

write.csv(tl, path)
