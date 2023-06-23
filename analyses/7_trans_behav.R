setwd("C:/Users/Katerina/Documents/Master thesis/model")

library(deSolve) # note due to some kind of bug in deSolve, it must be loaded before NicheMapR!
library(NicheMapR)
library(tidyverse)
library(zoo)
library(lubridate)

# define animal biophysical functional traits (TLCRP005 for this example)
Ww_g <- 61.7 # wet weight (g)
Usrhyt <- 0.01 # height of animal (mid-point) above ground (m)
alpha <- 0.9 # solar absorptivity (-)
T_F_min <- 36.21927579 # minimum foraging Tb (deg C)
T_F_max <- 38.65927579 # maximum foraging Tb (deg C)
T_B_min <- 13.57565549 # basking Tb, moving from shade to sun (deg C)
CT_max <- 45 # critical thermal maximum (deg C)
shape_b <- 1/5 # shape coefficient a, -
shape_c <- 1/5 # shape coefficient b, 
rho_body <- 1000 # animal density, kg/m3
c_body <- 3762 # heat capacity (J/kg-C)
q <- 0 # metabolic rate, W/m3
k_flesh <- 0.5 # thermal conductivity of flesh, W/mK
geom <- 2 # shape, -

# get microclimate data
loc <- c(-8.5898, 41.1073)
maxshade <- 90

load("C:/Users/Katerina/Documents/Master thesis/micro/micro_ncep_2022.Rda")

metout <- as.data.frame(micro$metout) # above ground microclimatic conditions, min shade
soil <- as.data.frame(micro$soil) # soil temperatures, minimum shade
shadmet <- as.data.frame(micro$shadmet) # above ground microclimatic conditions, min shade
shadsoil <- as.data.frame(micro$shadsoil) # soil temperatures, minimum shade
dates <- micro$dates
metout <- cbind(metout, dates)
shadmet <- cbind(shadmet, dates)
soil <- cbind(soil, dates)
shadsoil <- cbind(shadsoil, dates)
# get air pressure
elevation <- micro$elev
press <- 101325 * ((1 - (0.0065 * elevation / 288)) ^ (1 / 0.190284))


########################

mons <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
DOYs <- unique(metout$DOY)
months <- unique(month(metout$dates))

source("C:/Users/Katerina/Documents/Master thesis/model/trans_fun.R")

# loop through each month and run transient model with behaviour
for(i in 1:1){
  
  # subset current month
 # metout_in <- subset(metout, month(metout$dates) == months[i]) 
 # shadmet_in <- subset(shadmet, month(shadmet$dates) == months[i])
 # soil_in <- subset(soil, month(soil$dates) == months[i])
 # shadsoil_in <- subset(shadmet, month(shadmet$dates) == months[i])
  
  metout_in <- subset(metout, DOY == DOYs[i])
  shadmet_in <- subset(shadmet, DOY == DOYs[i])
  soil_in <- subset(soil, DOY == DOYs[i])
  shadsoil_in <- subset(shadmet, DOY == DOYs[i])
  
  # run transient behavioural simulation
  trans <- trans_fun(Ww_g = Ww_g, alpha = alpha, T_F_min = T_F_min, T_F_max = T_F_max,
                       CT_max = CT_max, T_B_min = T_B_min, geom = geom, shape_b = shape_b, shape_c = shape_c,
                       rho_body = rho_body, k_flesh = k_flesh, q = q, lump = 1,
                       metout = metout_in, shadmet = shadmet_in, soil = soil_in, shadsoil = shadsoil_in,
                       press = press, alpha_sub = 1 - micro$REFL)
  
  results <- as.data.frame(trans$day_results)
  sum_stats <- as.data.frame(trans$sum_stats)
  act_window <- as.data.frame(trans$act_window)
  
  # collate
  if(i == 1){
    all_act_window <- act_window
  }else{
    all_act_window <- rbind(all_act_window, act_window)
  }
  
  results$hours <- results$time / 3600
  
  # plot hourly results for the current day
  plot(results$Tb_open ~ results$hours, type = 'l', ylim = c(-10, 80), col = 'grey', xaxs = 'i', ylab = "temperature, deg C", xlab = "time",
       main = paste0(if(length(loc) == 2){paste("lon", loc[1], "lat", loc[2])}else{loc}, ", ", mons[i], ", ", Ww_g," g"), xlim = c(0, 23))
  grid(nx = 23, ny = 0, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
  abline(T_F_max, 0, col = 'red', lty = 2)
  abline(T_F_min, 0, col = 'light blue', lty = 2)
  abline(CT_max, 0, col = 'red')
  points(results$T_air_shd ~ results$hours, type = 'l', col = 'blue')
  points(results$Tb ~ results$hours, type = 'l', col = 'orange', lty = 1, lwd = 2)
  text(3, 60, paste0("bouts ", round(sum_stats$bouts_sun, 0)), cex = 1)
  text(3, 65, paste0("maximum bout ", round(sum_stats$max_foraging_bout_sun / 60, 1), " hrs"), cex = 1)
  text(3, 70, paste0("total activity ", round(sum_stats$sum_activity_sun / 60, 1), " hrs"), cex = 1)
}

# make seasonal activity plot
all_act_window$ZEN <- metout$ZEN
all_act_window$DOY <- metout$DOY
foraging<-subset(all_act_window, forage_sun > 0)
night<-subset(all_act_window, ZEN==90)
with(night, plot(time ~ DOY, pch=15, cex = 2, xlim = c(1, 365), col = 'dark blue', xlab = 'day of year', ylab = 'hour of day', main = "Seasonal Activity Plot, Sun"))
with(foraging, points(time ~ DOY, pch = 15, cex = forage_sun / 30, col = 'orange'))
foraging<-subset(all_act_window, forage_shd > 0)
with(night, plot(time ~ DOY, pch=15, cex = 2, xlim = c(1, 365), col = 'dark blue', xlab = 'day of year', ylab = 'hour of day', main = "Seasonal Activity Plot, Shade"))
with(foraging, points(time ~ DOY, pch = 15, cex = forage_shd / 30, col = 'orange'))

mtext(text =  paste0('Seasonal Activity Plot, ', if(length(loc) == 2){paste("lon", loc[1], "lat", loc[2])}else{loc}, " ", Ww_g," g"), outer = TRUE, side = 3, line = 0)
