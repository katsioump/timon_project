+4°C",library(deSolve) # note due to some kind of bug in deSolve, it must be loaded before NicheMapR!
library(NicheMapR)
library(tidyverse)
library(zoo)
library(lubridate)
library(dplyr)
library(ggplot2)


# define animal biophysical functional traits (TLCRP005 for this example)
Ww_g <- 50 # wet weight (g)
Usrhyt <- 0.01 # height of animal (mid-point) above ground (m)
alpha <- 0.9 # solar absorptivity (-)
T_F_min <- 29 # 36.21927579 # minimum foraging Tb (deg C)
T_F_max <- 36 # maximum foraging Tb (deg C)
T_B_min <- 12 # basking Tb, moving from shade to sun (deg C)
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

#load("data/micro_ncep_2022.Rda")
#load("data/micro_ncep_warm_2.Rda")
load("data/micro_ncep_warm_4.Rda")

########################

# try1 
# micro_test <- micro_global(loc = loc, timeinterval = 12)
# micro <- micro_test

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


mons <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
DOYs <- unique(metout$DOY)

# run function for the middle day of each month
# days <- c(1:29, 31:32, 34:39, 41:85, 87:153, 155:161, 164:214, 216:260, 262:301, 304:313, 316:365)
days <- c(1:365)


# loop through each month and run transient model with behaviour
for(i in days){
  
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
  trans <- tryCatch(
    trans_behav(
      Ww_g = Ww_g, alpha = alpha, T_F_min = T_F_min, T_F_max = T_F_max,
      CT_max = CT_max, T_B_min = T_B_min, geom = geom, shape_b = shape_b, shape_c = shape_c,
      rho_body = rho_body, k_flesh = k_flesh, q = q, lump = 1,
      metout = metout_in, shadmet = shadmet_in, soil = soil_in, shadsoil = shadsoil_in,
      press = press, alpha_sub = 1 - micro$REFL, shade = maxshade
    ),
    error = function(e) {
      return(NULL)
    }
  )
  
  if (is.null(trans)) {
    print(days[i])
    days <- days[! days %in% c(i)]
    next
  }
  
  results <- as.data.frame(trans$day_results)
  sum_stats <- as.data.frame(trans$sum_stats)
  act_window <- as.data.frame(trans$act_window)
  
  # collate
  if(i == days[1]){
    all_act_window <- act_window
    all_stats <- sum_stats
  }else{
    all_act_window <- rbind(all_act_window, act_window)
    all_stats <- rbind(all_stats, sum_stats)
  }
  
  results$hours <- results$time / 3600
  
  # plot hourly results for the current day
  plot(results$Tb_open ~ results$hours, type = 'l', ylim = c(-10, 80), col = 'grey', xaxs = 'i', ylab = "temperature, deg C", xlab = "time",
       main = paste0(as.Date(paste(2022, DOYs[i]), format = "%Y %j"), ", ", Ww_g, "g"), xlim = c(0, 23))
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

all_stats <- cbind(DOY = days, all_stats)

metout <- subset(metout, DOY %in% days)
shadmet <- subset(shadmet, DOY %in% days)
soil <- subset(soil, DOY %in% days)
shadsoil <- subset(shadmet, DOY %in% days)

par(mar=c(5,5,4,3))

# make seasonal activity plot
all_act_window$ZEN <- metout$ZEN
all_act_window$DOY <- metout$DOY
foraging<-subset(all_act_window, forage_sun > 0)
night<-subset(all_act_window, ZEN==90)
with(night, plot(time ~ DOY, pch=15, cex = 1.8, cex.main = 1.7, cex.lab = 2.8, cex.axis = 2, xlim = c(1, 365), col = 'skyblue4', xlab = 'Day of year', ylab = 'Hour of day', main = "Seasonal Activity Plot of a Generalist, 50 gr, 2022, Sun"))
with(foraging, points(time ~ DOY, pch = 15, cex = forage_sun / 90, col = 'orange2'))

foraging<-subset(all_act_window, forage_shd > 0)
with(night, plot(time ~ DOY, pch=15, cex = 0.5, cex.main = 1.7, cex.lab = 2, cex.axis = 2, xlim = c(1, 365), col = 'dark blue', xlab = 'Day of year', ylab = 'Hour of day', main = "Seasonal Activity Plot of a Specialist, 50 gr, Change +4°C, Shade"))
with(foraging, points(time ~ DOY, pch = 15, cex = forage_shd / 80, col = 'orange'))



#Change +4°C ; Change +2°C

mtext(text =  paste0('Seasonal Activity Plot, ', if(length(loc) == 2){paste("lon", loc[1], "lat", loc[2])}else{loc}, " ", Ww_g," g"), outer = TRUE, side = 3, line = 0)

# calculate days of activity/percentage of active days/hours active in total

hours_sun <- (round(all_stats$sum_activity_sun / 60))
hours_shd <- (round(all_stats$sum_activity_shd / 60))

activity_hours <- data.frame(days, hours_sun, hours_shd)


all_act_window$month <- NA

for (i in 1:(nrow(all_act_window)-1)){
  if (all_act_window[i, "DOY"] <= 31) {
    all_act_window[i, "month"] <- "JAN"
  } else if (all_act_window[i, "DOY"] > 31 & all_act_window[i, "DOY"] <= 59) {
    all_act_window[i, "month"] <- "FEB"
  } else if (all_act_window[i, "DOY"] > 59 & all_act_window[i, "DOY"] <= 90) {
    all_act_window[i, "month"] <- "MAR"
  } else if (all_act_window[i, "DOY"] > 90 && all_act_window[i, "DOY"] <= 120) {
    all_act_window[i, "month"] <- "APR"
  } else if (all_act_window[i, "DOY"] > 120 & all_act_window[i, "DOY"] <= 151) {
    all_act_window[i, "month"] <- "MAY"
  } else if (all_act_window[i, "DOY"] > 151 & all_act_window[i, "DOY"] <= 181) {
    all_act_window[i, "month"] <- "JUN"
  } else if (all_act_window[i, "DOY"] > 181 & all_act_window[i, "DOY"] <= 212) {
    all_act_window[i, "month"] <- "JUL"
  } else if (all_act_window[i, "DOY"] > 212 & all_act_window[i, "DOY"] <= 243) {
    all_act_window[i, "month"] <- "AUG"
  } else if (all_act_window[i, "DOY"] > 243 & all_act_window[i, "DOY"] <= 273) {
    all_act_window[i, "month"] <- "SEP"
  } else if (all_act_window[i, "DOY"] > 273 & all_act_window[i, "DOY"] <= 304) {
    all_act_window[i, "month"] <- "OCT"
  } else if (all_act_window[i, "DOY"] > 304 & all_act_window[i, "DOY"] <= 334) {
    all_act_window[i, "month"] <- "NOV"
  } else {
    all_act_window[i, "month"] <- "DEC"
  }
}

# specific activity for
## shade

#plot(activity_hours, type = "l")

print(paste0("days active per year (sun): ", length(which(activity_hours$hours_sun != 0))))
print(paste0("days active per year (shadow): ", length(which(activity_hours$hours_shd != 0))))

print(paste0("average hours of activity per month: ", sum(activity_hours$hours)/12))

#average hours of activity per month
hours_jan <- format(round(sum(activity_hours[1:31,]$hours)/31, 2), nsmall = 2)
hours_feb <- format(round(sum(activity_hours[32:59,]$hours)/28, 2), nsmall = 2)
hours_mar <- format(round(sum(activity_hours[60:90,]$hours)/31, 2), nsmall = 2)
hours_apr <- format(round(sum(activity_hours[91:120,]$hours)/30, 2), nsmall = 2)
hours_may <- format(round(sum(activity_hours[121:151,]$hours)/31, 2), nsmall = 2)
hours_jun <- format(round(sum(activity_hours[152:181,]$hours)/30, 2), nsmall = 2)
hours_jul <- format(round(sum(activity_hours[182:212,]$hours)/31, 2), nsmall = 2)
hours_aug <- format(round(sum(activity_hours[213:243,]$hours)/31, 2), nsmall = 2)
hours_sep <- format(round(sum(activity_hours[244:273,]$hours)/30, 2), nsmall = 2)
hours_oct <- format(round(sum(activity_hours[274:304,]$hours)/31, 2), nsmall = 2)
hours_nov <- format(round(sum(activity_hours[305:334,]$hours)/30, 2), nsmall = 2)
hours_dec <- format(round(sum(activity_hours[335:365,]$hours)/31, 2), nsmall = 2)


print(paste0("average hours of activity in January: ", hours_jan))
print(paste0("average hours of activity in February: ", hours_feb))
print(paste0("average hours of activity in March: ", hours_mar))



###################################
#plot with ggplot2
#############################################

all_act_window$ZEN <- metout$ZEN 
all_act_window$DOY <- metout$DOY
night <- subset(all_act_window, ZEN == 90)

foraging_sun <- subset(all_act_window, forage_sun > 0)


night_df <- data.frame(DOY = DOYs,
                       night_start = NA,
                       night_end = NA)

for(i in days) {
  a <- min(which(night$DOY == i))
  b <- max(which(night$DOY == i))
  for (j in a:(b-1)){
    if ((night[j, 1] + 1) != night[j+1, 1]){
      night_df$night_start[i] <- night[j+1, 1]
      night_df$night_end[i] <- night[j, 1]
    }
  }
}

for (i in 2:nrow(night_df)) {
  if (is.na(night_df$night_start[i])) {
    night_df$night_start[i] <- night_df$night_start[i - 1]
  }
  if (is.na(night_df$night_end[i])) {
    night_df$night_end[i] <- night_df$night_end[i - 1]
  }
}


ggplot() +
  # Night 1
  geom_ribbon(data = night_df,
              aes(x = DOY, ymin = 0, ymax = night_end), fill = "skyblue4") +
  
  # Night 2
  geom_ribbon(data = night_df,
              aes(x = DOY, ymin = night_start, ymax = 24), fill = "skyblue4") +
  
  # Activity
  geom_point(data = foraging_sun,
             shape = 16,
             aes(x = DOY, y = time),
             color = "#f8766d", size = 0.3) +
  

  scale_y_continuous(breaks = seq(0,24,5), limits = c(0,24), expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(0,360,100), limits = c(1,365), expand = c(0.01, 0.01)) +
  
  labs(title = "Generalist, 50 gr, 2022", #+4°C",
       x = "Day of year", y = "Hour of day") +
  theme_classic(base_size = 10)

##############################
p_total <- (p1 | p2) / (p3 | p4) +
  plot_annotation(tag_levels = "a")

ggsave("results/figures/patterns.tiff", plot = p_total,
       width = 2 * 8.5, height = 2 * 7, units = "cm", dpi = 600, compression = "lzw")
###############################


ggplot(activity_hours, aes(days, hours)) +
  geom_point(size = 0.1) +
  geom_line(linewidth = 0.8) +
  labs(x = "Day of year", y = "Active hours/day") +
  ylim(0, 12) +
  scale_y_continuous(breaks = seq(0,12,4), limits = c(0,13), expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(0,360,100), limits = c(1,365), expand = c(0.01, 0.01)) +
  theme_classic(base_size = 10)


ggplot(activity_hours, aes(days, hours)) +
  geom_smooth(method = "loess", span = 0.01, se = FALSE, linewidth = 1.2, color = "black") +
  labs(x = "Day of year", y = "Active hours/day") +
  ylim(0, 12) +
  scale_y_continuous(breaks = seq(0,12,4), limits = c(0,13), expand = c(0.01, 0.01)) +
  #scale_x_continuous(breaks = seq(0,360,100), limits = c(1,365), expand = c(0.01, 0.01)) +
  theme_classic(base_size = 10) +
  theme(
    axis.line = element_line(linewidth = 2),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )

# setwd("C:/Users/Katerina/Documents/Master thesis/model")

library(deSolve) # note due to some kind of bug in deSolve, it must be loaded before NicheMapR!
library(NicheMapR)
library(tidyverse)
library(zoo)
library(lubridate)

# define animal biophysical functional traits (TLCRP005 for this example)
Ww_g <- 100 # wet weight (g)
Usrhyt <- 0.01 # height of animal (mid-point) above ground (m)
alpha <- 0.9 # solar absorptivity (-)
T_F_min <- 23 # 36.21927579 # minimum foraging Tb (deg C)
T_F_max <- 35 # maximum foraging Tb (deg C)
T_B_min <- 12 # basking Tb, moving from shade to sun (deg C)
CT_max <- 45 # critical thermal maximum (deg C)
shape_b <- 1 / 5 # shape coefficient a, -
shape_c <- 1 / 5 # shape coefficient b,
rho_body <- 1000 # animal density, kg/m3
c_body <- 3762 # heat capacity (J/kg-C)
q <- 0 # metabolic rate, W/m3
k_flesh <- 0.5 # thermal conductivity of flesh, W/mK
geom <- 2 # shape, -

# get microclimate data
loc <- c(-8.5898, 41.1073)
maxshade <- 90

# load("C:/Users/Katerina/Documents/Master thesis/micro/micro_ncep_2022.Rda")
load("../data/micro_ncep_2022.Rda")

########################

# try1
# micro_test <- micro_global(loc = loc, timeinterval = 12)
# micro <- micro_test

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
press <- 101325 * ((1 - (0.0065 * elevation / 288))^(1 / 0.190284))


mons <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
DOYs <- unique(metout$DOY)

# run function for the middle day of each month
# days <- c(1:29, 31:32, 34:39, 41:85, 87:153, 155:161, 164:214, 216:260, 262:301, 304:313, 316:365)
days <- 1:365

# loop through each month and run transient model with behaviour
for (i in days) {
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
  trans <- tryCatch(
    trans_behav(
      Ww_g = Ww_g, alpha = alpha, T_F_min = T_F_min, T_F_max = T_F_max,
      CT_max = CT_max, T_B_min = T_B_min, geom = geom, shape_b = shape_b, shape_c = shape_c,
      rho_body = rho_body, k_flesh = k_flesh, q = q, lump = 1,
      metout = metout_in, shadmet = shadmet_in, soil = soil_in, shadsoil = shadsoil_in,
      press = press, alpha_sub = 1 - micro$REFL, shade = maxshade
    ),
    error = function(e) {
      return(NULL)
    }
  )

  if (is.null(trans)) {
    next
  }

  results <- as.data.frame(trans$day_results)
  sum_stats <- as.data.frame(trans$sum_stats)
  act_window <- as.data.frame(trans$act_window)

  # collate
  if (i == days[1]) {
    all_act_window <- act_window
  } else {
    all_act_window <- rbind(all_act_window, act_window)
  }

  results$hours <- results$time / 3600

  # plot hourly results for the current day
  plot(results$Tb_open ~ results$hours,
    type = "l", ylim = c(-10, 80), col = "grey", xaxs = "i", ylab = "temperature, deg C", xlab = "time",
    main = paste0(as.Date(paste(2022, DOYs[i]), format = "%Y %j"), ", ", Ww_g, "g"), xlim = c(0, 23)
  )
  grid(nx = 23, ny = 0, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
  abline(T_F_max, 0, col = "red", lty = 2)
  abline(T_F_min, 0, col = "light blue", lty = 2)
  abline(CT_max, 0, col = "red")
  points(results$T_air_shd ~ results$hours, type = "l", col = "blue")
  points(results$Tb ~ results$hours, type = "l", col = "orange", lty = 1, lwd = 2)
  text(3, 60, paste0("bouts ", round(sum_stats$bouts_sun, 0)), cex = 1)
  text(3, 65, paste0("maximum bout ", round(sum_stats$max_foraging_bout_sun / 60, 1), " hrs"), cex = 1)
  text(3, 70, paste0("total activity ", round(sum_stats$sum_activity_sun / 60, 1), " hrs"), cex = 1)
}


metout <- subset(metout, DOY %in% days)
shadmet <- subset(shadmet, DOY %in% days)
soil <- subset(soil, DOY %in% days)
shadsoil <- subset(shadmet, DOY %in% days)

# make seasonal activity plot
all_act_window$ZEN <- metout$ZEN
all_act_window$DOY <- metout$DOY
foraging <- subset(all_act_window, forage_sun > 0)
night <- subset(all_act_window, ZEN == 90)
with(night, plot(time ~ DOY, pch = 15, cex = 0.6, xlim = c(1, 365), col = "dark blue", xlab = "day of year", ylab = "hour of day", main = "Seasonal Activity Plot of a Generalist, 100 gr, (2022), Sun"))
with(foraging, points(time ~ DOY, pch = 15, cex = forage_sun / 80, col = "orange"))
foraging <- subset(all_act_window, forage_shd > 0)
with(night, plot(time ~ DOY, pch = 15, cex = 0.5, xlim = c(1, 365), col = "dark blue", xlab = "day of year", ylab = "hour of day", main = "Seasonal Activity Plot of a Generalist, 100 gr, (2022), Shade"))
with(foraging, points(time ~ DOY, pch = 15, cex = forage_shd / 90, col = "orange"))


mtext(text = paste0("Seasonal Activity Plot, ", if (length(loc) == 2) {
  paste("lon", loc[1], "lat", loc[2])
} else {
  loc
}, " ", Ww_g, " g"), outer = TRUE, side = 3, line = 0)

### ggplot2 figures ###

all_act_window$ZEN <- metout$ZEN 
all_act_window$DOY <- metout$DOY
night <- subset(all_act_window, ZEN == 90)

foraging_sun <- subset(all_act_window, forage_sun > 0)


night_df <- data.frame(DOY = DOYs,
                       night_start = NA,
                       night_end = NA)

for(i in days) {
  a <- min(which(night$DOY == i))
  b <- max(which(night$DOY == i))
  for (j in a:(b-1)){
    if ((night[j, 1] + 1) != night[j+1, 1]){
      night_df$night_start[i] <- night[j+1, 1]
      night_df$night_end[i] <- night[j, 1]
    }
  }
}

for (i in 2:nrow(night_df)){
  if (is.na(night_df[i,"night_start"])){
    night_df[i,"night_start"] <- night_df[(i-1),"night_start"]
    night_df[i,"night_end"] <- night_df[(i-1),"night_end"]
  }
}


g1 <- ggplot() +
  # Night 1
  geom_ribbon(data = night_df,
              aes(x = DOY, ymin = 0, ymax = night_end), fill = "skyblue4") +
  
  # Night 2
  geom_ribbon(data = night_df,
              aes(x = DOY, ymin = night_start, ymax = 24), fill = "skyblue4") +
  
  # Activity
  geom_point(data = foraging_sun,
             shape = 19,
             aes(x = DOY, y = time),
             color = "orange2", size = 0.3) +
  
  
  scale_y_continuous(breaks = seq(0,24,5), limits = c(0,24), expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(0,360,100), limits = c(1,365), expand = c(0.01, 0.01)) +
  
  labs(title = "a              Specialist, 50 gr, 2022", #+4°C",
       x = "Day of year", y = "Hour of day") +
  
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20)
  )


#####################

load("~/Master thesis/model/trans_spe_50_plus4.RData")

all_stats <- cbind(DOY = days, all_stats)

metout <- subset(metout, DOY %in% days)
shadmet <- subset(shadmet, DOY %in% days)
soil <- subset(soil, DOY %in% days)
shadsoil <- subset(shadmet, DOY %in% days)

all_act_window$ZEN <- metout$ZEN 
all_act_window$DOY <- metout$DOY
night <- subset(all_act_window, ZEN == 90)

foraging_sun <- subset(all_act_window, forage_sun > 0)


night_df <- data.frame(DOY = DOYs,
                       night_start = NA,
                       night_end = NA)

for(i in days) {
  a <- min(which(night$DOY == i))
  b <- max(which(night$DOY == i))
  for (j in a:(b-1)){
    if ((night[j, 1] + 1) != night[j+1, 1]){
      night_df$night_start[i] <- night[j+1, 1]
      night_df$night_end[i] <- night[j, 1]
    }
  }
}

for (i in 2:nrow(night_df)){
  if (is.na(night_df[i,"night_start"])){
    night_df[i,"night_start"] <- night_df[(i-1),"night_start"]
    night_df[i,"night_end"] <- night_df[(i-1),"night_end"]
  }
}


g2 <- ggplot() +
  # Night 1
  geom_ribbon(data = night_df,
              aes(x = DOY, ymin = 0, ymax = night_end), fill = "skyblue4") +
  
  # Night 2
  geom_ribbon(data = night_df,
              aes(x = DOY, ymin = night_start, ymax = 24), fill = "skyblue4") +
  
  # Activity
  geom_point(data = foraging_sun,
             shape = 19,
             aes(x = DOY, y = time),
             color = "orange2", size = 0.3) +
  
  
  scale_y_continuous(breaks = seq(0,24,5), limits = c(0,24), expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(0,360,100), limits = c(1,365), expand = c(0.01, 0.01)) +
  
  labs(title = "b              Specialist, 50 gr, +4°C",
       x = "Day of year", y = "Hour of day") +
  
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20)
  )

#########################

load("~/Master thesis/model/trans_gen_50_2022.RData")

all_act_window$ZEN <- metout$ZEN 
all_act_window$DOY <- metout$DOY
night <- subset(all_act_window, ZEN == 90)

foraging_sun <- subset(all_act_window, forage_sun > 0)


night_df <- data.frame(DOY = DOYs,
                       night_start = NA,
                       night_end = NA)

for(i in days) {
  a <- min(which(night$DOY == i))
  b <- max(which(night$DOY == i))
  for (j in a:(b-1)){
    if ((night[j, 1] + 1) != night[j+1, 1]){
      night_df$night_start[i] <- night[j+1, 1]
      night_df$night_end[i] <- night[j, 1]
    }
  }
}

for (i in 2:nrow(night_df)){
  if (is.na(night_df[i,"night_start"])){
    night_df[i,"night_start"] <- night_df[(i-1),"night_start"]
    night_df[i,"night_end"] <- night_df[(i-1),"night_end"]
  }
}


g3 <- ggplot() +
  # Night 1
  geom_ribbon(data = night_df,
              aes(x = DOY, ymin = 0, ymax = night_end), fill = "skyblue4") +
  
  # Night 2
  geom_ribbon(data = night_df,
              aes(x = DOY, ymin = night_start, ymax = 24), fill = "skyblue4") +
  
  # Activity
  geom_point(data = foraging_sun,
             shape = 19,
             aes(x = DOY, y = time),
             color = "orange2", size = 0.3) +
  
  
  scale_y_continuous(breaks = seq(0,24,5), limits = c(0,24), expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(0,360,100), limits = c(1,365), expand = c(0.01, 0.01)) +
  
  labs(title = "c              Generalist, 50 gr, 2022", #+4°C",
       x = "Day of year", y = "Hour of day") +
  
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20)
  )


##############

load("~/Master thesis/model/trans_gen_50_plus4.RData")

all_act_window$ZEN <- metout$ZEN 
all_act_window$DOY <- metout$DOY
night <- subset(all_act_window, ZEN == 90)


foraging_sun <- subset(all_act_window, forage_sun > 0)


night_df <- data.frame(DOY = DOYs,
                       night_start = NA,
                       night_end = NA)

for(i in days) {
  a <- min(which(night$DOY == i))
  b <- max(which(night$DOY == i))
  for (j in a:(b-1)){
    if ((night[j, 1] + 1) != night[j+1, 1]){
      night_df$night_start[i] <- night[j+1, 1]
      night_df$night_end[i] <- night[j, 1]
    }
  }
}


for (i in 2:nrow(night_df)){
  if (is.na(night_df[i,"night_start"])){
    night_df[i,"night_start"] <- night_df[(i-1),"night_start"]
    night_df[i,"night_end"] <- night_df[(i-1),"night_end"]
  }
}


g4 <- ggplot() +
  # Night 1
  geom_ribbon(data = night_df,
              aes(x = DOY, ymin = 0, ymax = night_end), fill = "skyblue4") +
  # Night 2
  geom_ribbon(data = night_df,
              aes(x = DOY, ymin = night_start, ymax = 24), fill = "skyblue4") +
  # Activity
  geom_point(data = foraging_sun,
             shape = 19,
             aes(x = DOY, y = time),
             color = "orange2", size = 0.3) +
  
  
  scale_y_continuous(breaks = seq(0,24,5), limits = c(0,24), expand = c(0.01, 0.01)) +
  scale_x_continuous(breaks = seq(0,360,100), limits = c(1,365), expand = c(0.01, 0.01)) +
  
  labs(title = "d              Generalist, 50 gr, +4°C",
       x = "Day of year", y = "Hour of day") +
  
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20)
  )

g_all <- grid.arrange(g1, g2, g3, g4, nrow = 2)
