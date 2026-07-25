library(ggplot2)
library(stringr)
library(lubridate)


rm(list=ls())

#################################################################################

### Calculate total percentage of mobility for every lizard 

move <- data.frame(code=character(),
                   move=integer())

for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("data/final/TLCRP0", s, ".csv")
  tl <- read.csv(file = path, head = TRUE)
  tl_code <- paste0("TLCRP0", s)
  
  na_locs <- which(!complete.cases(tl$Move))
  tl <- tl[-na_locs,]
  
  # night <- which(tl$Time <= "06:29:00.000" | tl$Time >= "20:30:00.000")
  # tl_day <- tl[-night,]
  # tl_night <- tl[night, ]
  
  per_mov <- (nrow(tl[tl$Move == 'mobile', ]) * 100)/nrow(tl)
  
  tl_move <- data.frame(code=tl_code, move=per_mov)
  move <- rbind(move, tl_move)
}


#### Calculate mobile minutes per hour and visualize average mobility per hour (mins) for all animals

min_mov <- data.frame(code=character(),
                      day=integer(),
                      hour=integer(),
                      min=integer())


for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("data/final/TLCRP0", s, ".csv")
  tl <- read.csv(file = path, head = TRUE)
  tl_code <- paste0("TLCRP0", s)
  
  
  tl$date_time <- as.POSIXct(
    paste(tl$Date, tl$Time),
    format = "%Y-%m-%d %H:%M:%S.%OS",
    tz = "UTC"
  )
  
  na_locs <- which(!complete.cases(tl$Move))
  tl <- tl[-na_locs,]
  
  days <- unique(mday(tl$date_time))
  tl_min <-  data.frame(matrix(NA,
                               ncol = 4,
                               nrow = (length(days)*24)))
  
  colnames(tl_min) <- c("code", "day", "hour", "min")
  
  tl_min$code <- tl_code
  tl_min$day <- rep(days, each = 24)
  tl_min$hour <- rep(0:23, length(days))
  tl_min$min <- 0
  
  # Trim pre-allocated hourly rows to match actual recorded hours 
  tl_min <- tl_min[-c((ceiling(nrow(tl)/36000)+1):nrow(tl_min)), ]
  
  
  for (j in 1:nrow(tl)){
    if (tl[j, "Move"] == "mobile"){
      r <- which(tl_min$day == day(tl[j, "date_time"]) & tl_min$hour == hour(tl[j, "date_time"]))
      tl_min[r, "min"] <- tl_min[r, "min"] + 1 
    }
  }
  min_mov <- rbind(min_mov, tl_min)
}

min_mov$minutes <- round(min_mov$min/600)

ggplot(min_mov, aes(x = hour, y = min, group = hour)) + 
  geom_boxplot(aes(group = hour),
               outlier.size = 2,
               fill = "darkseagreen") +
  stat_boxplot(geom = "errorbar",
               width = 0.5) +  
  scale_x_continuous("Time (h)", breaks = seq(0, 23, 2)) +
  scale_y_continuous("Average minutes of movement", breaks = seq(0,20,5)) +
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_text(vjust = -0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(colour = "black"))

move$total <- as.vector(tapply(min_mov$minutes, min_mov$code, sum))
# write.csv(move, "results/movement_perc.csv")

# write.csv(min_mov, "results/movement.csv")

# Plot minutes of movement per individual ##

move <- read.csv(file = "results/movement.csv", head = T)

move$group <- rep(NA, times=nrow(move))

for (i in 1:nrow(move)){
  if (move[i,1] == 'TLCRP005' | move[i,1] == 'TLCRP006' | move[i,1] == 'TLCRP007' | move[i,1] == 'TLCRP008') {
    move[i, 'group'] <- "May 23-26"
  } else if (move[i,1] == 'TLCRP009' | move[i,1] == 'TLCRP012'){
    move[i, 'group'] <- "May 30-June 2"
  } else if (move[i,1] == 'TLCRP013' | move[i,1] == 'TLCRP014' | move[i,1] == 'TLCRP015' | move[i,1] == 'TLCRP016'){
    move[i, 'group'] <- "June 6-9"
  } else if (move[i,1] == 'TLCRP021' | move[i,1] == 'TLCRP022' | move[i,1] == 'TLCRP023' | move[i,1] == 'TLCRP024'){
    move[i, 'group'] <- "June 20-23"
  } else {
    move[i, 'group'] <- "June 27-30"
  }
}

ggplot(move, aes(x = hour, y = minutes, group = hour)) + 
  geom_boxplot(aes(group = hour),
               outlier.size = 1,
               fill = "darkseagreen") +
  stat_boxplot(geom = "errorbar",
               width = 0.5) +
  facet_wrap(code~group, ncol = 3) +
  scale_x_continuous("Time (h)", breaks = seq(0, 23, 4)) +
  scale_y_continuous("Average minutes of movement", breaks = seq(0,20,5)) +
  theme_classic(base_size = 10) +
  theme(axis.title.x = element_text(vjust = -0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(colour = "black"))


###
min_mov <- read.csv(file = "results/movement_perc.csv", head = TRUE)
t_data <- read.csv(file = "data/Timon_data.csv", head = TRUE)

move$weight <- t_data$W
move$svl <- t_data$SVL


with(move, plot(weight ~ move, pch = 16))
with(move, plot(svl ~ move, pch = 16))

cor.test(move$svl, move$move)

with(move, plot(weight ~ day_move, pch = 16))
with(move, plot(svl ~ day_move, pch = 16))


###################################################################################


### Visualize relationship of VeDBA and Temp/Humidity

library(dplyr)
library(zoo) # running mean
library(scales)

for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("data/Tb_first/TLCRP0", s, ".csv")
  tl <- read.csv(file = path, head = TRUE)
  
  tl$VeDBA <- tl$VeDBA * 9.8 # if VeDBA is not already in m/s2
  
  tl$date_time <- as.POSIXct(
    paste(tl$Date, tl$Time),
    format = "%Y-%m-%d %H:%M:%S.%OS",
    tz = "UTC"
  )
  
  max.val <- 0.500339
  t.maxperf <- tl$TemperatureC[which.max(tl$VeDBA)]
  # h.maxperf <- tl$Humidity[which.max(tl$VeDBA)]
  
  p1 <- ggplot(tl) + 
    geom_point(aes(x=TemperatureC, y=VeDBA, col= as.factor(VeDBA <= max.val)),
               size = 0.4, shape = 16) +
    # geom_vline(xintercept = t.maxperf) +
    # geom_hline(yintercept = max.val) +
    # scale_color_discrete(name = "Group",
    #                     label = c('Mobile', 'Immobile')) +
    theme_classic(base_size = 10) +
    scale_x_continuous("Temperature (°C)")+
    scale_y_continuous(name = expression("VeDBA (m/s"^2*")"))+
    theme(axis.text = element_text(colour = "black"),
          axis.title.y = element_text(vjust = +2),
          legend.position = "none") 
  
  # Determine a scale factor so temperature maps onto the full VeDBA axis range
  scale_factor <- max(tl$VeDBA, na.rm = TRUE) / max(tl$TemperatureC, tl$Tb, na.rm = TRUE)
  
  p2 <- ggplot(tl) + 
    geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= max.val)),
               size = 0.4, shape = 16) +
    geom_line(aes(x=date_time, y = TemperatureC * scale_factor), color = "red", linewidth = 0.3) +
    geom_line(aes(x=date_time, y = Tb * scale_factor), color = "black", linewidth = 0.3) +
    scale_y_continuous("VeDBA (g)", sec.axis = sec_axis(~ . / scale_factor, name = "Temperature (ºC)")) +
    scale_x_datetime("Date") +
    geom_hline(yintercept = max.val) +
    #scale_color_discrete(name = "Group", label = c('Mobile', 'Immobile')) +
    theme_classic(base_size = 10) +
    theme(axis.text = element_text(colour = "black"),
          axis.title.y.left = element_text(vjust = +2),
          axis.title.y.right = element_text(vjust = +2),
          legend.position = "none",
          legend.background = element_blank(),
          # legend.key.size = unit(1.3, "cm"),
          legend.box.background = element_rect(colour = "grey3", fill = "transparent"))
  
  p <- (p1 | p2) +
    plot_layout(widths = c(8, 10)) +
    plot_annotation(tag_levels = "a")
  ggsave(paste0("results/figures/figure2_TLCPR0", s, ".tiff"), plot = p, width = 7 + 10, height = 7, units = "cm", dpi = 600, compression = "lzw")
  
  
  #ggplot(tl) + 
  #  geom_point(aes(x=Humidity, y=VeDBA, col= as.factor(VeDBA <= max.val))) +
  # # geom_vline(xintercept = h.maxperf) +
  #  geom_hline(yintercept = max.val) +
  #  theme_bw() +
  #  scale_x_continuous("Humidity (%)")+
  #  scale_y_continuous("VeDBA (g)")+
  #  theme(text = element_text(size=25), 
  #        axis.text = element_text(colour = "black"),
  #        axis.title.y = element_text(vjust = +2),
  #        legend.position =  "none",
  #        legend.title = element_blank(),
  #        legend.background = element_blank(),
  #        legend.key.size = unit(1.3, "cm"),
  #        legend.box.background = element_rect(colour = "transparent", fill = "transparent"))
  #
  
  
  #ggplot(tl) + 
  #  geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= max.val))) +
  #  geom_line(aes(x=date_time, y = Humidity/16.5), color = "skyblue4") +
  #  # geom_line(aes(x=date_time, y = air_temp/10), color = "blue") +
  #  scale_y_continuous("VeDBA (g)", sec.axis = sec_axis(~ . * 16.5, name = "Humidity (%)")) +
  #  scale_x_datetime("Date") +
  #  geom_hline(yintercept = max.val) +
  #  #scale_color_discrete(name = "Group", label = c('Mobile', 'Immobile')) +
  #  theme_bw() +
  #  theme(text = element_text(size=30), 
  #        axis.text = element_text(colour = "black"),
  #        axis.title.y.left = element_text(vjust = +2),
  #        axis.title.y.right = element_text(vjust = +2),
  #        legend.position = "none",
  #        legend.background = element_blank(),
  #        legend.key.size = unit(1.3, "cm"),
  #        legend.box.background = element_rect(colour = "grey3", fill = "transparent"))
  
}



###################################################################################

## density plots for the Tb of all animals

tl_all <- data.frame(code = character(),
                     group = character(),
                     datetime = as.POSIXct(character()),
                     Tb = integer(), 
                     move = character(), 
                     weight = integer())
  
for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("data/Tb_first/TLCRP0", s, ".csv")
  tl <- read.csv(file = path, head = TRUE)
  tl$code <- paste0("TLCRP0", s)
  if (i == 5 | i == 6 | i == 7 | i == 8) {
    tl$group <- "May 23-26"
  } else if (i == 9 | i == 12){
    tl$group <- "May 30-June 2"
  } else if (i == 13 | i == 14 | i == 15 | i == 16){
    tl$group <- "June 6-9"
  } else if (i == 17 | i == 18){
    tl$group = "June 13-16"
  } else if (i == 21 | i == 22 | i == 23 | i == 24){
    tl$group <- "June 20-23"
  } else {
    tl$group <- "June 27-30"
  }
  tl_liz <- data.frame(code = tl$code, group = tl$group, datetime = tl$date_time, Tb = tl$Tb, move = tl$Move, weight = tl$weight)
  tl_all <- rbind(tl_all, tl_liz)
}


ggplot(tl_all, aes(x = Tb, color = move)) +
  geom_density(linewidth = 0.5) +
  labs(x = "Body temperature (°C)", y = "Density") +
  facet_wrap(code~group, ncol=3) +
  scale_colour_manual(values = c("#00bfc4","#f8766d"),
                      labels = c("Immobile", "Mobile")) +
  theme_classic(base_size = 10) +
  theme(axis.text = element_text(colour = "black"),
       axis.title.y = element_text(vjust = +2),
       axis.title.x = element_text(vjust = -0.5),
       legend.position = "inside",
       
       legend.position.inside = c(0.85, -0.035),
       legend.direction = "horizontal",
       legend.background = element_blank(),
       legend.title = element_blank())


ggsave("results/figures/Tb_density.tiff", width = 15, height = 22, units = "cm", dpi = 600)


### Parameters of density plots ###

tl_density <- data.frame(code = character(),
                         min_pref_move = integer(),
                         pref_move = integer(),
                         max_pref_move = integer(),
                         min_pref_imm = integer(),
                         pref_imm = integer(),
                         max_pref_imm = integer())


for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("data/Tb_first/TLCRP0", s, ".csv")
  tl <- read.csv(file = path, head = TRUE)
  code <- paste0("TLCRP0", s)
  
  mobile <- which(tl$Move == "mobile")
  tl_mobile <- tl[mobile,]
  tl_immobile <- tl[-mobile,]
  
  
  density_move <- density(tl_mobile$Tb)
  density_imm <- density(tl_immobile$Tb)
  
  
  pos_move <- which(density_move$y == max(density_move$y))
  pos_imm <- which(density_imm$y == max(density_imm$y))
  
  ## most common Tb when moving vs. when immobile
  t.max.den.move <- density_move$x[pos_move]
  t.max.den.imm <- density_move$x[pos_imm]
  
  
  common_move <- which(density_move$y > 0.05)
  common_imm <- which(density_imm$y > 0.05)
  
  ## range of Tb where the animal was preferring while moving vs while immobile
  comm_temp_move_min <- min(density_move$x[common_move])
  comm_temp_move_max <- max(density_move$x[common_move])
  
  comm_temp_imm_min <- min(density_imm$x[common_imm])
  comm_temp_imm_max <- max(density_imm$x[common_imm])
  
  
  tl_n <- data.frame(code = code, 
                     min_pref_move = comm_temp_move_min,
                     pref_move = t.max.den.move,
                     max_pref_move = comm_temp_move_max,
                     min_pref_imm = comm_temp_imm_min,
                     pref_imm = t.max.den.imm,
                     max_pref_imm =  comm_temp_imm_max)
  
  tl_density <- rbind(tl_density, tl_n)
}

write.csv(tl_density, "results/density_Tb.csv")



## Split data for day and night ##

night <- which(hour(tl_all$datetime) <= 5 | hour(tl_all$datetime) >= 21)
tl_all_night <- tl_all[night,]
tl_all_day <- tl_all[-night,]

ggplot(tl_all_day, aes(x = Tb, color = move)) +
  geom_density() +
  facet_wrap(code~group, ncol=4)

ggplot(tl_all_night, aes(x = Tb, color = move)) +
  geom_density() +
  facet_wrap(code~group, ncol=4)

# Density 2d with body weight for active animals

active <- which(tl_all$move == "mobile")
tl_all_active <- tl_all[active,]

library(viridis)
ggplot(tl_all_active, aes(x = Tb, y = weight)) +
  stat_density2d(aes(fill = after_stat(density)), contour = F, geom = 'tile') +
  scale_fill_viridis()



## Check for correlations
tl_den <- read.csv(file = "/results/density_Tb.csv", head = TRUE)
t_data <- read.csv(file = "data/Timon_data.csv", head = TRUE)
t_data <- t_data[-c(11,12), ]

tl_den$weight <- t_data$W
tl_den$svl <- t_data$SVL

# 
with(tl_den, plot(max_pref_imm ~ svl, pch = 16, 
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
abline(lm(max_pref_imm ~ svl, data=tl_den), lty=4, lwd = 2)
cor.test(tl_den$svl, tl_den$max_pref_imm)


with(tl_den, plot(max_pref_imm ~ weight, pch = 16, 
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
abline(lm(max_pref_imm ~ weight, data=tl_den), lty=4, lwd = 2)
cor.test(tl_den$weight, tl_den$max_pref_imm)

max_pref_imm_lm <- lm(max_pref_imm ~ weight, data=tl_den)

summary(max_pref_imm_lm)


####################### CORRELATIONS AND HYPOTHESES TESTING ########################


tl_res <- read.csv(file = "results/TPC_params_new.csv", head = TRUE)
tl_res <- subset(tl_res, select = -c(1))
t_data <- read.csv(file = "data/Timon_data.csv", head = TRUE)
t_data <- t_data[-c(11,12), ] # remove TLCRP017 & TLCRP018 that looked weird


sharp <- which(tl_res$model == "sharpeschoolhigh")
tl_mod <- tl_res[sharp,]
tl_mod$sensor <- t_data$sensor
tl_mod$weight <- t_data$W
tl_mod$svl <- t_data$SVL

### Calculate Tb_emerge & Tb_retreat 

Tb_min_max <- data.frame(code = character(),
                         Tb_emerge = integer(),
                         Tb_retreat = integer())

for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("data/Tb_first/TLCRP0", s, ".csv")
  tl <- read.csv(file = path, head = TRUE)
  code <- paste0("TLCRP0", s)
  
  active <- which(tl$Move == "mobile")
  tl_active <- tl[active,]
  
  night <- which(tl_active$Time <= "06:29:00.000" | tl_active$Time >= "20:30:00.000")
  tl_day <- tl_active[-night, ]
  
  Tb_emerge <- min(tl_day$Tb)
  Tb_retreat <- max(tl_day$Tb)
  
  tl_par <- data.frame(code = code, Tb_emerge = Tb_emerge, Tb_retreat = Tb_retreat)
  Tb_min_max <- rbind(Tb_min_max, tl_par)
  
}

tl_mod$Tb_emerge <- Tb_min_max$Tb_emerge
tl_mod$Tb_retreat <- Tb_min_max$Tb_retreat

write.csv(tl_mod, "results/Tb_param_new.csv")


###############################################################################3
# Tretreat - Topt correlation test

tl_mod <- read.csv(file = "results/Tb_param_new.csv", head = TRUE)

tl_mod$sens <- tl_mod$Tb_retreat - tl_mod$topt

cor.test(tl_mod$svl, tl_mod$sens)
lm_svl <- lm(sens ~ svl, data=tl_mod)
summary(lm_svl)

with(tl_mod, plot(sens ~ svl, pch = 16, ylab='Tretreat - Topt', xlab = "SVL", 
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
abline(lm(sens ~ svl, data=tl_mod), lty=4, lwd = 2)

cor.test(tl_mod$weight, tl_mod$sens)
lm_w <- lm(sens ~ weight, data=tl_mod)
summary(lm_w)

with(tl_mod, plot(sens ~ weight, pch = 16, ylab='Tretreat - Topt', xlab = "Body mass", 
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
abline(lm(sens ~ weight, data=tl_mod), lty=4, lwd = 2)

###############################################################################

# Check for correlation between body size (SVL/weight) and Topt/breadth/rmax - NO CORRELATION

par(mfrow = c(3, 2), mar = c(4.5, 5, 2, 1))

# Topt / body mass & SVL
with(tl_mod, plot(topt ~ weight, pch = 16, ylab='Topt (°C)', xlab = "", 
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
cor.test(tl_mod$weight, tl_mod$topt)

with(tl_mod, plot(topt ~ svl, pch = 16, ylab = "", xlab = "",
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
cor.test(tl_mod$svl, tl_mod$topt)

#rmax / body mass & SVL
with(tl_mod, plot(rmax ~ weight, pch = 16, ylab='rmax (g)', xlab = "",
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
cor.test(tl_mod$weight, tl_mod$rmax)

with(tl_mod, plot(rmax ~ svl, pch = 16, ylab = '', xlab = "",
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
cor.test(tl_mod$svl, tl_mod$rmax)

#breadth / body mass & SVL
with(tl_mod, plot(breadth ~ weight, pch = 16, ylab = "Breadth (°C)", xlab = "Body mass (gr)", 
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
cor.test(tl_mod$weight, tl_mod$breadth)

with(tl_mod, plot(breadth ~ svl, pch = 16, ylab = "", xlab = "SVL (mm)",
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
cor.test(tl_mod$svl, tl_mod$breadth)

dev.off()



##############################################################################

#par(mfrow = c(1, 2), mar = c(6, 5, 1, 1))
par(mfrow = c(3, 2), mar = c(4.5, 5, 2, 1))

# Test "hotter is better" hypothesis - NO CORRELATION
with(tl_mod, plot(topt ~ rmax, pch = 16, ylab = "Topt (°C)", xlab = "rmax (g)",
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2),
     box(lwd = 3))
cor.test(log(tl_mod$rmax), log(tl_mod$topt))


################################################################################

# Test "specialist-generalist" hypothesis - (P < 0.001)
with(tl_mod, plot(rmax ~ breadth, pch = 16,  ylab='rmax (g)', xlab = 'Breadth (°C)', 
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
abline(lm(rmax ~ breadth, data=tl_mod), lty=4, lwd = 2)
cor.test(log(tl_mod$rmax), log(tl_mod$breadth))

ggplot(tl_mod, aes(x=breadth, y=rmax)) +
  geom_point(size = 3) +    # Use hollow circles
  geom_smooth(method=lm, color = "black") +
  scale_y_continuous("Maximum performance (m/s2)") +
  scale_x_continuous("Thermal breadth (°C )") +
  theme_classic(base_size = 10) +
  theme(axis.text = element_text(colour = "black"),
        axis.title.y.left = element_text(vjust = +2),
        axis.title.y.right = element_text(vjust = +2),
        legend.position = "none",
        legend.background = element_blank(),
        legend.key.size = unit(1.3, "cm"),
        legend.box.background = element_rect(colour = "grey3", fill = "transparent"))

lm_sp_gen <- lm(rmax ~ breadth, data=tl_mod)
summary(lm_sp_gen)

################################################################################
## Emerge and retreat temperatures?

tl_mod$Tb_emerge <- Tb_min_max$Tb_emerge
tl_mod$Tb_retreat <- Tb_min_max$Tb_retreat

with(tl_mod, plot(Tb_emerge ~ weight, pch = 16))
cor.test(tl_mod$weight, tl_mod$Tb_emerge)

with(tl_mod, plot(Tb_retreat ~ weight, pch = 16))
cor.test(tl_mod$weight, tl_mod$Tb_retreat)

with(tl_mod, plot(Tb_emerge ~ svl, pch = 16))
cor.test(tl_mod$svl, tl_mod$Tb_emerge)

with(tl_mod, plot(Tb_retreat ~ svl, pch = 16))
cor.test(tl_mod$svl, tl_mod$Tb_retreat)


