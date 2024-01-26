library(ggplot2)
library(stringr)
library(lubridate)


### Calculate total percentage of movement for every lizard 

move <- data.frame(code=character(),
                   move=integer())

for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("C:\\Users\\Katerina\\Desktop\\mesocosms\\final\\TLCRP0", s, ".csv")
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

# night <- which(tl$Time < "06:30:00.000" | tl$Time >= "20:30:00.000")
# tl_day <- tl[-night,]
# tl_night <- tl[night, ]


#### Visualize average movement per hour (mins) for all animals

min_mov <- data.frame(code=character(),
                      day=integer(),
                      hour=integer(),
                      min=integer())


for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("C:\\Users\\Katerina\\Desktop\\mesocosms\\final\\TLCRP0", s, ".csv")
  tl <- read.csv(file = path, head = TRUE)
  tl_code <- paste0("TLCRP0", s)
  
  na_locs <- which(!complete.cases(tl$Move))
  tl <- tl[-na_locs,]
  
  tl$date_time <- strptime(tl$date_time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  tl$date_time <- as.POSIXct(tl$date_time)
  
  days <- unique(mday(tl$date_time))
  tl_min <-  data.frame(matrix(NA,
                               ncol = 4,
                               nrow = (length(days)*24)))
  
  colnames(tl_min) <- c("code", "day", "hour", "min")
  
  tl_min$code <- tl_code
  tl_min$day <- rep(days, each = 24)
  tl_min$hour <- rep(0:23, length(days))
  tl_min$min <- 0
  
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

ggplot(min_mov, aes(x = hour, y = minutes, group = hour)) + 
  geom_boxplot(aes(group = hour),
               outlier.size = 2,
               fill = "darkseagreen") +
  stat_boxplot(geom = "errorbar",
               width = 0.5) +  
  scale_x_continuous("Time (h)", breaks = seq(0, 23, 2)) +
  scale_y_continuous("Average minutes of movement", breaks = seq(0,20,5)) +
  theme_bw(base_size = 12) +
  theme(text = element_text(size=30), 
        axis.title.x = element_text(vjust = -0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(colour = "black"))

move$total <- as.vector(tapply(min_mov$minutes, min_mov$code, sum))

# write.csv(min_mov, "C:\\Users\\Katerina\\Documents\\Master thesis\\results\\movement.csv")

# Plot minutes of movement per individual ##

move <- read.csv(file = "C:\\Users\\Katerina\\Documents\\Master thesis\\results\\movement.csv", head = T)
move <- move[-(which(move$code == "TLCRP017" | move$code == "TLCRP018")),]

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
  theme_bw(base_size = 10) +
  theme(text = element_text(size=17), 
        axis.title.x = element_text(vjust = -0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(colour = "black"))


###
min_mov <- read.csv(file = "C:\\Users\\Katerina\\Documents\\Master thesis\\results\\movement.csv", head = TRUE)
t_data <- read.csv(file = "C:/Users/Katerina/Documents/Master thesis/Timon_data.csv", head = TRUE)
t_data <- t_data[-c(11,12),]

move$weight <- t_data$W
move$svl <- t_data$SVL


with(move, plot(weight ~ move, pch = 16))
with(move, plot(svl ~ move, pch = 16))

cor.test(move$svl, move$move)

with(move, plot(weight ~ day_move, pch = 16))
with(move, plot(svl ~ day_move, pch = 16))



### Visualize relationship of VeDBA and Temp/Humidity

library(dplyr)
library(zoo) # running mean
library(scales)

for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("C:\\Users\\Katerina\\Desktop\\mesocosms\\Tb_first\\TLCRP0", s, ".csv")
  tl <- read.csv(file = path, head = TRUE)
  tl$date_time <- strptime(tl$date_time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  tl$date_time <- as.POSIXct(tl$date_time)
  
  max.val <- 0.051055
  t.maxperf <- tl$TemperatureC[which.max(tl$VeDBA)]
  h.maxperf <- tl$Humidity[which.max(tl$VeDBA)]
  
  ggplot(tl) + 
    geom_point(aes(x=TemperatureC, y=VeDBA, col= as.factor(VeDBA <= max.val))) +
    # geom_vline(xintercept = t.maxperf) +
    geom_hline(yintercept = max.val) +
    # scale_color_discrete(name = "Group",
    #                     label = c('Mobile', 'Immobile')) +
    theme_bw() +
    scale_x_continuous("Temperature (°C)")+
    scale_y_continuous("VeDBA (g)")+
    theme(text = element_text(size=30), 
          axis.text = element_text(colour = "black"),
          axis.title.y = element_text(vjust = +2),
          legend.position = "none") # c(.2, .9)
  #legend.title = element_blank(),
  #legend.background = element_blank(),
  #legend.key.size = unit(1.3, "cm"),
  #legend.box.background = element_rect(colour = "transparent", fill = "transparent"))
  ggsave(paste0("C:/Users/Katerina/Documents/Master thesis/GRAPHS/temp_hum/TLCRP0", s, "_temp1.png"), width = 8, height = 7)
  
  ggplot(tl) + 
    geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= max.val))) +
    geom_line(aes(x=date_time, y = TemperatureC/10), color = "red") +
    # geom_line(aes(x=date_time, y = air_temp/10), color = "blue") +
    scale_y_continuous("VeDBA (g)", sec.axis = sec_axis(~ . * 10, name = "Temperature (ºC)")) +
    scale_x_datetime("Date") +
    geom_hline(yintercept = max.val) +
    #scale_color_discrete(name = "Group", label = c('Mobile', 'Immobile')) +
    theme_bw() +
    theme(text = element_text(size=25), 
          axis.text = element_text(colour = "black"),
          axis.title.y.left = element_text(vjust = +2),
          axis.title.y.right = element_text(vjust = +2),
          legend.position = "none",
          legend.background = element_blank(),
          legend.key.size = unit(1.3, "cm"),
          legend.box.background = element_rect(colour = "grey3", fill = "transparent"))
  
  ggsave(paste0("C:/Users/Katerina/Documents/Master thesis/GRAPHS/temp_hum/TLCRP0", s, "_temp2.png"), width = 10, height = 7)
  
  
  ggplot(tl) + 
    geom_point(aes(x=Humidity, y=VeDBA, col= as.factor(VeDBA <= max.val))) +
    # geom_vline(xintercept = h.maxperf) +
    geom_hline(yintercept = max.val) +
    theme_bw() +
    scale_x_continuous("Humidity (%)")+
    scale_y_continuous("VeDBA (g)")+
    theme(text = element_text(size=25), 
          axis.text = element_text(colour = "black"),
          axis.title.y = element_text(vjust = +2),
          legend.position =  "none",
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.key.size = unit(1.3, "cm"),
          legend.box.background = element_rect(colour = "transparent", fill = "transparent"))
  
  ggsave(paste0("C:/Users/Katerina/Documents/Master thesis/GRAPHS/temp_hum/TLCRP0", s, "_hum1.png"), width = 8, height = 7)
  
  
  
  ggplot(tl) + 
    geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= max.val))) +
    geom_line(aes(x=date_time, y = Humidity/16.5), color = "skyblue4") +
    # geom_line(aes(x=date_time, y = air_temp/10), color = "blue") +
    scale_y_continuous("VeDBA (g)", sec.axis = sec_axis(~ . * 16.5, name = "Humidity (%)")) +
    scale_x_datetime("Date") +
    geom_hline(yintercept = max.val) +
    #scale_color_discrete(name = "Group", label = c('Mobile', 'Immobile')) +
    theme_bw() +
    theme(text = element_text(size=30), 
          axis.text = element_text(colour = "black"),
          axis.title.y.left = element_text(vjust = +2),
          axis.title.y.right = element_text(vjust = +2),
          legend.position = "none",
          legend.background = element_blank(),
          legend.key.size = unit(1.3, "cm"),
          legend.box.background = element_rect(colour = "grey3", fill = "transparent"))
  
  ggsave(paste0("C:/Users/Katerina/Documents/Master thesis/GRAPHS/temp_hum/TLCRP0", s, "_hum2.png"), width = 10, height = 7)
  
}
