setwd("C:\\Users\\Katerina\\Desktop\\mesocosms\\obs")

library(dplyr)
library(zoo) # running mean
library(ggplot2)
library(scales)
library(stringr)
library(cluster)
library(lubridate)

options(scipen=999)

# rm(list=ls()) # removes all variables from environment

########### Using clara method to get threshold ################

source("clean_sensor.R") 

for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  tl <- clean_sensor(n_sensor = i)
  tl_code <- paste0("TLCRP0", s)
  assign(tl_code, tl)
}

tl <- TLCRP025

tl_c <- subset(tl, select = c('Time','date_time','DBAx','DBAy','DBAz','SDdbax','SDdbay','SDdbaz','VeDBA','ODBA', 'MAXdbax'))

na_locs <- which(!complete.cases(tl_c))
tl_c <- tl_c[-na_locs,]

tl_c$temp <- na.approx(tl$TemperatureC[-na_locs])
tl_c$air_temp <- na.approx(tl$air_temp[-na_locs])
tl_c$hum <- na.approx(tl$Humidity[-na_locs])
tl_c$lux <- na.approx(tl$lux[-na_locs])
tl_c$lux_max <- na.approx(tl$lux_max[-na_locs])
tl_c$rel_light <- na.approx(tl$rel_light[-na_locs])
tl_c$VeDBA_cont <- na.approx(tl$VeDBA[-na_locs])
str(tl_c)

night <- which(tl_c$Time <= "06:29:00.000" | tl_c$Time >= "20:30:00.000")
tl_night <- tl_c[night, ]
tl_c <- tl_night

# 1st step is data scaling.
tl_c_scaled <- scale(tl_c[,c('DBAx','SDdbax', 'MAXdbax')])


clus <- clara(tl_c_scaled,
                  k = 2,
                  stand = T,
                  samples = 100,
                  metric = "manhattan",
                  pamLike = T)


tl_c$clustering <- as.factor(clus$clustering)

######### 

#with(tl_c, tapply(VeDBA, clustering, mean))
thres <- sort(with(tl_c, tapply(VeDBA, clustering, max)))
thres

### 

tl_c$Move <- factor(ifelse(tl_c$VeDBA <= thres[1], "immobile", "mobile"))

### !!! ###
#thres <- 0.054233
thres <- thres[1]

ggplot(tl_c) + 
  geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= thres))) +
  geom_line(aes(x=date_time, y = temp/10), color = "red") +
  # geom_line(aes(x=date_time, y = air_temp/10), color = "blue") +
  scale_y_continuous("VeDBA (g)", sec.axis = sec_axis(~ . * 10, name = "Temperature (ºC)")) +
  scale_x_datetime("Date") +
  geom_hline(yintercept = thres) +
  scale_color_discrete(name = "Group", label = c('Mobile', 'Immobile')) +
  theme_classic() +
  theme(text = element_text(size=30), 
        axis.text = element_text(colour = "black"),
        axis.title.y.left = element_text(vjust = +2),
        axis.title.y.right = element_text(vjust = +2),
        legend.position = c(.1, .9),
        legend.background = element_blank(),
        legend.key.size = unit(1.3, "cm"),
        legend.box.background = element_rect(colour = "grey3", fill = "transparent"))

element_rect(fill = "transparent")

############# After deciding on the threshold ####################

# e.g. # thres <- 0.051055

for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  tl <- clean_sensor(n_sensor = i)
  tl$Move <- factor(ifelse(tl$VeDBA <= thres, "immobile", "mobile"))
  tl <- subset(tl, select = -c(15:20, 23:28))
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("C:\\Users\\Katerina\\Desktop\\mesocosms\\final\\TLCRP0", s, ".csv")
  write.csv(tl, path)
}




