setwd("C:\\Users\\Katerina\\Desktop\\mesocosms\\obs")

library(dplyr)
library(zoo) # running mean
library(ggplot2)
library(scales)
library(stringr)
library(cluster)

options(scipen=999)

# rm(list=ls()) # removes all variables from environment

source("clean_sensor.R")

for (i in c(7, 8, 9, 12, 13, 14, 15, 16, 18, 21, 22, 23, 24, 25)){
  tl <- clean_sensor(n_sensor = i)
  tl$Move <- factor(ifelse(tl$VeDBA <= 0.09272727, "immobile", "mobile"))
  tl <- subset(tl, select = -c(15:20, 23:28))
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("C:\\Users\\Katerina\\Desktop\\mesocosms\\final\\TLCRP0", s, ".csv")
  write.csv(tl, path)
}


# ### Average of metrics, e.g., VeDBA, every 1 minute (600 rows)
# 
# i <- 1
# k <- 1
# rows <- nrow(tl)
# VeDBA_means <- c()
# while(i <= rows) {
#   j <- i + 599       ## to calculate the mean every minute (every 600 rows)
#   VeDBA_means [k] <- colMeans(as.data.frame(tl[i:j, "VeDBA"]), na.rm = T)
#   k <- k + 1 
#   for (m in 1:598) {
#     VeDBA_means [k] <- NA
#     k <- k + 1
#   }
#   i <- i + 600
#   k <- k + 1
# }
# 
# VeDBA_means <- VeDBA_means[-c((rows +1):length(VeDBA_means))] # to make the vector (VeDBA) the same length as the dataframe
# tl1 <- cbind(tl, VeDBA_means)
# 
# 

# ###########
# 
# 
# # Select day
# # tl_1 <- tl[tl$Date == '2022-06-06', ]
# # tl13 <- tl
# # tl <- tl_1
# 
# ## Selecting rows in intervals; here every 1 minute (600 rows) ##
# 
# tl_new <- tl[0,]
# i <- 1
# while(i <= rows) {
#   tl_new <- rbind(tl_new, tl[i,])
#   i <- i + 600
# }
# 
# #### 

# # Plotting the acceleration metric (like VeDBA) with time. Note the difference between VeDBA and VeDBA_means ##
# 
# p <- ggplot(tl) +
#   geom_line(aes(x=date_time, y=VeDBA)) +
#   #geom_point(aes(x=date_time, y=VeDBA)) +
#   scale_x_datetime(labels = date_format("%H:%M"))
# p



########### Using clara method to get threshold ################

# library(factoextra)

tl_c <- subset(tl, select = c('Time','date_time', 'DBAx','DBAy','DBAz','SDdbax','SDdbay','SDdbaz','VeDBA','ODBA', 'MAXdbax'))
str(tl_c)

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

# # Nautical Twilight for the 6th of June
# night <- which(tl_c$Time <= "05:29:00.000" | tl_c$Time >= "21:37:00.000")
# tl_c_night <- tl_c[night,]
# tl_c_day <- tl_c[-night,]
# head(tl_c_day)
# 
# # 1st step is data scaling.
# #tl_c <- scale(tl_c)
# tl_c_day_scaled <- scale(tl_c_day[,c('DBAx','DBAy','VeDBA','ODBA','SDdbax','SDdbay','SDdbaz')])
# tl_c_night_scaled <- scale(tl_c_night[,c('DBAx','DBAy','VeDBA','ODBA','SDdbax','SDdbay','SDdbaz')])
# head(tl_c_day_scaled)

# 1st step is data scaling.
tl_c_scaled <- scale(tl_c[,c('DBAx','SDdbax', 'MAXdbax')])

## clara classification

# clus_day <- clara(tl_c_day_scaled,
#                   k = 3,
#                   stand = T,
#                   samples = 50,
#                   metric = "manhattan",
#                   pamLike = T)
# 
# clus_night <- clara(tl_c_night_scaled,
#               k = 3,
#               stand = T,
#               samples = 50,
#               metric = "manhattan",
#               pamLike = T)

clus <- clara(tl_c_scaled,
                  k = 2,
                  stand = T,
                  samples = 100,
                  metric = "manhattan",
                  pamLike = T)

#plot(clus_night)
#plot(clus_day)

# cl_clusters <- clus$cluster
# fviz_cluster(list(data=tl_c, cluster = cl_clusters))


tl_c$clustering <- as.factor(clus$clustering)


# Nautical Twilight for the 6th of June
# night <- which(tl_c$Time <= "05:29:00.000" | tl_c$Time >= "21:37:00.000")

# tl_c_night <- tl_c[night,]
# tl_c_day <- tl_c[-night,]
# head(tl_c_day)


#summary(glm(temp ~ clustering, data = tl_c))
#summary(glm(lux_max ~ clustering, data = tl_c))

# Acceleration during the logging period
# ggplot(tl_c) +
#   geom_point(aes(x=date_time, y=VeDBA, color=as.factor(clustering))) +
#   scale_x_datetime(labels = date_format("%H:%M")) 
# during the day
# ggplot(tl_c_day) +
#   geom_point(aes(x=date_time, y=VeDBA, color=as.factor(clustering))) +
#   scale_x_datetime(labels = date_format("%H:%M")) 
# during night
# ggplot(tl_c_night) +
#   geom_point(aes(x=date_time, y=VeDBA, color=as.factor(clustering))) +
#   scale_x_datetime(labels = date_format("%H:%M")) 



# Acceleration as a function of surrounding air temperature
# ggplot(tl_c[1:1000000,]) + 
#   geom_point(aes(x=temp, y=VeDBA, col=clustering)) +
#   theme_classic()
# during the day
# ggplot(tl_c_day) + 
#   geom_point(aes(x=temp, y=VeDBA, col=clustering)) +
 #  theme_classic()
# during night
# ggplot(tl_c_night) + 
#   geom_point(aes(x=temp, y=VeDBA, col=clustering)) +
#   theme_classic()


# ggplot(tl_c[1:1000000,]) + 
#   geom_point(aes(x=lux_max, y=VeDBA, col=clustering)) +
#   theme_classic()


######### 

#with(tl_c, tapply(VeDBA, clustering, mean))
thres <- sort(with(tl_c, tapply(VeDBA, clustering, max)))
thres
sum(tl_c$VeDBA <= thres[1])


### 

tl_c$Move <- factor(ifelse(tl_c$VeDBA <= thres[1], "immobile", "mobile"))


#max.val <- with(tl_c, tapply(VeDBA, clustering, max))[2]
# max.val <- min(with(tl_c, tapply(VeDBA, clustering, max)))
t.maxperf <- tl_c$temp[which.max(tl_c$VeDBA)]


ggplot(tl_c[1:1000000,]) + 
  geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= thres[1]))) +
  geom_line(aes(x=date_time, y = temp/10), color = "red") +
  # geom_line(aes(x=date_time, y = air_temp/10), color = "blue") +
  scale_y_continuous("VeDBA", sec.axis = sec_axis(~ . * 10, name = "Temperature")) +
  geom_hline(yintercept = thres[1]) +
  scale_color_discrete(name = "Group",
                      label = c('mobile', 'immobile')) +
  theme_classic()


## thermal curve (do again with body temperature)

ggplot(tl_c) + 
  geom_line(aes(x=temp, y=VeDBA)) +
  scale_y_continuous("VeDBA") +
  scale_x_continuous(limits = c(10, 40)) +
  theme_classic()


tl$VeDBA_mean <- NA
for(i in 1:nrow(tl)) {
  tl[i, 'VeDBA_mean'] <- mean(tl$VeDBA[tl$TemperatureC == tl[i, 'TemperatureC']], na.rm = T)
}



