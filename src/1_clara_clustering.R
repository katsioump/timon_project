library(dplyr)
library(zoo) # running mean
library(ggplot2)
library(patchwork)
library(scales)
library(stringr)
library(cluster)
library(lubridate)

options(scipen=999)

rm(list=ls())

########### Using CLARA method to get threshold ################

source("src/functions/acc_metrics.R")

thresholds <- data.frame(code = character(),
                         strategy = character(), 
                         threshold = integer())


for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  tl <- acc_metrics(n_sensor = i)
  tl_code <- paste0("TLCRP0", s)
  assign(tl_code, tl)
  
  tl_c <- subset(tl, select = c('Time','date_time','DBAx','DBAy','DBAz','SDdbax','SDdbay','SDdbaz','VeDBA','ODBA', 'MAXdbax'))
  
  na_locs <- which(!complete.cases(tl_c))
  tl_c <- tl_c[-na_locs,]
  
  tl_c$temp <- na.approx(tl$TemperatureC[-na_locs])
  tl_c$air_temp <- na.approx(tl$air_temp[-na_locs])
  tl_c$hum <- na.approx(tl$Humidity[-na_locs])
  tl_c$lux <- na.approx(tl$lux[-na_locs])
  tl_c$rel_light <- na.approx(tl$rel_light[-na_locs])
  tl_c$VeDBA_cont <- na.approx(tl$VeDBA[-na_locs])
  str(tl_c)
  
  #### 1st strategy: all metrics and the full animal dataset #### 
  
  # 1st step is data scaling.
  tl_c_scaled <- scale(tl_c[,c('DBAx','DBAy','DBAz','SDdbax','SDdbay','SDdbaz','VeDBA','ODBA', 'MAXdbax')])
  
  
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
  
  p1 <- ggplot(tl_c) + 
    geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= thres[1])),
               size = 0.4, shape = 16) +
    scale_y_continuous(name = expression("VeDBA (m/s"^2*")")) +
    scale_x_datetime("Date") +
    geom_hline(yintercept = thres[1], linewidth = 0.3, linetype = "dashed") +
    theme_classic(base_size = 10) +
    labs(title = paste0('Strategy 1'))+
    theme(axis.text = element_text(colour = "black"),
          axis.title.y.left = element_text(vjust = +2),
          axis.title.y.right = element_text(vjust = +2),
          legend.position = "none")
  
  
  thresholds_temp <- data.frame(code = tl_code, strategy = 1, threshold = thres[1])
  thresholds <- rbind(thresholds, thresholds_temp)
  
  #### 2nd strategy: only x-axis related metrics and full animal dataset ####
  
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
  
  p2 <- ggplot(tl_c) + 
    geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= thres[1])),
               size = 0.4, shape = 16) +
    scale_y_continuous(name = expression("VeDBA (m/s"^2*")")) +
    scale_x_datetime("Date") +
    geom_hline(yintercept = thres[1], linewidth = 0.3, linetype = "dashed") +
    theme_classic(base_size = 10) +
    labs(title = paste0('Strategy 2'))+
    theme(axis.text = element_text(colour = "black"),
          axis.title.y.left = element_text(vjust = +2),
          axis.title.y.right = element_text(vjust = +2),
          legend.position = "none")
  
  
  thresholds_temp <- data.frame(code = tl_code, strategy = 2, threshold = thres[1])
  thresholds <- rbind(thresholds, thresholds_temp)
  
  
  #### 3rd strategy: all acceleration metrics and only nighttime data (mostly immobile states) ####
  
  night <- which(tl_c$Time <= "06:29:00.000" | tl_c$Time >= "20:30:00.000")
  tl_night <- tl_c[night, ]
  
  tl_c_scaled <- scale(tl_night[,c('DBAx','DBAy','DBAz','SDdbax','SDdbay','SDdbaz','VeDBA','ODBA', 'MAXdbax')])
  
  
  clus <- clara(tl_c_scaled,
                k = 2,
                stand = T,
                samples = 100,
                metric = "manhattan",
                pamLike = T)
  
  
  tl_night$clustering <- as.factor(clus$clustering)
  
  ######### 
  
  #with(tl_c, tapply(VeDBA, clustering, mean))
  thres <- sort(with(tl_night, tapply(VeDBA, clustering, max)))
  thres
  
  p3 <- ggplot(tl_c) + 
    geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= thres[1])),
               size = 0.4, shape = 16) +
    scale_y_continuous(name = expression("VeDBA (m/s"^2*")")) +
    scale_x_datetime("Date") +
    geom_hline(yintercept = thres[1], linewidth = 0.3, linetype = "dashed") +
    theme_classic(base_size = 10) +
    labs(title = paste0('Strategy 3'))+
    theme(axis.text = element_text(colour = "black"),
          axis.title.y.left = element_text(vjust = +2),
          axis.title.y.right = element_text(vjust = +2),
          legend.position = "none")
  
  
  thresholds_temp <- data.frame(code = tl_code, strategy = 3, threshold = thres[1])
  thresholds <- rbind(thresholds, thresholds_temp)
  
  
  #### 4th strategy: only x-axis related metrics and and only nighttime data ####
  
  tl_c_scaled <- scale(tl_night[,c('DBAx','SDdbax', 'MAXdbax')])
  
  
  clus <- clara(tl_c_scaled,
                k = 2,
                stand = T,
                samples = 100,
                metric = "manhattan",
                pamLike = T)
  
  
  tl_night$clustering <- as.factor(clus$clustering)
  
  ######### 
  
  #with(tl_c, tapply(VeDBA, clustering, mean))
  thres <- sort(with(tl_night, tapply(VeDBA, clustering, max)))
  thres
  
  p4 <- ggplot(tl_c) + 
    geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= thres[1])),
               size = 0.4, shape = 16) +
    scale_y_continuous(name = expression("VeDBA (m/s"^2*")")) +
    scale_x_datetime("Date") +
    geom_hline(yintercept = thres[1], linewidth = 0.3, linetype = "dashed") +
    theme_classic(base_size = 10) +
    labs(title = paste0('Strategy 4'))+
    theme(axis.text = element_text(colour = "black"),
          axis.title.y.left = element_text(vjust = +2),
          axis.title.y.right = element_text(vjust = +2),
          legend.position = "none")
  
  
  thresholds_temp <- data.frame(code = tl_code, strategy = 4, threshold = thres[1])
  thresholds <- rbind(thresholds, thresholds_temp)
  
  p <- (p1 | p2) / (p3 | p4) +
    plot_annotation(tag_levels = "a")
  ggsave(paste0("results/figures/cluster_TLCPR0", s, ".tiff"), plot = p, width = 2 * 8, height = 2 * 7, units = "cm", dpi = 600, compression = "lzw")
  
}


### 

thres_summary <- thresholds %>%
  group_by(strategy) %>%
  summarise(
    mean_threshold   = mean(threshold, na.rm = TRUE),
    median_threshold = median(threshold, na.rm = TRUE),
    sd_threshold      = sd(threshold, na.rm = TRUE)
  )

############# After deciding on the threshold ####################

thres <- 0.500339

for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  tl <- acc_metrics(n_sensor = i)
  tl <- subset(tl, select = -c(DBAx, DBAy, DBAz, SDdbax, SDdbay, SDdbaz, ODBA, MAXdbax, MAXdbay, MAXdbaz, STx, STy, STz))
  tl$Move <- factor(ifelse(tl$VeDBA <= thres, "immobile", "mobile"))
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("data/final/TLCRP0", s, ".csv")
  write.csv(tl, path)
}


