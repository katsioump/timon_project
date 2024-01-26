library(ggplot2)
library(stringr)
library(lubridate)

tl_all <- data.frame(code = character(),
                     group = character(),
                     datetime = as.POSIXct(character()),
                     Tb = integer(), 
                     move = character(), 
                     weight = integer())
  
for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("C:\\Users\\Katerina\\Desktop\\mesocosms\\Tb_first\\TLCRP0", s, ".csv")
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
  geom_density() +
  labs(x = "Body temperature (°C)", y = "Density") +
  facet_wrap(code~group, ncol=3) +
  scale_colour_manual(values = c("#00bfc4","#f8766d"),
                      labels = c("Immobile", "Mobile")) +
  theme(text = element_text(size=15), 
       axis.text = element_text(colour = "black"),
       axis.title.y = element_text(vjust = +2),
       axis.title.x = element_text(vjust = -0.5),
       legend.position = c(0.85, -0.048),
       legend.direction = "horizontal",
       legend.background = element_blank(),
       legend.title = element_blank())




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
  path <- paste0("C:\\Users\\Katerina\\Desktop\\mesocosms\\Tb_first\\TLCRP0", s, ".csv")
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

write.csv(tl_density, "C:\\Users\\Katerina\\Documents\\Master thesis\\results\\density_Tb.csv")



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
tl_den <- read.csv(file = "C:\\Users\\Katerina\\Documents\\Master thesis\\results\\density_Tb.csv", head = TRUE)
t_data <- read.csv(file = "C:/Users/Katerina/Documents/Master thesis/Timon_data.csv", head = TRUE)
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
