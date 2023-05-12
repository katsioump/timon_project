library(ggplot2)
library(stringr)
library(lubridate)

datetime <- vector()
Tb <- vector()
code <- vector()
group <- vector()
move <- vector()
weight <- vector()
tl_all <- data.frame(code, group, datetime, Tb, move, weight)
  
for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 17, 18, 21, 22, 23, 24, 25)){
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
  facet_wrap(code~group, ncol=4)

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

# Density 2d with body weight

library(viridis)
ggplot(tl_all_night, aes(x = Tb, y = weight)) +
  stat_density2d(aes(fill = after_stat(density)), contour = F, geom = 'tile') +
  scale_fill_viridis()

