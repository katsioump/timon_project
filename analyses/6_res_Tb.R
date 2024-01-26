library(stringr)

tl_res <- read.csv(file = "C:/Users/Katerina/Documents/Master thesis/results/TPC_params_new.csv", head = TRUE)
tl_res <- subset(tl_res, select = -c(1))
t_data <- read.csv(file = "C:/Users/Katerina/Documents/Master thesis/Timon_data.csv", head = TRUE)
t_data <- t_data[-c(11,12), ] # remove TLCRP017 & TLCRP018 that looked weird


sharp <- which(tl_res$model == "sharpeschoolhigh")
tl_mod <- tl_res[sharp,]
tl_mod$sensor <- t_data$sensor
tl_mod$weight <- t_data$W
tl_mod$svl <- t_data$SVL

### Tb_emerge & Tb_retreat

Tb_min_max <- data.frame(code = character(),
                         Tb_emerge = integer(),
                         Tb_retreat = integer())

for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("C:\\Users\\Katerina\\Desktop\\mesocosms\\Tb_first\\TLCRP0", s, ".csv")
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

write.csv(tl_mod, "C:/Users/Katerina/Documents/Master thesis/results/Tb_param_new.csv")
 

tl_mod <- read.csv(file = "C:/Users/Katerina/Documents/Master thesis/results/Tb_param_new.csv", head = TRUE)

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

# Check for correlation between SVL and performance - no cor.

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


#par(mfrow = c(1, 2), mar = c(6, 5, 1, 1))
par(mfrow = c(3, 2), mar = c(4.5, 5, 2, 1))

# Test "hotter is better" hypothesis - rejected
with(tl_mod, plot(topt ~ rmax, pch = 16, ylab = "Topt (°C)", xlab = "rmax (g)",
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2),
                  box(lwd = 3))
cor.test(tl_mod$rmax, tl_mod$topt)

# Test "specialist-generalist" hypothesis - retained (p < 0.001)
with(tl_mod, plot(rmax ~ breadth, pch = 16,  ylab='rmax (g)', xlab = 'Breadth (°C)', 
                  cex.lab = 2,
                  cex.axis = 2,
                  cex.main = 2,
                  cex.sub = 2))
abline(lm(rmax ~ breadth, data=tl_mod), lty=4, lwd = 2)
cor.test(tl_mod$rmax, tl_mod$breadth)

lm_sp_gen <- lm(rmax ~ breadth, data=tl_mod)
summary(lm_sp_gen)

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

