tl_res <- read.csv(file = "C:/Users/Katerina/Documents/Master thesis/results/TPC_params_VeDBA.csv", head = TRUE)
tl_res <- subset(tl_res, select = -c(1))
t_data <- read.csv(file = "C:/Users/Katerina/Documents/Master thesis/Timon_data.csv", head = TRUE)


sharp <- which(tl_res$model == "sharpeschoolhigh")
tl_mod <- tl_res[sharp,]
tl_mod$sensor <- t_data$sensor
tl_mod$weight <- t_data$W
tl_mod$svl <- t_data$SVL

tl_mod <- tl_mod[-c(11,12), ] # remove TLCRP017 & TLCRP018 that looked weird

# Get Tmin,foraging and Tmax,foraging

tl_mod$Tfor_min <- tl_mod$topt - (tl_mod$breadth/2)
tl_mod$Tfor_max <- tl_mod$topt + (tl_mod$breadth/2)


# Check for correlation between weight and performance - no cor.
with(tl_mod, plot(topt ~ weight, pch = 16))
cor.test(tl_mod$weight, tl_mod$topt)

with(tl_mod, plot(rmax ~ weight, pch = 16))
cor.test(tl_mod$weight, tl_mod$rmax)

# Check for sensor bias - no
cor.test(tl_mod$sensor, tl_mod$topt)

# Test "hotter is better" hypothesis - rejected
with(tl_mod, plot(topt ~ rmax, pch = 16))
cor.test(tl_mod$rmax, tl_mod$topt)

# Test "specialist-generalist" hypothesis - retained (p < 0.001)
with(tl_mod, plot(rmax ~ breadth, pch = 16))
cor.test(tl_mod$rmax, tl_mod$breadth)


### Tb_emerge & Tb_retreat

Tb_min_max <- data.frame(code = character(),
                         Tb_emerge = integer(),
                         Tb_retreat = integer())

for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 17, 18, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("C:\\Users\\Katerina\\Desktop\\mesocosms\\Tb_first\\TLCRP0", s, ".csv")
  tl <- read.csv(file = path, head = TRUE)
  code <- paste0("TLCRP0", s)
  
  active <- which(tl$Move == "mobile")
  tl_active <- tl[active,]
  
  Tb_emerge <- min(tl_active$Tb)
  Tb_retreat <- max(tl_active$Tb)
  
  tl_par <- data.frame(code = code, Tb_emerge = Tb_emerge, Tb_retreat = Tb_retreat)
  Tb_min_max <- rbind(Tb_min_max, tl_par)
  
}

Tb_min_max <- Tb_min_max[-c(11,12), ] # remove TLCRP017 & TLCRP018 

tl_mod$Tb_emerge <- Tb_min_max$Tb_emerge
tl_mod$Tb_retreat <- Tb_min_max$Tb_retreat

with(tl_mod, plot(Tb_emerge ~ weight, pch = 16))
cor.test(tl_mod$weight, tl_mod$Tb_emerge)

with(tl_mod, plot(Tb_retreat ~ weight, pch = 16))
cor.test(tl_mod$weight, tl_mod$Tb_retreat)


# Comparison between the TPC parameters from VeDBA and force - Is one of them size dependent?
tl_res_f <- read.csv(file = "C:/Users/Katerina/Documents/Master thesis/results/TPC_params_force.csv", head = TRUE)
tl_res_f <- subset(tl_res_f, select = -c(1))

tl_mod_f <- tl_res_f[sharp,]
tl_mod_f <- tl_mod_f[-c(11,12), ]

tl_mod$topt_f <- tl_mod_f$topt
tl_mod$rmax_f <- tl_mod_f$rmax
tl_mod$breadth_f <- tl_mod_f$breadth

# Topt and weight are not correlated also here. rmax is (p=0.01) but it makes sense since it contains weight.
with(tl_mod, plot(topt_f ~ weight, pch = 16))
cor.test(tl_mod$weight, tl_mod$topt_f)

with(tl_mod, plot(rmax_f ~ weight, pch = 16))
cor.test(tl_mod$weight, tl_mod$rmax_f)

# Topt from both TPCs are the same (r = 0.99) - the addition of weight in the TPCs doesn't affect pref. temp.
with(tl_mod, plot(topt_f ~ topt, pch = 16))
cor.test(tl_mod$topt, tl_mod$topt_f)

# rmax from both TPCs are also positively correlated (r = 0.59, p = 0.01). So, this means that none of them is weight dependent? 
with(tl_mod, plot(rmax_f ~ rmax, pch = 16))
cor.test(tl_mod$rmax, tl_mod$rmax_f)

write.csv(tl_mod, "C:/Users/Katerina/Documents/Master thesis/results/Tb_param.csv")
