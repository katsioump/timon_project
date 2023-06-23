trans_fun <- function (Tc_init = rep(20, 60), Ts_init = Tc_init + 0.1, To_init = Tc_init + 
            0.2, Ww_g = 500, T_F_min = 33, T_F_max = 38, T_B_min = 25, 
          CT_max = 43, rho_body = 932, x_shell = 0.001, lump = 1, q = 0, 
          c_body = 3073, c_body_inner = c_body, c_body_outer = c_body, 
          k_flesh = 0.5, k_inner = k_flesh, k_outer = k_flesh, emis = 0.95, 
          alpha = 0.85, geom = 2, shape_b = 1/5, shape_c = 1/5, shape_coefs = c(10.4713, 
                                                                                0.688, 0.425, 0.85, 3.798, 0.683, 0.694, 0.743), posture = "n", 
          orient = 1, fatosk = 0.4, fatosb = 0.4, alpha_sub = 0.2, 
          pdif = 0.1, shade = 90, metout = metout_in, shadmet = shadmet_in, 
          soil = soil_in, shadsoil = shadsoil_in, press = 101325) 
{
  if (exists("day_results")) {
    rm(day_results)
  }
  require(deSolve)
  micro_sun <- cbind(metout[, 1:4], metout[, 7], soil[, 3], 
                     metout[, 12:14], metout[, 5])
  names <- c("DOY", "TIME", "TALOC", "TA2m", "VLOC", "TS", 
             "ZEN", "SOLR", "TSKYC", "RHLOC")
  colnames(micro_sun) <- names
  micro_shd <- cbind(shadmet[, 1:4], shadmet[, 7], shadsoil[, 
                                                            3], shadmet[, 12:14], shadmet[, 5])
  colnames(micro_shd) <- names
  nhs <- nrow(metout_in)
  time <- seq(0, 60 * nhs, 60)
  time <- time * 60
  Qsolf_sun <- approxfun(time, c(micro_sun[, 8], (micro_sun[1, 
                                                            8] + micro_sun[24, 8])/2), rule = 2)
  Tradf_sun <- approxfun(time, rowMeans(cbind(c(micro_sun[, 
                                                          6], (micro_sun[1, 6] + micro_sun[24, 6])/24), c(micro_sun[, 
                                                                                                                    9], (micro_sun[1, 9] + micro_sun[24, 9])/24)), na.rm = TRUE), 
                         rule = 2)
  Qsolf_shd <- approxfun(time, c(micro_shd[, 8], (micro_shd[1, 
                                                            8] + micro_shd[24, 8])/2) * (1 - shade/100), rule = 2)
  Tradf_shd <- approxfun(time, rowMeans(cbind(c(micro_shd[, 
                                                          6], (micro_shd[1, 6] + micro_shd[24, 6])/24), c(micro_shd[, 
                                                                                                                    9], (micro_shd[1, 9] + micro_shd[24, 9])/24)), na.rm = TRUE), 
                         rule = 2)
  velf_sun <- approxfun(time, c(micro_sun[, 5], (micro_sun[1, 
                                                           5] + micro_sun[24, 5])/2), rule = 2)
  velf_shd <- approxfun(time, c(micro_shd[, 5], (micro_shd[1, 
                                                           5] + micro_shd[24, 5])/2), rule = 2)
  Tairf_sun <- approxfun(time, c(micro_sun[, 3], (micro_sun[1, 
                                                            3] + micro_sun[24, 3])/2), rule = 2)
  Tairf_shd <- approxfun(time, c(micro_shd[, 3], (micro_shd[1, 
                                                            3] + micro_shd[24, 3])/2), rule = 2)
  Zenf <- approxfun(time, c(micro_sun[, 7], 90), rule = 2)
  times_sec <- seq(0, 3600 * nhs, 3600)
  times <- seq(0, 3600 * nhs, 10)
  times <- times[1:(length(times) - 1)]
  hours <- times/3600
  times_orig <- times
  emerge <- function(t, y, pars) {
    if (Zenf(t) != 90 & y[1] > T_B_min) {
      y[1] <- 0
    }
    return(y)
  }
  retreat <- function(t, y, pars) {
    if (Zenf(t) == 90 | y[1] < T_B_min) {
      y[1] <- 0
    }
    return(y)
  }
  shuttle <- function(t, y, pars) {
    if (y[1] >= T_F_max | y < T_F_min) {
      y[1] <- 0
    }
    return(y)
  }
  forage <- function(t, y, pars) {
    if (lump == 1) {
      return(y[1] - max(T_F_min, if (Tairf_shd(t) > T_F_max - 
                                     2) {
        0
      } else {
        Tairf_shd(t) + 1
      }))
    }
    if (lump == 2) {
      return(c(y[1] - max(T_F_min, if (Tairf_shd(t) > T_F_max - 
                                       2) {
        0
      } else {
        Tairf_shd(t) + 1
      }), y[2:3]))
    }
  }
  eventfun <- function(t, y, pars) {
    if (lump == 1) {
      return(y <- 1)
    }
    if (lump == 2) {
      return(y <- c(1, 1, 1))
    }
  }
  morning <- function() {
    if (lump == 1) {
      Tbs_ode <- as.data.frame(ode(y = Tc_init, times = subtime, 
                                   func = onelump_var, parms = indata, events = list(func = eventfun, 
                                                                                     root = TRUE, terminalroot = 1), rootfun = emerge, 
                                   method = "lsoda"))
      colnames(Tbs_ode) <- c("time", "Tb", "Tcfinal", "tau", 
                             "dTc")
    }
    if (lump == 2) {
      Tbs_ode <- as.data.frame(ode(y = c(Tc_init, Ts_init, 
                                         To_init), times = subtime, func = twolump, parms = indata, 
                                   events = list(func = eventfun, root = TRUE, terminalroot = 1), 
                                   rootfun = emerge, method = "lsoda"))
      colnames(Tbs_ode) <- c("time", "Tb", "Ts", "To", 
                             "Tcf")
    }
    return(Tbs_ode)
  }
  afternoon <- function() {
    if (lump == 1) {
      Tbs_ode <- as.data.frame(ode(y = Tc_init, times = subtime, 
                                   func = onelump_var, parms = indata, events = list(func = eventfun, 
                                                                                     root = TRUE, terminalroot = 1), rootfun = retreat, 
                                   method = "lsoda"))
      colnames(Tbs_ode) <- c("time", "Tb", "Tcfinal", "tau", 
                             "dTc")
    }
    if (lump == 2) {
      Tbs_ode <- as.data.frame(ode(y = c(Tc_init, Ts_init, 
                                         To_init), times = subtime, func = twolump, parms = indata, 
                                   events = list(func = eventfun, root = TRUE, terminalroot = 1), 
                                   rootfun = retreat, method = "lsoda"))
      colnames(Tbs_ode) <- c("time", "Tb", "Ts", "To", 
                             "Tcf")
    }
    return(Tbs_ode)
  }
  warming <- function() {
    if (lump == 1) {
      Tbs_ode <- as.data.frame(ode(y = Tc_init, times = subtime, 
                                   func = onelump_var, parms = indata, events = list(func = eventfun, 
                                                                                     root = TRUE, terminalroot = 1), rootfun = shuttle, 
                                   method = "lsoda"))
      colnames(Tbs_ode) <- c("time", "Tb", "Tcfinal", "tau", 
                             "dTc")
    }
    if (lump == 2) {
      Tbs_ode <- as.data.frame(ode(y = c(Tc_init, Ts_init, 
                                         To_init), times = subtime, func = twolump, parms = indata, 
                                   events = list(func = eventfun, root = TRUE, terminalroot = 1), 
                                   rootfun = shuttle, method = "lsoda"))
      colnames(Tbs_ode) <- c("time", "Tb", "Ts", "To", 
                             "Tcf")
    }
    return(Tbs_ode)
  }
  cooling <- function() {
    if (lump == 1) {
      Tbs_ode <- as.data.frame(ode(y = Tc_init, times = subtime, 
                                   func = onelump_var, parms = indata, events = list(func = eventfun, 
                                                                                     root = TRUE, terminalroot = 1), rootfun = forage, 
                                   method = "lsoda"))
      colnames(Tbs_ode) <- c("time", "Tb", "Tcfinal", "tau", 
                             "dTc")
    }
    if (lump == 2) {
      Tbs_ode <- as.data.frame(ode(y = c(Tc_init, Ts_init, 
                                         To_init), times = subtime, func = twolump, parms = indata, 
                                   events = list(func = eventfun, root = TRUE, terminalroot = 1), 
                                   rootfun = forage, method = "lsoda"))
      colnames(Tbs_ode) <- c("time", "Tb", "Ts", "To", 
                             "Tcf")
    }
    return(Tbs_ode)
  }
  cat("computing transient heat budget \n")
  posture <- "n"
  Tc_init <- Tairf_shd(0)
  Ts_init <- Tc_init
  To_init <- Tc_init
  Tairf <- Tairf_sun
  Qsolf <- Qsolf_sun
  Tradf <- Tradf_sun
  velf <- velf_sun
  if (lump == 1) {
    indata <- list(alpha = alpha, emis = emis, alpha_sub = alpha_sub, 
                   press = press, Ww_g = Ww_g, c_body = c_body, rho_body = rho_body, 
                   q = q, k_flesh = k_flesh, geom = geom, posture = posture, 
                   orient = orient, shape_b = shape_b, shape_c = shape_c, 
                   shape_coefs = shape_coefs, pdif = pdif, fatosk = fatosk, 
                   fatosb = fatosb, Tairf = Tairf, velf = velf, Qsolf = Qsolf, 
                   Tradf = Tradf, Zenf = Zenf)
    indata$c_body = 0.1
  }
  if (lump == 2) {
    indata <- list(Ww_g = Ww_g, x_shell = x_shell, geom = geom, 
                   k_inner = k_inner, k_outer = k_outer, q = q, c_body_inner = c_body_inner, 
                   c_body_outer = c_body_outer, emis = emis, rho_body_body = rho_body_body, 
                   alpha = alpha, shape_coefs = shape_coefs, shape_b = shape_b, 
                   shape_c = shape_c, posture = posture, orient = orient, 
                   fatosk = fatosk, fatosb = fatosb, alpha_sub = alpha_sub, 
                   pdif = pdif, press = press)
    indata$c_body_inner = 0.1
    indata$c_body_outer = 0.1
  }
  if (lump == 1) {
    Te <- try(ode(y = Tc_init, times = times, func = onelump_var, 
                  parms = indata)[, 2])
  }
  if (lump == 2) {
    Te <- try(ode(y = c(Tc_init, Ts_init, To_init), times = times, 
                  func = twolump, parms = indata)[, 2])
  }
  if (class(Te) == "try-error") {
    Te <- rep(NA, length(times))
  }
  if (lump == 1) {
    indata$c_body <- 3073
  }
  if (lump == 2) {
    indata$c_body_inner <- 3073
    indata$c_body_outer <- 3073
  }
  if (lump == 1) {
    Tb_open <- ode(y = Tc_init, times = times, func = onelump_var, 
                   parms = indata)[, 2]
  }
  if (lump == 2) {
    Tb_open <- ode(y = c(Tc_init, Ts_init, To_init), times = times, 
                   func = twolump, parms = indata)[, 2]
  }
  out <- 0
  bask <- 1
  daybreak <- 0
  posture <- "n"
  arvo <- times[(length(times)/2):length(times)]
  zeniths <- as.data.frame(cbind(arvo, Zenf(arvo)))
  colnames(zeniths) <- c("time", "zen")
  evening <- subset(zeniths, zen == 90)
  if (nrow(evening) == 0) {
    subtime <- times
  } else {
    sunset <- evening[1, 1]
    times <- times[times < sunset]
    subtime <- times
  }
  while (length(subtime) > 0) {
    if (daybreak == 0) {
      indata$posture <- "a"
      indata$Tairf <- Tairf_shd
      indata$Tradf <- Tradf_shd
      indata$Qsolf <- Qsolf_shd
      indata$velf <- velf_shd
      Tbs <- morning()
      Tc_last <- Tbs[nrow(Tbs), 2]
      Tc_prev <- Tbs[nrow(Tbs) - 1, 2]
      while (sign(Tc_last) != sign(Tc_prev)) {
        Tc_init <- 0
        subtime <- subset(times, times > Tbs[nrow(Tbs), 
                                             1])
        Tbs2 <- morning()
        Tc_last <- Tbs2[nrow(Tbs2), 2]
        Tc_prev <- Tbs2[nrow(Tbs2) - 1, 2]
        Tbs <- rbind(Tbs, Tbs2)
      }
      Tbs$posture <- 0
      Tbs$active <- 0
      Tbs$state <- 0
      if (exists("day_results")) {
        day_results <- rbind(day_results, Tbs)
      }
      else {
        day_results <- Tbs
      }
      Tc_init <- Tbs[nrow(Tbs), 2]
      if (lump == 2) {
        Ts_init <- Tbs[nrow(Tbs), 3]
        To_init <- Tbs[nrow(Tbs), 4]
      }
      subtime <- subset(times, times > Tbs[nrow(Tbs), 1])
      daybreak <- 1
    }
    while (bask == 1 & length(subtime) > 0) {
      indata$Tairf <- Tairf_sun
      indata$Tradf <- Tradf_sun
      indata$Qsolf <- Qsolf_sun
      indata$velf <- velf_sun
      if (Tc_init < T_F_min) {
        indata$posture <- "n"
        Tbs <- cooling()
        Tbs$posture <- 1
        Tbs$active <- 0
        Tbs$state <- 1
        if (exists("day_results")) {
          day_results <- rbind(day_results, Tbs)
        }
        else {
          day_results <- Tbs
        }
        Tc_init <- Tbs[nrow(Tbs), 2]
        if (lump == 2) {
          Ts_init <- Tbs[nrow(Tbs), 3]
          To_init <- Tbs[nrow(Tbs), 4]
        }
        subtime <- subset(times, times > Tbs[nrow(Tbs), 
                                             1])
        if (length(subtime) == 0) {
          break
        }
      }
      indata$posture <- "a"
      if (length(subtime) == 0) {
        break
      }
      Tbs <- warming()
      Tbs$posture <- 0
      Tbs$active <- 1
      Tbs$state <- 2
      if (exists("day_results")) {
        day_results <- rbind(day_results, Tbs)
      }
      else {
        day_results <- Tbs
      }
      Tc_init <- Tbs[nrow(Tbs), 2]
      if (lump == 2) {
        Ts_init <- Tbs[nrow(Tbs), 3]
        To_init <- Tbs[nrow(Tbs), 4]
      }
      subtime <- subset(times, times > Tbs[nrow(Tbs), 1])
      if (length(subtime) == 0) {
        break
      }
      if (Tc_init > T_F_min) {
        bask <- 0
      }
    }
    if (length(subtime) == 0) {
      break
    }
    indata$Tairf <- Tairf_shd
    indata$Tradf <- Tradf_shd
    indata$Qsolf <- Qsolf_shd
    indata$velf <- velf_shd
    Tbs <- cooling()
    Tbs$posture <- 0
    Tbs$active <- 0
    Tbs$state <- 3
    if (exists("day_results")) {
      day_results <- rbind(day_results, Tbs)
    }
    else {
      day_results <- Tbs
    }
    Tc_init <- Tbs[nrow(Tbs), 2]
    if (lump == 2) {
      Ts_init <- Tbs[nrow(Tbs), 3]
      To_init <- Tbs[nrow(Tbs), 4]
    }
    subtime <- subset(times, times > Tbs[nrow(Tbs), 1])
    indata$Tairf <- Tairf_sun
    indata$Tradf <- Tradf_sun
    indata$Qsolf <- Qsolf_sun
    indata$velf <- velf_sun
    if (length(subtime) == 0) {
      break
    }
    Tbs <- warming()
    Tbs$posture <- 0
    Tbs$active <- 1
    Tbs$state <- 2
    if (exists("day_results")) {
      day_results <- rbind(day_results, Tbs)
    }
    else {
      day_results <- Tbs
    }
    Tc_init <- Tbs[nrow(Tbs), 2]
    if (lump == 2) {
      Ts_init <- Tbs[nrow(Tbs), 3]
      To_init <- Tbs[nrow(Tbs), 4]
    }
    subtime <- subset(times, times > Tbs[nrow(Tbs), 1])
    if (Tc_init < T_F_min) {
      indata$posture <- "n"
      indata$Tairf <- Tairf_sun
      indata$Tradf <- Tradf_sun
      indata$Qsolf <- Qsolf_sun
      indata$velf <- velf_sun
      Tbs <- afternoon()
      Tc_last <- Tbs[nrow(Tbs), 2]
      Tc_prev <- Tbs[nrow(Tbs) - 1, 2]
      while (sign(Tc_last) != sign(Tc_prev)) {
        Tc_init <- 0
        if (lump == 2) {
          Ts_init <- 0
          To_init <- 0
        }
        subtime <- subset(times, times > Tbs[nrow(Tbs), 
                                             1])
        Tbs2 <- afternoon()
        Tc_last <- Tbs2[nrow(Tbs2), 2]
        Tc_prev <- Tbs2[nrow(Tbs2) - 1, 2]
        Tbs <- rbind(Tbs, Tbs2)
      }
      Tbs$posture <- 1
      Tbs$active <- 0
      Tbs$state <- 1
      if (exists("day_results")) {
        day_results <- rbind(day_results, Tbs)
      }
      else {
        day_results <- Tbs
      }
      Tc_init <- Tbs[nrow(Tbs), 2]
      if (lump == 2) {
        Ts_init <- Tbs[nrow(Tbs), 3]
        To_init <- Tbs[nrow(Tbs), 4]
      }
      subtime <- subset(times, times > Tbs[nrow(Tbs), 1])
      if (length(subtime) == 0) {
        break
      }
      indata$posture <- "a"
      indata$Tairf <- Tairf_shd
      indata$Tradf <- Tradf_shd
      indata$Qsolf <- Qsolf_shd
      indata$velf <- velf_shd
      Tbs <- morning()
      Tc_last <- Tbs[nrow(Tbs), 2]
      Tc_prev <- Tbs[nrow(Tbs) - 1, 2]
      while (sign(Tc_last) != sign(Tc_prev)) {
        Tc_init <- 0
        if (lump == 2) {
          Ts_init <- 0
          To_init <- 0
        }
        subtime <- subset(times, times > Tbs[nrow(Tbs), 
                                             1])
        Tbs2 <- morning()
        Tc_last <- Tbs2[nrow(Tbs2), 2]
        Tc_prev <- Tbs2[nrow(Tbs2) - 1, 2]
        Tbs <- rbind(Tbs, Tbs2)
      }
      Tbs$posture <- 0
      Tbs$active <- 0
      Tbs$state <- 0
      if (exists("day_results")) {
        day_results <- rbind(day_results, Tbs)
      }
      else {
        day_results <- Tbs
      }
      Tc_init <- Tbs[nrow(Tbs), 2]
      if (lump == 2) {
        Ts_init <- Tbs[nrow(Tbs), 3]
        To_init <- Tbs[nrow(Tbs), 4]
      }
      subtime <- subset(times, times > Tbs[nrow(Tbs), 1])
      if (length(subtime) == 0) {
        break
      }
    }
  }
  if (length(subtime) == 0 & nrow(evening) != 0) {
    subtime <- evening[, 1]
    indata$posture <- "a"
    indata$Tairf <- Tairf_shd
    indata$Tradf <- Tradf_shd
    indata$Qsolf <- Qsolf_shd
    indata$velf <- velf_shd
    if (lump == 1) {
      Tbs <- as.data.frame(ode(y = Tc_init, times = subtime, 
                               func = onelump_var, parms = indata, method = "lsoda"))
      colnames(Tbs) <- c("time", "Tb", "Tcfinal", "tau", 
                         "dTc")
    }
    if (lump == 2) {
      Tbs <- as.data.frame(ode(y = c(Tc_init, Ts_init, 
                                     To_init), times = subtime, func = twolump, parms = indata, 
                               method = "lsoda"))
      colnames(Tbs) <- c("time", "Tb", "Ts", "To", "Tcf")
    }
    Tbs$posture <- 0
    Tbs$active <- 0
    Tbs$state <- 0
    if (exists("day_results")) {
      day_results <- rbind(day_results, Tbs)
    }
    else {
      day_results <- Tbs
    }
  }
  day_results <- subset(day_results, day_results$time %in% 
                          times_orig)
  day_results$Te <- Te[1:nrow(day_results)]
  day_results$Tb_open <- Tb_open[1:nrow(day_results)]
  day_results$state[day_results$Tb < T_F_min + 0.15 & day_results$Tb > 
                      T_F_min - 0.15 & day_results$Tb_open > T_F_max] <- 3
  day_results$state[day_results$Tb < T_F_min + 0.15 & day_results$Tb > 
                      T_F_min - 0.15 & day_results$state == 2] <- 1
  day_results$active[day_results$Tb < T_F_min + 0.15 & day_results$Tb > 
                       T_F_min - 0.15] <- 0
  interval <- length(times_orig)
  Q10 <- 2.44
  mrate.reptile <- (0.11 * Ww_g^0.768 * 10^((day_results$Tb - 
                                               20) * log10(Q10)/10)) * 0.0056 * (24/interval) * 3600/1000
  day_results <- cbind(day_results, mrate.reptile)
  mrate_sum <- sum(day_results$mrate.reptile)
  inactive <- subset(day_results, active == 0)
  active <- subset(day_results, active == 1)
  active_shd <- subset(day_results, state == 3)
  mrate_sum_inactive <- sum(inactive$mrate.reptile)
  mrate_sum_active <- sum(active$mrate.reptile)
  mrate_sum_active_shd <- sum(active_shd$mrate.reptile)
  mrate_sum_inactive_shd <- mrate_sum - mrate_sum_active_shd
  T_F_max_time_Te <- length(Te[Te > T_F_max + 0.5])/(interval/24) * 
    60
  CT_max_time_Te <- length(Te[Te > CT_max])/(interval/24) * 
    60
  T_F_max_time_Tb_open <- length(Tb_open[Tb_open > T_F_max + 
                                           0.5])/(interval/24) * 60
  CT_max_time_Tb_open <- length(Tb_open[Tb_open > CT_max])/(interval/24) * 
    60
  T_F_maxtime <- length(day_results$Tb[day_results$Tb > T_F_max + 
                                         0.5])/(interval/24) * 60
  CT_max_time <- length(day_results$Tb[day_results$Tb > CT_max + 
                                         0.5])/(interval/24) * 60
  max_Tb_open <- max(Tb_open)
  min_Tb_open <- min(Tb_open)
  max_Te <- max(Te)
  min_Te <- min(Te)
  max_Tb <- max(day_results$Tb)
  min_Tb <- min(day_results$Tb)
  Hour <- trunc(day_results$time/3600)
  day_results <- cbind(Hour, day_results)
  active <- aggregate(day_results$active, by = list(day_results$Hour), 
                      sum)
  active <- active$x/(interval/24) * 60
  y <- rle(day_results$active)
  
  max_foraging_bout <- max((y$lengths[y$values == 1]))/(interval/24) * 
    60
  z <- rle(day_results$state)
  morning.bask <- z$lengths[z$values == 1][1]/(interval/24) * 
    60
  active.bouts <- y$lengths[y$values == 1]
  total.bouts <- length(active.bouts)
  if (total.bouts > 1) {
    morning.bout <- y$lengths[2]/(interval/24) * 60
    afternoon.bout <- y$lengths[length(y$lengths) - 1]/(interval/24) * 
      60
  }  else {
    morning.bout <- max_foraging_bout
    afternoon.bout <- max_foraging_bout
  }
  if (total.bouts > 2) {
    midday.bouts <- active.bouts[2:(length(active.bouts) - 
                                      1)]/(interval/24) * 60
    midday.bout1 <- midday.bouts[1]
    mean.midday.bout <- mean(midday.bouts)
  }  else {
    midday.bout1 <- max_foraging_bout
    mean.midday.bout <- max_foraging_bout
  }
  if (max_foraging_bout == "-Inf") {
    max_foraging_bout <- 0
    morning.bout <- 0
    midday.bout1 <- 0
    mean.midday.bout <- 0
    afternoon.bout <- 0
    morning.bask <- 0
  }
  sum_activity <- sum(active)
  active_sun <- active
  shade_active <- day_results$state
  shade_active[shade_active != 3] <- 0
  shade_active[shade_active == 3] <- 1
  active <- aggregate(shade_active, by = list(day_results$Hour), 
                      sum)
  active <- active$x/(interval/24) * 60
  y <- rle(shade_active)
  max_foraging_bout_shd <- max((y$lengths[y$values == 1]))/(interval/24) * 
    60
  active.bouts <- y$lengths[y$values == 1]
  total.bouts_shd <- length(active.bouts)
  if (total.bouts_shd > 1) {
    morning.bout_shd <- y$lengths[2]/(interval/24) * 60
    afternoon.bout_shd <- y$lengths[length(y$lengths) - 1]/(interval/24) * 
      60
  }  else {
    morning.bout_shd <- max_foraging_bout_shd
    afternoon.bout_shd <- max_foraging_bout_shd
  }
  if (total.bouts_shd > 2) {
    midday.bouts <- active.bouts[2:(length(active.bouts) - 
                                      1)]/(interval/24) * 60
    midday.bout1_shd <- midday.bouts[1]
    mean.midday.bout_shd <- mean(midday.bouts)
  }  else {
    midday.bout1_shd <- max_foraging_bout_shd
    mean.midday.bout_shd <- max_foraging_bout_shd
  }
  if (max_foraging_bout_shd == "-Inf") {
    max_foraging_bout_shd <- 0
    morning.bout_shd <- 0
    midday.bout1_shd <- 0
    mean.midday.bout_shd <- 0
    afternoon.bout_shd <- 0
  }
  sum_activity_shd <- sum(active)
  active_shd <- active
  sum_stats <- as.data.frame(cbind(Ww_g, T_F_min, T_F_max, 
                                   max_foraging_bout, max_foraging_bout_shd, sum_activity, 
                                   sum_activity_shd, total.bouts, total.bouts_shd, morning.bask, 
                                   morning.bout, morning.bout_shd, midday.bout1, midday.bout1_shd, 
                                   mean.midday.bout, mean.midday.bout_shd, afternoon.bout, 
                                   afternoon.bout_shd, mrate_sum, mrate_sum_inactive, mrate_sum_inactive_shd, 
                                   mrate_sum_active, mrate_sum_active_shd, T_F_max_time_Te, 
                                   CT_max_time_Te, T_F_max_time_Tb_open, CT_max_time_Tb_open, 
                                   T_F_maxtime, CT_max_time, max_Tb_open, min_Tb_open, max_Te, 
                                   min_Te, max_Tb, min_Tb))
  colnames(sum_stats) <- c("Ww_g", "T_F_min", "T_F_max", "max_bout_sun", 
                           "max_bout_shd", "sum_activity_sun", "sum_activity_shd", 
                           "bouts_sun", "bouts_shd", "morning_bask", "morning_forage_sun", 
                           "morning_forage_shd", "midday_bout1_sun", "midday_bout1_shd", 
                           "mean_midday_bout_sun", "mean_midday_bout_shd", "afternoon_forage_sun", 
                           "afternoon_forage_shd", "mrate_sum", "mrate_sum_inactive_sun", 
                           "mrate_sum_inactive_shd", "mrate_sum_active_sun", "mrate_sum_active_shd", 
                           "T_F_max_time_Te", "CT_max_time_Te", "T_F_max_time_Tb_open", 
                           "CT_max_time_Tb_open", "T_F_maxtime", "CT_max_time", 
                           "max_Tb_open", "min_Tb_open", "max_Te", "min_Te", "max_Tb", 
                           "min_Tb")
  for (i in 0:23) {
    run <- subset(day_results, Hour == i)
    y <- rle(run$active)
    run <- max((y$lengths[y$values == 1]))/(interval/24) * 
      60
    if (run == "-Inf") {
      run <- 0
    }
    if (i == 0) {
      runs <- run
    }
    else {
      runs <- c(runs, run)
    }
  }
  runs_sun <- runs
  runs_shd <- runs
  for (i in 0:23) {
    run <- subset(day_results, Hour == i)
    y <- rle(run$state)
    run <- max((y$lengths[y$values == 3]))/(interval/24) * 
      60
    if (run == "-Inf") {
      run <- 0
    }
    if (i == 0) {
      runs <- run
    }
    else {
      runs <- c(runs, run)
    }
  }
  runs_shd <- runs
  act_window <- cbind(seq(0, 23, 1), active_sun, runs_sun, 
                      active_shd, runs_shd)
  act_window <- as.data.frame(act_window)
  Tair_shd <- Tairf_shd(day_results$time)
  day_results$Tair_shd <- Tair_shd
  day_results <- day_results[, c(2, 1, 13, 3, 4, 11, 10, 5:9, 
                                 12)]
  colnames(day_results) <- c("time", "hour", "T_air_shd", "Tb", 
                             "Tb_final", "Tb_open", "Te_open", "time_constant", "dTb_dt", 
                             "posture", "active", "state", "mrate")
  colnames(act_window) <- c("time", "forage_sun", "max_bout_sun", 
                            "forage_shd", "max_bout_shd")
  day_results$mrate <- day_results$mrate * 1000
  return(list(day_results = day_results, sum_stats = sum_stats, 
              act_window = act_window))
}