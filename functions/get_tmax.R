get_tmax <- function (model, level = 0.5) 
{
  x <- model$m$getEnv()
  formula <- stats::as.formula(model$m$formula())
  param_ind <- all.vars(formula[[3]])[!all.vars(formula[[3]]) %in% 
                                        names(model$m$getPars())]
  vals <- x[[param_ind]]
  newdata_extrap <- data.frame(x = seq(min(vals), max(vals), 
                                       by = 0.001), stringsAsFactors = FALSE)
  newdata <- data.frame(x = seq(min(vals), max(vals), by = 1), 
                        stringsAsFactors = FALSE)
  names(newdata) <- param_ind
  names(newdata_extrap) <- param_ind
  newdata$preds <- stats::predict(model, newdata = newdata)
  newdata_extrap$preds <- stats::predict(model, newdata = newdata_extrap)
  newdata_extrap <- newdata_extrap[!is.nan(newdata_extrap$preds), 
  ]
  topt = newdata[newdata$preds == max(newdata$preds, na.rm = TRUE), 
                 param_ind]
  rmax = newdata[newdata$preds == max(newdata$preds), "preds"]
  newdata_extrap_low <- newdata_extrap[newdata_extrap[, param_ind] <= 
                                         topt, ]
  newdata_extrap_high <- newdata_extrap[newdata_extrap[, param_ind] >= 
                                          topt, ]
  newdata_extrap_low <- newdata_extrap_low[newdata_extrap_low$preds >= 
                                             (level * rmax), ]
  newdata_extrap_high <- newdata_extrap_high[newdata_extrap_high$preds >= 
                                               (level * rmax), ]
  low_val <- suppressWarnings(min(newdata_extrap_low[, param_ind]))
  high_val <- suppressWarnings(max(newdata_extrap_high[, param_ind]))
  return(tmax = high_val)
}