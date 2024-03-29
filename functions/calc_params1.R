calc_params1 <- function(model){
  t <- data.frame(rmax = suppressWarnings(tryCatch(rTPC::get_rmax(model), error = function(err) NA)),
                  topt = suppressWarnings(tryCatch(rTPC::get_topt(model), error = function(err) NA)),
                  ctmin = suppressWarnings(tryCatch(rTPC::get_ctmin(model), error = function(err) NA)),
                  ctmax = suppressWarnings(tryCatch(rTPC::get_ctmax(model), error = function(err) NA)),
                  e = suppressWarnings(tryCatch(rTPC::get_e(model), error = function(err) NA)),
                  eh = suppressWarnings(tryCatch(rTPC::get_eh(model), error = function(err) NA)),
                  q10 = suppressWarnings(tryCatch(rTPC::get_q10(model), error = function(err) NA)),
                  thermal_safety_margin = suppressWarnings(tryCatch(rTPC::get_thermalsafetymargin(model), error = function(err) NA)),
                  thermal_tolerance = suppressWarnings(tryCatch(rTPC::get_thermaltolerance(model), error = function(err) NA)),
                  breadth = suppressWarnings(tryCatch(rTPC::get_breadth(model, level = 0.5), error = function(err) NA)),
                  skewness = suppressWarnings(tryCatch(rTPC::get_skewness(model), error = function(err) NA)),
                  stringsAsFactors = FALSE)
  
  return(t)
}