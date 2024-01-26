
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(MuMIn)

tl_all_param <- data.frame(code=character(),
                 model=character(), 
                 AIC=integer(),
                 topt=integer(),
                 rmax=integer(),
                 breadth=integer(),
                 stringsAsFactors=FALSE) 

source("C:\\Users\\Katerina\\Documents\\Master thesis\\R scripts\\calc_params1.R")
source("C:\\Users\\Katerina\\Documents\\Master thesis\\R scripts\\get_tmax.R")
source("C:\\Users\\Katerina\\Documents\\Master thesis\\R scripts\\get_tmin.R")


for (i in c(5, 6, 7, 8, 9, 12, 13, 14, 15, 16, 21, 22, 23, 24, 25)){
  s <- str_pad(i, 2, pad = "0")
  path <- paste0("C:\\Users\\Katerina\\Desktop\\mesocosms\\Tb_first\\TLCRP0", s, ".csv")
  tl <- read.csv(file = path, head = TRUE)
  tl <- subset(tl, select = -c(1))
  tl$code <- paste0("TLCRP0", s)
  
  active <- which(tl$Move == "mobile")
  tl <- tl[active,]
  
  #### Data format
  #### Create bins for Tb and select the rows with the 5 maximum Tb values ###
  tl_c <- subset(tl, select = c("code", "Tb", "VeDBA"))
  tl_c <- tl_c[, c("code", "Tb", "VeDBA")]
  colnames(tl_c) <- c("id", "temp", "rate")
  
  temp <- tl_c$temp
  hist(temp)
  bins <- 5*ceiling(max(temp) - min(temp))
  bins
  
  tl_c <- tl_c %>% mutate(bins = cut(temp, breaks=bins))
  tl_c <- tl_c[order(tl_c$bins, tl_c$temp, decreasing = T),]
  
  tl_c <- tl_c %>%                                 
    arrange(desc(rate)) %>% 
    group_by(bins) %>%
    slice(1:5)
  
  tl_c <- subset(tl_c, select = -c(4))
  
  #############################################################################
  
  ##### 7 models: 4 from Angiletta paper and 4 that look nice from graphs #####
  d_fits <- nest(tl_c, data = c(temp, rate)) %>%
    mutate(gaussian = map(data, ~nls_multstart(rate~gaussian_1987(temp = temp, rmax, topt, a),
                                               data = .x,
                                               iter = c(4,4,4),
                                               start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                               start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                               lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                               upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                               supp_errors = 'Y',
                                               convergence_count = FALSE)),
           beta = map(data, ~nls_multstart(rate~beta_2012(temp = temp, a, b, c, d, e),
                                           data = .x,
                                           iter = c(6,6,6,6,6),
                                           start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') - 10,
                                           start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'beta_2012') + 10,
                                           lower = get_lower_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                           upper = get_upper_lims(.x$temp, .x$rate, model_name = 'beta_2012'),
                                           supp_errors = 'Y',
                                           convergence_count = FALSE)),
           rezende = map(data, ~nls_multstart(rate~rezende_2019(temp = temp, q10, a,b,c),
                                              data = .x,
                                              iter = c(4,4,4,4),
                                              start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'rezende_2019') - 10,
                                              start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'rezende_2019') + 10,
                                              lower = get_lower_lims(.x$temp, .x$rate, model_name = 'rezende_2019'),
                                              upper = get_upper_lims(.x$temp, .x$rate, model_name = 'rezende_2019'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE)),
           quadratic = map(data, ~nls_multstart(rate~quadratic_2008(temp = temp, a, b, c),
                                                data = .x,
                                                iter = c(4,4,4),
                                                start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') - 0.5,
                                                start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') + 0.5,
                                                lower = get_lower_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                                upper = get_upper_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                                supp_errors = 'Y',
                                                convergence_count = FALSE)),
           sharpeschoolhigh = map(data, ~nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                                                       data = .x,
                                                       iter = c(4,4,4,4),
                                                       start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') - 10,
                                                       start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') + 10,
                                                       lower = get_lower_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                       upper = get_upper_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                       supp_errors = 'Y',
                                                       convergence_count = FALSE)),
           spain = map(data, ~nls_multstart(rate~spain_1982(temp = temp, a,b,c,r0),
                                            data = .x,
                                            iter = c(4,4,4,4),
                                            start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'spain_1982') - 1,
                                            start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'spain_1982') + 1,
                                            lower = get_lower_lims(.x$temp, .x$rate, model_name = 'spain_1982'),
                                            upper = get_upper_lims(.x$temp, .x$rate, model_name = 'spain_1982'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)),
           weibull = map(data, ~nls_multstart(rate~weibull_1995(temp = temp, a,topt,b,c),
                                              data = .x,
                                              iter = c(4,4,4,4),
                                              start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') - 10,
                                              start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'weibull_1995') + 10,
                                              lower = get_lower_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                              upper = get_upper_lims(.x$temp, .x$rate, model_name = 'weibull_1995'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE)))
  
  #############################################################################
  
  # stack models
  d_stack <- select(d_fits, -data) %>%
    pivot_longer(., names_to = 'model_name', values_to = 'fit', gaussian:weibull)
  # get parameters using tidy
  params <- d_stack %>%
    mutate(., est = map(fit, tidy)) %>%
    select(-fit) %>%
    unnest(est)
  
  # get predictions using augment
  newdata <- tibble(temp = seq(min(tl_c$temp), max(tl_c$temp), length.out = 100))
  d_preds <- d_stack %>%
    mutate(., preds = map(fit, augment, newdata = newdata)) %>%
    select(-fit) %>%
    unnest(preds)
  
  d_ic <- d_stack %>%
    mutate(., info = map(fit, glance),
           AICc =  map_dbl(fit, MuMIn::AICc)) %>%
    select(-fit) %>%
    unnest(info) %>%
    select(model_name, sigma, AIC, AICc, BIC, df.residual)
  
  d_ic 
  
 # plot
  data <- data.frame(model_name = d_ic$model_name, AIC = paste0("AIC = ", round(d_ic$AIC, digits = 2)))
  
  ggplot(d_preds, aes(temp, rate)) +
    geom_point(aes(temp, rate), tl_c) +
    geom_line(aes(temp, .fitted), col = 'blue') +
    facet_wrap(~model_name, ncol = 4) +
    theme_bw(base_size = 12) +
    theme(legend.position = 'none',
          strip.text = element_text(hjust = 0),
          strip.background = element_blank()) +
    labs(x = 'Body temperature (ºC)',
         y = 'VeDBA (g)',
         title = paste0('Fits of selected models for TLCRP0', s)) +
    geom_text(data = data, aes(label=AIC), x = 60/100*(max(tl$Tb)), y = 98/100*(max(tl$VeDBA)),
               size = 5) +
    theme(text=element_text(size=20),
          legend.position = c(.1, .9)) +
    geom_hline(aes(yintercept = 0), linetype = 2)
  
  
  ggsave(paste0("C:/Users/Katerina/Documents/Master thesis/some graphs/TPCs_new/TLCRP0", s, ".png"), width = 20, height = 10.4)
  
  ## Model selection
  ## In this instance, we will use AICc score to compare between models. For a model selection approach, the model with the lowest AICc score is chosen as the model that best supports the data. In this instance, it is the Sharpe-Schoolfield model.
  
  # filter for best model
  best_model = filter(d_ic, AICc == min(AICc)) %>% pull(model_name)
  best_model
  
  
  # calculate estimated parameters
  params <- d_stack %>%
    mutate(., params = map(fit, calc_params1)) %>%
    select(-fit) %>%
    unnest(params)
  
  tmax <- d_stack %>%
    mutate(., tmax = map(fit, get_tmax)) %>%
    select(-fit) %>%
    unnest(tmax)
  
  tmin <- d_stack %>%
    mutate(., tmin = map(fit, get_tmin)) %>%
    select(-fit) %>%
    unnest(tmin)
  
  tl_param <- data.frame(code = params$id, model = params$model_name, AIC = d_ic$AIC, topt = params$topt, rmax = params$rmax, breadth = params$breadth, tfmin = tmin$tmin, tfmax = tmax$tmax)
  tl_all_param <- rbind(tl_all_param, tl_param)
}

write.csv(tl_all_param, file = "C:\\Users\\Katerina\\Documents\\Master thesis\\results\\TPC_params_new.csv")

