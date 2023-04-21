
# Calculate RMSEs
rmse.calc <- function(pred, obs, depth=1, micro){
  env_test <- pred
  env_test$datetime <- micro$dates
  env_test <- merge(env_test, obs, by="datetime")
  
  round(sqrt(mean((env_test[,ncol(env_test)] - env_test[,3+depth])^2)), 3)
}
