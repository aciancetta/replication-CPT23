lasso <- function(d, target, horizon = 1) {
  
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  target_idx <- which(colnames(d) == target)
  y <- d_scaled[,target_idx]    
  x <- d_scaled[,-target_idx]
  
  d_pcr <- as.data.frame(cbind(y = y[(horizon+1):length(y)],
                               x_lag =  d_scaled[1:(nrow(d_scaled)-horizon),]))
  
  d_test <- as.data.frame(d_scaled) #d_scaled
  d_test <- d_test[nrow(d_test), ]

  
  
  ## Model tuning, training and prediction
  ## If regularization = "none", then estimator is the OLS
  pred <- tune_predict_penalized(d_pcr = d_pcr, d_test = d_test,regularization = "lasso") 
  
  ## Scale back the predictions
  y_hat <- pred*d_sd[target_idx] + d_mean[target_idx]
  
  ## Output
  y_hat

}
