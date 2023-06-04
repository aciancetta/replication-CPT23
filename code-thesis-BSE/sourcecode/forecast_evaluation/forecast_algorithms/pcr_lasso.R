pcr_lasso <- function(d, target, horizon = 1) {
  
 
  ## Scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  target_idx <- which(colnames(d) == target)
  ## get the target
  y <- d_scaled[,target_idx]    
  ## get the principal components of the other variables
  x <- d_scaled[,-target_idx]
  eigen_list <- prcomp(x, center=FALSE)
  f <- eigen_list$x 
  
  ## define the dataframes 
  d_pcr <- as.data.frame(cbind(
    y = y[(horizon+1):length(y)], y_lag =  y[1:(length(y)-horizon)],  
    f[1:(nrow(f)-horizon),]))
  
  d_test <- as.data.frame(cbind(y_lag=y,f))
  d_test <- d_test[nrow(d_test), ]
  
  ## Model tuning, training and prediction
  ## If regularization = "none", then estimator is the OLS
  pred <- tune_predict_penalized(d_pcr = d_pcr, d_test = d_test,regularization = "lasso") 

  ## Scale back the predictions
  y_hat <- pred*d_sd[target_idx] + d_mean[target_idx]
  
  ## Output
  y_hat
}



