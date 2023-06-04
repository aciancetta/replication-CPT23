pcr_quadratic_reg <- function(d, target, horizon = 1) {
  
  ## Scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  target_idx <- which(colnames(d) == target)
  ## get the target
  y <- d_scaled[,target_idx]    
  ## get the principal components of the other variables
  x <- d_scaled[,-target_idx]
  x<-cbind(x, x^2)
  eigen_list <- prcomp(x, center=FALSE)
  f <- eigen_list$x[,1:8] 
  
  ## define the dataframes 
  d_pcr <- as.data.frame(cbind(
    y = y[(horizon+1):length(y)], y_lag =  y[1:(length(y)-horizon)],  
    f[1:(nrow(f)-horizon),]))
  
  d_test <- as.data.frame(cbind(y_lag=y,f))
  d_test <- d_test[nrow(d_test), ]
  
  ## Principal compoent regression
  m_pcr <- lm(y ~ ., data = d_pcr)
  y_hat <- predict(m_pcr, newdata = d_test)*d_sd[target_idx] + d_mean[target_idx]
  y_hat
}
