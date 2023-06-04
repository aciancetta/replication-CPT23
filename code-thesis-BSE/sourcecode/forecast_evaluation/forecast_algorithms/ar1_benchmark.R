ar1_benchmark <- function(d, target, horizon = 1) {

  ## Scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  ## get the target
  target_idx <- which(colnames(d) == target)
  y <- d_scaled[,target_idx]  
  
  ## Fit AR(1)
  ar1 <- arima(y, c(1,0,0))
  
  ## Forecast
  y_hat <- predict(ar1, n.ahead = horizon)
  
  ## Output
  y_hat$pred[horizon]
}