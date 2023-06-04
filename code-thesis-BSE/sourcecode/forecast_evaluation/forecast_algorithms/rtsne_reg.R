rtsne_reg <- function(d, target, horizon = 1) {
  library(Rtsne)
  ## Scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  target_idx <- which(colnames(d) == target)
  ## get the target
  y <- d_scaled[,target_idx]    
  ## get the principal components of the other variables
  x <- d_scaled[,-target_idx]
  
  ## Perform Isomap
  rtsne <- Rtsne(x, pca = TRUE, dims = 2, theta = 0.25)
  #trade off with speed/accurace of theta, the close to 0, the more accurate.
  ## Create the PCR dataset
  
  ## define the dataframes 
  # d_pcr <- as.data.frame(cbind(rtsne$Y[1:(nrow(rtsne$Y)-1),],
  #                              y = y[2:length(y)], y_lag =  y[1:(length(y)-1)]))
  d_pcr <- as.data.frame(cbind(rtsne$Y[1:(nrow(rtsne$Y)-horizon),],
                               y = y[(horizon+1):length(y)], y_lag =  y[1:(length(y)-horizon)]))

  d_test <- as.data.frame(cbind(rtsne$Y,y_lag=y))
  d_test <- d_test[nrow(d_test), ]
  
  ## Principal compoent regression
  m_pcr <- lm(y ~ ., data = d_pcr)
  y_hat <- predict(m_pcr, newdata = d_test)*d_sd[target_idx] + d_mean[target_idx]
  y_hat
}