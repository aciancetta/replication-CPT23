kernel_pcr <- function(d, target, horizon = 1, 
                                 kernel = c("gaussian", "poly"), 
                                 sigma = 0.001, degree = 2,
                                 n_components = 8) {
  
  ## Scale the data
  library("kernlab")
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  target_idx <- which(colnames(d) == target)
  ## get the target
  y <- d_scaled[,target_idx]    
  ## get the principal components of the other variables
  x <- d_scaled[,-target_idx]
  x <- as_tibble(x)
  
  ## Set kernel-specific parameters
  if(kernel == "gaussian"){
    kernel <- 'rbfdot'
    kpar <- list(sigma = sigma)
  } else if(kernel == "poly"){
    kernel <- 'polydot'
    kpar <- list(degree = degree)
  } else {
    stop(paste("kernel", kernel, "not implemented yet. Only 'gaussian' and 'poly' kernels are available"))
  }
  
  ## Fit the model
  kpca <- kpca(~., data = x, kernel = kernel, features = n_components, kpar = kpar)
  f <- kpca@pcv
  
  ## define the dataframes 
  d_pcr <- as.data.frame(cbind(f[1:(nrow(f)-horizon),],
                               y = y[(horizon+1):length(y)], y_lag =  y[1:(length(y)-horizon)]))
  
  d_test <- as.data.frame(cbind(f,y_lag=y))
  d_test <- d_test[nrow(d_test), ]
  
  ## Principal compoent regression
  m_pcr <- lm(y ~ ., data = d_pcr)
  y_hat <- predict(m_pcr, newdata = d_test)*d_sd[target_idx] + d_mean[target_idx]
  y_hat
}
