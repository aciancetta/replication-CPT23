pcr_reg <- function(d, target, horizon = 1, n_eig=2, tune= TRUE) {
  
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
  f <- eigen_list$x[,1:n_eig] 
  
  ## define the dataframes 
  d_pcr <- as.data.frame(cbind(
    y = y[(horizon+1):length(y)], y_lag =  y[1:(length(y)-horizon)],  
    f[1:(nrow(f)-horizon),]))
  
  d_test <- as.data.frame(cbind(y_lag=y,f))
  d_test <- d_test[nrow(d_test), ]
  optimal_number=0

  x_temp=x
  y_temp=y
  ######################
  if (tune) {
    
    param_seq<- seq(2,15)
    rmse_vec <- rep(0, length(param_seq))
    
    test_size=40
    train_size <- nrow(d_pcr)-test_size
    
    test_index <- (train_size+1):(train_size+test_size)
   
    
    for (i in 1:length(param_seq)) {
      
      for (j in 0:(nrow(d_pcr) - train_size-2)) {
        
    
        y_temp <- d_scaled[1:(train_size+j),target_idx] 
        y_test <- d_scaled[train_size+j+1,target_idx] 
        ## get the principal components of the other variables
        x_temp <- d_scaled[1:(train_size+j),-target_idx]
        eigen_list <- prcomp(x_temp, center=FALSE)
        f <- eigen_list$x[,1:param_seq[i]] 
        
        ## define the dataframes 
        d_pcr_temp <- as.data.frame(cbind(
          y_temp = y_temp[(horizon+1):length(y_temp)], y_lag =  y_temp[1:(length(y_temp)-horizon)],  
          f[1:(nrow(f)-horizon),]))
        
        d_test_temp <- as.data.frame(cbind(y_lag=y_temp,f))
        d_test_temp <- d_test_temp[nrow(d_test_temp), ]
        
        m_pcr <- lm(y_temp ~ ., data = d_pcr_temp)
        y_hat <- predict(m_pcr, newdata = d_test_temp)
        #*d_sd[target_idx] + d_mean[target_idx]
        y_hat
        ############
        
        rmse_vec[i] <- rmse_vec[i] + (y_test- y_hat)^2
      }
    }

    optimal_number <- param_seq[which.min(rmse_vec)]
  
  }
  ################
  
  ## define the dataframes 
  eigen_list <- prcomp(x, center=FALSE)
  f <- eigen_list$x[,1:optimal_number]
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
