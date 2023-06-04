time_varying_pcr_penalized_ada_CV <- function(d, target, horizon = 1, n_components=250,compute_components=TRUE) {
  
  
  library(RSpectra)
  library(glmnet)
  obtainMatrix1 <- function(data) {
    # Create a list to store the modified data frames
    dfs <- list()
    dfs[[1]] <- data
    
    # Loop over the desired number of rows to set to zero
    for (i in seq(from = 1, to = (nrow(data)-1), by = 1)) {
      # Make a copy of A and set the first i rows to zero
      df <- data
      df[1:i, ] <- 0
      
      # Add the modified data frame to the list
      dfs[[length(dfs) + 1]] <- df
    }
    
    # Concatenate the data frames together
    final_df <- do.call(cbind, dfs)
    
    # Return the concatenated data frame
    return(final_df)
  }
  ##############################
  
  target='UNRATE'
  horizon=1
  n_components=250
  compute_components=TRUE
  
  ##############################
  ## Scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  target_idx <- which(colnames(d) == target)
  ## get the target
  y <- d_scaled[,target_idx]    
  ## get the principal components of the other variables
  x <- d_scaled[,-target_idx]
  
  ###########
  ## Compute the principal components of the time-varying matrix
  if(compute_components){
    x_modified <- obtainMatrix1(x)
    eigen_list <- RSpectra::svds(x_modified, k=n_components)
    f <-eigen_list$u
    
    ## define the dataframes 
    d_pcr <- as.data.frame(cbind(f[1:(nrow(f)-horizon),],
                                 y = y[(horizon+1):length(y)],
                                 y_lag =  y[1:(length(y)-horizon)]))
    d_test <- as.data.frame(cbind(f,y_lag=y))
    d_test <- d_test[nrow(d_test), ]
    
  } else {
    f <- d[, -target_idx]                                            # PCs
    d_pcr <- as.data.frame(cbind(f[1:(nrow(f)-horizon),],            # lagged PCs
                                 y = y[(horizon+1):length(y)],       # target
                                 y_lag =  y[1:(length(y)-horizon)])) # lagged target
  }
  
  ######################
  
  epsilon <-.0001
  lambda_seq <- round(exp(seq(log(0.8866933), log(0.8866933*epsilon), 
                              length.out = 40)), digits = 10)

  
  test_size=40
  train_size <- nrow(d_pcr)-test_size
  
  test_index <- (train_size+1):(train_size+test_size)
  rmse_vec <- rep(0, length(lambda_seq))
  
  for (i in 1:length(lambda_seq)) {
    
        for (j in 0:(nrow(d_pcr) - train_size-1)) {
      
      fit=glmnet(x=as.matrix(d_pcr[1:(train_size+j),-(n_components+1)]),y=d_pcr[1:(train_size+j),(n_components+1)],
                 family='gaussian',alpha=0, lambda =lambda_seq[i])
      
      pred <- predict(fit, newx = as.matrix(d_pcr[train_size+j+1,-(n_components+1)]), s = lambda_seq[i])
      rmse_vec[i] <- rmse_vec[i] + (d_pcr[train_size+j+1,(n_components+1)] - pred)^2
    }
  }
  

  #operation <- function(x) {sqrt(x/test_size)}
  #rmse_vec <- lapply(rmse_vec, operation)
  optimal_lambda <- lambda_seq[which.min(rmse_vec)]
  ridge_model <- glmnet(x=as.matrix(d_pcr[,-(n_components+1)]),y=d_pcr[,(n_components+1)],family='gaussian',alpha=0, lambda =
                          optimal_lambda)
  
  coef=coef(ridge_model)[-1]
  tau=1
  penalty.factor <- 1/abs(coef)^tau
  penalty.factor[penalty.factor == Inf] <- 999999999 
  
  
  ########################.  find opt lambda for ada
  
  epsilon <-.0001
  lambda_seq <- round(exp(seq(log(0.8866933), log(0.8866933*epsilon), 
                              length.out = 40)), digits = 10)
  
  
  test_size=40
  train_size <- nrow(d_pcr)-test_size
  
  test_index <- (train_size+1):(train_size+test_size)
  rmse_vec <- rep(0, length(lambda_seq))
  
  for (i in 1:length(lambda_seq)) {
    
    for (j in 0:(nrow(d_pcr) - train_size-1)) {
      
      fit=glmnet(x=as.matrix(d_pcr[1:(train_size+j),-(n_components+1)]),y=d_pcr[1:(train_size+j),(n_components+1)],
                 family='gaussian',alpha=1, lambda =lambda_seq[i],penalty.factor = penalty.factor)
      
      pred <- predict(fit, newx = as.matrix(d_pcr[train_size+j+1,-(n_components+1)]), s = lambda_seq[i])
      rmse_vec[i] <- rmse_vec[i] + (d_pcr[train_size+j+1,(n_components+1)] - pred)^2
    }
  }
  
  
  #operation <- function(x) {sqrt(x/test_size)}
  #rmse_vec <- lapply(rmse_vec, operation)
  lambda.opt.ADA <- lambda_seq[which.min(rmse_vec)]

  ##########################

  model_LASSO.ADA=glmnet(x=as.matrix(d_pcr[,-(n_components+1)]),y=d_pcr[,(n_components+1)],penalty.factor=penalty.factor, lambda = lambda.opt.ADA)
  

  y_hat <- predict(model_LASSO.ADA, newx = as.matrix(d_test), s =lambda.opt.ADA)*d_sd[target_idx] + d_mean[target_idx]
  y_hat

}







