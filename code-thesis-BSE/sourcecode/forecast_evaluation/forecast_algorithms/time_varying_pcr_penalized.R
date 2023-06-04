#' Time varying Principal Component Regression with L1 Penalization
#'
#' @param d A matrix of data
#' @param target String. Name of the target variable in the TV-PCR regression
#' @param horizon Integer. Forecast horizon
#' @param n_components Integer. Number of components of the time-varying matrix 
#' to use in regression
#' @param compute_components Logical (default is FALSE). If FALSE, then the data
#' must contain the target variable and the already-computed principal components.
#' If TRUE, the time-varying matrix is built and the principal components are extracted
#' (takes long)
#'
#' @return
#' @export
#'
#' @examples
#' 
time_varying_pcr_penalized <- function(d, target, horizon = 1, n_components=500,
                                       compute_components = FALSE
                                       ) {
  
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
  

  
  ## Scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  target_idx <- which(colnames(d) == target)
  ## get the target
  y <- d_scaled[,target_idx]    
  ## get the principal components of the other variables
  x <- d_scaled[,-target_idx]
  
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


  ## Tune lambda using rolling window cross validation (caret)
#   rolling_window_control <- caret::trainControl(method = "timeslice",
#                                 initialWindow = floor(nrow(d_pcr)*0.9),
#                                 horizon = 6,
#                                 fixedWindow = TRUE,
#                                 allowParallel = FALSE)
# 
#   lasso_reg <- caret::train(y ~ . ,
#                         data = d_pcr,
#                         method = "glmnet",
#                         family = "gaussian",
#                         trControl = rolling_window_control,
#                         # tuneLength = 3,
#                         tuneGrid = data.frame(alpha = 1,
#                                         lambda = c(0, 0.001, 0.01, 0.1)),
#                         metric='RMSE')
# 
#   ## Get prediction from best model
#   y_hat <- predict(lasso_reg$finalModel,
#           newx = as.matrix(d_pcr[nrow(d_pcr), -(ncol(d_pcr)-1)]),
#           s = lasso_reg$bestTune$lambda)
# 
#   (y_hat * d_sd[target_idx]) + d_mean[target_idx]
# }
#   
  ######################
  
  ## 
  epsilon <-.0001
  lambda_seq <- round(exp(seq(log(0.8866933), log(0.8866933*epsilon), 
                              length.out = 40)), digits = 10)
  
  test_size=40
  train_size <- nrow(d_pcr)-test_size

  test_index <- (train_size+1):(train_size+test_size)
  rmse_vec <- rep(0, length(lambda_seq))

  for (i in 1:length(lambda_seq)) {

        for (j in 0:(nrow(d_pcr) - train_size-1)) {

      fit=glmnet::glmnet(x=as.matrix(d_pcr[1:(train_size+j),-(n_components+1)]),y=d_pcr[1:(train_size+j),(n_components+1)],
                 family='gaussian',alpha=1, lambda =lambda_seq[i])

      pred <- predict(fit, newx = as.matrix(d_pcr[train_size+j+1,-(n_components+1)]), s = lambda_seq[i])
      rmse_vec[i] <- rmse_vec[i] + (d_pcr[train_size+j+1,(n_components+1)] - pred)^2
    }
  }


  #operation <- function(x) {sqrt(x/test_size)}
  #rmse_vec <- lapply(rmse_vec, operation)
  optimal_lambda <- lambda_seq[which.min(rmse_vec)]

  ################

  model_LASSO=glmnet::glmnet(x=as.matrix(d_pcr[,-(n_components+1)]),y=d_pcr[,(n_components+1)],family='gaussian',alpha=1, lambda =
                       optimal_lambda)
  y_hat <- predict(model_LASSO, newx = as.matrix(d_test), s =optimal_lambda)*d_sd[target_idx] + d_mean[target_idx]
  y_hat
  }





