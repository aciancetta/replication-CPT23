tvpcr <- function(d, target, horizon = 1, n_components=500,
                  regularization = c("none","lasso", "adaptive_lasso", "ridge", "elastic_net", "adaptive_lasso_BIC",'adaptive_lasso_lasso'),
                  tuning = TRUE,
                  compute_components = FALSE){
  
  if(!(regularization %in% c("none","lasso", "adaptive_lasso", "ridge", "elastic_net","adaptive_lasso_BIC",'adaptive_lasso_lasso'))){
    stop("'regularization' must be one of 'none', 'lasso', 'adaptive_lasso', 'ridge', 'elastic_net','adaptive_lasso_BIC','adaptive_lasso_lasso'")
  }
  
  ## 0) Initialization ----
  ## Scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  ## Get target and predictors
  target_idx <- which(colnames(d) == target)
  y <- d_scaled[,target_idx]    
  x <- d_scaled[,-target_idx]
  
  ## 1) Compute/load the principal components of the time-varying matrix ----
  if(compute_components){
    ## time-varying matrix: t_max x (t_max*n_var). 
    ## entries in column j > i*n_var are 0 (i = row)
    t_max <- nrow(x)
    n_var <- ncol(x)
    x_tv <- matrix(rep(x, t_max), t_max, t_max*n_var)
    x_tv[col(x_tv)>(row(x_tv)*n_var)] <- 0
    
    ## Get first n_components PCs
    eigen_list <- RSpectra::svds(x_tv, k=n_components)
    f <-eigen_list$u
  } else {
    ## If compute_components = FALSE, then the columns other than the target
    ## already contain the principal components of the time-varying matrix
    f <- d[, -target_idx]   
    ## select only the first n_components
    f <- f[, 1:n_components]
  }
  
  ## 2) Build the training set and the last (unlabelled) observation that will be
  ## used for forecasting horizon-ahead periods
  d_pcr <- as.data.frame(cbind(f[1:(nrow(f)-horizon),],
                               y = y[(horizon+1):length(y)],
                               y_lag =  y[1:(length(y)-horizon)]))
  d_test <- as.data.frame(cbind(f,y_lag=y))
  d_test <- d_test[nrow(d_test), ]
  
  
  ## 3) Model tuning, training and prediction
  pred <- tune_predict_penalized(d_pcr = d_pcr, d_test = d_test, regularization = regularization) 
  
  ## 4) Forecast (rescaled)
  y_hat <- pred * d_sd[target_idx] + d_mean[target_idx]
  
  ## Output
  y_hat
}

