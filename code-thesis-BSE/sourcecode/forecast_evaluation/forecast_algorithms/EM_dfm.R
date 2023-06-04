dfm_M_step <- function(factors, x) {
  
  ## Update parameters based only on available observations
  idx_available <- complete.cases(x)
  x_full <- x[idx_available,]
  factors_full <- factors[idx_available,]
  
  ## Lag 0, lag 1
  factors_lag0 <- factors_full[2:nrow(factors_full),]
  factors_lag1 <- factors_full[1:(nrow(factors_full)-1),]
  
  ## 2) Initialize parameters (matrices)
  ## Maps
  Lambda <- solve(crossprod(factors_full)) %*% crossprod(factors_full, x_full)
  A <- solve(crossprod(factors_lag1)) %*% crossprod(factors_lag1, factors_lag0)
  ## Covariances
  eps <- x - factors%*%Lambda
  eta <- factors_lag0 - factors_lag1 %*% A
  sigma_eps <- cov(eps, use = "complete.obs")
  sigma_eta <- cov(eta)
  
  ## Notation...
  th <- list(Hs = Lambda,  
             Fs = A,
             Rs = sigma_eps,
             Qs = sigma_eta)
}


dfm_E_step <- function(th, x) {
  
  d <- x
  # Get index of missing values
  index <- is.na(d)
  d[index] <- rnorm(n = sum(index))
  
  # Given th, we obtain matrices R, Q, H, F
  R <- th$Rs
  Q <- th$Qs
  H <- th$Hs
  F <- th$Fs
  
  # Initialize filter (i.e. latent factors)
  n_var <- nrow(H)
  n_latent <- nrow(F)
  captst <- nrow(d)
  filter <- matrix(0, nrow = captst, ncol = n_latent)
  filterptt <- array(dim = c(n_latent, n_latent, captst))
  
  # Initialize ML and KF
  beta00 <- rep(0, n_latent)
  P00 <- diag(n_latent)
  like <- rep(0, captst)
  
  # KALMAN FILTER
  for (it in 1:captst) { 
    ## Update only available variables at iteration t by :
    ## 1) removing the rows corresponding to the missing variables in the update matrix
    ## (i.e. set to zero the columns that multiply a missing observation)
    idx_missing <- index[it,]
    Hit <- t(H)
    Hit[idx_missing,] <- 0   
    ## 2) constraining the model to match the observed values, imposing zero variance to the measurement errors
    Rit <- R
    # diag(Rit) <- diag(Rit)*(idx_missing*1) ## checkneeded: make diagonal measurement matrix (exact DFM)
    
    # Prediction equations
    beta10 <- F %*% beta00
    P10 <- F %*% P00 %*% t(F) + Q
    
    # Forecast error
    n10 <- d[it,] - Hit %*% beta10
    
    # Likelihood function
    F10 <- Hit %*% P10 %*% t(Hit) + Rit 
    F10_inv <- MASS::ginv(F10)
    like[it] <- -0.5 * (log(det(F10)) + t(n10) %*% F10_inv %*% n10 + log(2*pi))
    
    # Kalman gain
    K <- P10 %*% t(Hit) %*% F10_inv
    
    # Updating equations (missing variables are not updated)
    beta11 <- beta10 + K %*% n10
    filter[it,] <- beta11
    P11 <- P10 - K %*% Hit %*% P10
    filterptt[,,it] <- P11
    
    beta00 <- beta11
    P00 <- P11
  }
  
  # Sum of the likelihood
  fun <- -sum(like)
  
  ## Output
  filter
}

