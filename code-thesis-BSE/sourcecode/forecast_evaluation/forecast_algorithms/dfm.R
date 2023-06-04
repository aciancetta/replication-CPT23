dfm <- function(d, target=NULL, horizon = 1, n_components = 6, maxiter = 1000) {
  
  ## Scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  ## 1) Initialize factors with PCA and parameters via OLS (this can be done directly using the M_step function)
  x_imputed <- x <- d# imputed dataset used only in the very first initialization of the factors
  x_imputed[is.na(x)] <- 0 ## EM impute
  eigen_list <- RSpectra::svds(x_imputed, k = n_components)
  factors <- eigen_list$u
  th <- dfm_M_step(factors, x)
  
  ## 2) Alternate E_step and M_step 
  distance <- Inf
  iter <- 0
  while(distance > 1e-6 & iter < maxiter){
    th_old   <- th
    factors  <- dfm_E_step(th, x)
    th       <- dfm_M_step(factors, x)
    distance <- sum(abs(as.vector(unlist(th)) - as.vector(unlist(th_old))))
    iter <- iter + 1
  }
  
  ## Get forecast based on last estimated factors and parameters
  `%^%` <- expm::`%^%`
  Fh <-  th$Fs %^% horizon
  pred <-  t(th$Hs) %*% Fh %*% factors[nrow(factors),]
  ## Back-scale to original mean and standard deviation
  y_hat <- pred*d_sd + d_mean
  if(!is.null(target)){
    y_hat <- y_hat[target,]
  }
  
  ## Output the forecasts
  t(y_hat)
}




