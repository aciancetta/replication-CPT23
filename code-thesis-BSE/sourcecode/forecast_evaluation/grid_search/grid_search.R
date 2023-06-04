###isomap using grid search:
predictor.lazyts <- function(d) {
  
  ## Perform a grid search to find the optimal k value
  k_values <- c(2, 4, 6, 8, 10)  # Set the range of k values to evaluate
  mse_values <- numeric(length(k_values))  # Initialize an empty vector to store the MSE values
  
  for (i in seq_along(k_values)) {
    i=1
    result=forecast_evaluation_fredmd(forecaster = diffusion_map_reg(k=i), 
                               horizon = 1,
                               fredmd,
                               target = target_variables,
                               window_size = 672,
                               pseudo = TRUE,
                               num_cores = 7,
                               verbose = TRUE)
    mse_values[i] <- mean((y_hat - y_test)^2)
  }
  
  ## Choose the k value with the lowest MSE
  optimal_k <- k_values[which.min(mse_values)]
  print(optimal_k)
}