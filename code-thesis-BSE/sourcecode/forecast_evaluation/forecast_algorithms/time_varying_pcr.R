time_varying_pcr <- function(d, target, horizon = 1, n_components) {
  
  library(RSpectra)
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
  
  x_modified <- obtainMatrix1(x)
  eigen_list <- svds(x_modified, k=n_components)

  f <-eigen_list$u
  
  ## define the dataframes 
  d_pcr <- as.data.frame(cbind(f[1:(nrow(f)-horizon),],
                               y = y[(horizon+1):length(y)],
                               y_lag =  y[1:(length(y)-horizon)]
  )
  )

  
  d_test <- as.data.frame(cbind(f,y_lag=y))
  d_test <- d_test[nrow(d_test), ]
  
  ## Principal compoent regression
  m_pcr <- lm(y ~ ., data = d_pcr)
  y_hat <- predict(m_pcr, newdata = d_test)*d_sd[target_idx] + d_mean[target_idx]
  y_hat
}
