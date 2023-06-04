diffusion_map_reg <- function(d, target, horizon = 1, k=15) {
  
  library(diffusionMap)
  # if (!require("BiocManager", quietly = TRUE))
  #   install.packages("BiocManager")
  # BiocManager::install("destiny")
  library(destiny)
  ## Scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  target_idx <- which(colnames(d) == target)
  ## get the target
  y <- d_scaled[,target_idx]    
  ## get the principal components of the other variables
  x <- d_scaled[,-target_idx]

  dm <- DiffusionMap(x, n_eigs  = k)
  
  dm=as.data.frame(dm)
  
  # Extract the reduced features
  f<-dm[1:k]

  #f <- cbind(dm$DC1,dm$DC2,dm$DC3,dm$DC4,dm$DC5,dm$DC6,dm$DC7,dm$DC8,dm$DC9,dm$DC10,dm$DC11,
  #           dm$DC12,dm$DC13,dm$DC14,dm$DC15)
  
  #colnames(f)=c('F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11','F12') 
  ## define the dataframes 
  
  d_pcr <- as.data.frame(cbind(
    f[1:(nrow(f)-horizon),], y = y[(horizon+1):length(y)], y_lag =  y[1:(length(y)-horizon)]  
    ))
  
  d_test <- as.data.frame(cbind(f,y_lag=y))
  d_test <- d_test[nrow(d_test), ]
  
  
  ## Principal compoent regression
  m_pcr <- lm(y ~ ., data = d_pcr)
  y_hat <- predict(m_pcr, newdata = d_test)*d_sd[target_idx] + d_mean[target_idx]
  y_hat
}
