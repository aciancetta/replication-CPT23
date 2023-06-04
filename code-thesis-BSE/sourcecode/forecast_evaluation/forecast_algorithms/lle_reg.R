lle_reg <- function(d, target, horizon = 1,n_components=4) {
  
  ## Required package: RDRToolbox
  # if (!require("BiocManager", quietly = TRUE))
  #   install.packages("BiocManager")
  # 
  # BiocManager::install("RDRToolbox")
  
  library(RDRToolbox)
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  target_idx <- which(colnames(d) == target)
  ## get the target
  y <- d_scaled[,target_idx]    
  ## get the principal components of the other variables
  x <- d_scaled[,-target_idx]
  
  lle<-LLE(x,k=n_components)
  #lle
  
  ## define the dataframes 
  d_pcr <- as.data.frame(cbind(lle[1:(nrow(lle)-horizon),],
                               y = y[(horizon+1):length(y)], y_lag =  y[1:(length(y)-horizon)]))
  

  
  
  d_test <- as.data.frame(cbind(lle,y_lag=y))
  d_test <- d_test[nrow(d_test), ]
  
  ## Principal compoent regression
  m_pcr <- lm(y ~ ., data = d_pcr)
  y_hat <- predict(m_pcr, newdata = d_test)*d_sd[target_idx] + d_mean[target_idx]
  y_hat
}