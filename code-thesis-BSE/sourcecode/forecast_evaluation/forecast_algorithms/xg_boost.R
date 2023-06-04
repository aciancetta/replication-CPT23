xg_boost<-function(d, target, horizon = 1){
  library(xgboost)
  
  ## scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  ## extract the target variable and the independent variables
  target_idx <- which(colnames(d) == target)
  y <- as.matrix(d_scaled[, target_idx])
  x <- as.matrix(d_scaled[, -target_idx])
  
  ## split the data into training and testing sets 70/30 split
  
  numberOfTrainingSamples <- round(length(y) * .7)
  ## split the data into training and testing sets
  train_data <- x[1:numberOfTrainingSamples,]
  train_labels <- y[1:numberOfTrainingSamples]
  
  test_data <- x[-(1:numberOfTrainingSamples),]
  test_labels <- y[-(1:numberOfTrainingSamples)]
  
  # put our testing & training data into two seperates Dmatrixs objects
  dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
  dtest <- xgb.DMatrix(data = test_data, label= test_labels)
  
  
  ## train an XGBoost model
  params <- params <- list(objective = "reg:squarederror",eta = 0.1,max_depth = 6)
  model <- xgboost(data = dtrain, # the data   
                 nround = 10,params)
  
  #y_hat <- predict(model, newdata = dtest)[1]
  
  
  ## predict 
  y_hat <- predict(model, newdata = dtest[nrow(dtest), ]) * d_sd[target_idx] + d_mean[target_idx]
  #y_hat <- predict(model, newdata = dtest)[1]* d_sd[target_idx] + d_mean[target_idx]
  
  ## return the predicted value
  y_hat
}