randomforest_reg<-function(d, target, horizon = 1){
  library(randomForest)
  library(caret)
  ## scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
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
  
  ## define hyperparameters to search
  hyperparameters <- expand.grid(
    ntree = c(10, 20, 50, 100),  # number of trees in the forest
    mtry = c(2, 4, 6,sqrt(ncol(train_data))),  # number of variables randomly sampled at each split
    max_depth = c(5, 10, 15),  # maximum depth of each tree
    min_samples_leaf = c(1, 5, 10)  # minimum number of samples required to be at a leaf node
  )
  
  
  model <- randomForest(train_data, train_labels, tuneGrid = hyperparameters,trControl = trainControl(method = "cv", number = 10))
  
  # put our testing & training data into two seperates Dmatrixs objects
  dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
  dtest <- xgb.DMatrix(data = test_data, label= test_labels)
  
  #model=randomForest(train_data, train_labels, ntree = 20, mtry = sqrt(ncol(train_data)))
  y_hat <- predict(model, newdata = test_data[nrow(test_data), ]) * d_sd[target_idx] + d_mean[target_idx]
  y_hat
  
}