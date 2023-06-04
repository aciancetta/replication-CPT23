#' Tune and estimate penalized time-series regression
#'
#' @param d_pcr A dataset containing the target variable in a named column "y"
#' @param regularization Type of penalization in regression estimation. One of
#' "none","lasso", "adaptive_lasso".
#'
#' @return A model object that can be passed through the predict function
#' @export
#'
#' @examples
#' 
tune_predict_penalized <- function(d_pcr, 
                                   d_test,
                                   regularization = c("none","lasso", "adaptive_lasso", "ridge", "elastic_net","adaptive_lasso_BIC",'adaptive_lasso_lasso')) {
    
  ## Use rolling window cross validation (caret)
  rolling_window_control <- caret::trainControl(method = "timeslice",
                                                initialWindow = floor(nrow(d_pcr)*0.9),
                                                horizon = 6,
                                                fixedWindow = TRUE,
                                                allowParallel = FALSE)
  
  ## Run tuning and estimation of the best model
  if(regularization == "none"){
    ## Simple pcr
    model <- lm(y ~ ., data = d_pcr)
    pred <- predict(model, newdata = d_test)
  }
  if(regularization == "lasso"){
    ## Rolling-window cross validation
    model <- caret::train(y ~ . ,
                          data = d_pcr,
                          method = "glmnet",
                          family = "gaussian",
                          trControl = rolling_window_control,
                          tuneGrid = data.frame(alpha = 1,
                                                lambda = c(0,
                                                           0.001, 0.003, 0.005, 0.007, 
                                                           0.01, 0.03, 0.05, 0.07,  
                                                           0.1, 0.3, 0.5, 0.7)),
                          metric='RMSE')

    
    pred <- predict(object = model$finalModel,
                    newx = as.matrix(d_test),
                    s = model$bestTune$lambda)
    
  } else if(regularization == "adaptive_lasso"){
    ## Step 1 - Ridge regression to initialize the variable-specific penalty factors
    model_ridge <- caret::train(y ~ . ,
                          data = d_pcr,
                          method = "glmnet",
                          family = "gaussian",
                          trControl = rolling_window_control,
                          tuneGrid = data.frame(alpha = 0,    # Ridge
                                                lambda = c(0,
                                                           0.001, 0.003, 0.005, 0.007, 
                                                           0.01, 0.03, 0.05, 0.07,  
                                                           0.1, 0.3, 0.5, 0.7)),
                          metric='RMSE')
    
    ## Define penalty factors as 1/|ridge_estimates|
    ridge_estimates <- coef(model_ridge$finalModel, s = model_ridge$bestTune$lambda)[-1]
    tau <- 1
    penalty.factor <- 1/abs(ridge_estimates)^tau
    penalty.factor[penalty.factor == Inf] <- 999999999 
    
    ## Step 2 - LASSO with variable-specific penalty factors
    model <- caret::train(y ~ . ,
                          data = d_pcr,
                          method = "glmnet",
                          penalty.factor = penalty.factor,
                          family = "gaussian",
                          trControl = rolling_window_control,
                          tuneGrid = data.frame(alpha = 1,    # Lasso
                                                lambda = c(0,
                                                           0.001, 0.003, 0.005, 0.007, 
                                                           0.01, 0.03, 0.05, 0.07,  
                                                           0.1, 0.3, 0.5, 0.7)),
                          metric='RMSE')
    
    ## Get predictions
    pred <- predict(object = model$finalModel,
                    newx = as.matrix(d_test),
                    s = model$bestTune$lambda)
    
  } else if(regularization == "ridge"){
    ## Fit ridge regression
    model <- caret::train(y ~ . ,
                                data = d_pcr,
                                method = "glmnet",
                                family = "gaussian",
                                trControl = rolling_window_control,
                                tuneGrid = data.frame(alpha = 0,    # Ridge
                                                      lambda = c(0,
                                                                 0.001, 0.003, 0.005, 0.007, 
                                                                 0.01, 0.03, 0.05, 0.07,  
                                                                 0.1, 0.3, 0.5, 0.7)),
                                metric='RMSE')
    
    
    ## Get predictions
    pred <- predict(object = model$finalModel,
                    newx = as.matrix(d_test),
                    s = model$bestTune$lambda)
    
    
  } else if(regularization == "elastic_net"){
    ## Fit ridge regression
    model <- caret::train(y ~ . ,
                          data = d_pcr,
                          method = "glmnet",
                          family = "gaussian",
                          trControl = rolling_window_control,
                          tuneGrid = expand.grid(alpha  = c(0.2, 0.4, 0.5, 0.6, 0.8),    
                                                lambda = c(0,
                                                           0.001, 0.003, 0.005, 0.007, 
                                                           0.01, 0.03, 0.05, 0.07,  
                                                           0.1, 0.3, 0.5, 0.7)),
                          metric='RMSE')
    
    
    ## Get predictions
    pred <- predict(object = model$finalModel,
                    newx = as.matrix(d_test),
                    s = model$bestTune$lambda)
    
    
  }else if(regularization == "adaptive_lasso_BIC"){
    ## Step 1 - Ridge regression to initialize the variable-specific penalty factors
    model_ridge <- caret::train(y ~ . ,
                                data = d_pcr,
                                method = "glmnet",
                                family = "gaussian",
                                trControl = rolling_window_control,
                                tuneGrid = data.frame(alpha = 0,    # Ridge
                                                      lambda = c(0,
                                                                 0.001, 0.003, 0.005, 0.007, 
                                                                 0.01, 0.03, 0.05, 0.07,  
                                                                 0.1, 0.3, 0.5, 0.7)),
                                metric='RMSE')
    
    ## Define penalty factors as 1/|ridge_estimates|
    ridge_estimates <- coef(model_ridge$finalModel, s = model_ridge$bestTune$lambda)[-1]
    tau <- 1
    penalty.factor <- 1/abs(ridge_estimates)^tau
    penalty.factor[penalty.factor == Inf] <- 999999999 
    
    
    ######
    ADA.lasso.bic <- function(y,x,penalty.factor,extended=FALSE) {
      #Select model in LASSO path with best BIC (using LASSO regression estimates)
      #Input
      # - y: vector with response variable
      # - x: design matrix
      #
      #Output: list with the following elements
      # - coef: LASSO-estimated regression coefficient with lambda set via BIC
      # - ypred: predicted y
      # - lambda.opt: optimal value of lambda
      # - lambda: data.frame with bic and number of selected variables for each value of lambda
      require(glmnet)
      fit <- glmnet::glmnet(x=x,y=y,family='gaussian',alpha=1, penalty.factor = penalty.factor)
      pred <- cbind(1,x) %*% rbind(fit$a0,fit$beta)
      n <- length(y)
      p <- colSums(fit$beta!=0) + 1
      if (!extended){
        bic <- n * log(colSums((y-pred)^2)/length(y)) + n*(log(2*pi)+1) + log(n)*p 
      } else {
        bic <- n * log(colSums((y-pred)^2)/length(y)) + n*(log(2*pi)+1) + log(n)*p + 2*log(choose(ncol(x),p))
      }
      sel <- which.min(bic)
      beta <- c(fit$a0[sel],fit$beta[,sel]); names(beta)[1]= 'Intercept'
      ypred <- pred[,sel]
      ans <- list(bic=min(bic),coef=beta,ypred=ypred,lambda.opt=fit$lambda[sel],lambda=data.frame(lambda=fit$lambda,bic=bic,nvars=p))
      return(ans)
    }
    ###############
    
    lambda.opt.ADA=ADA.lasso.bic(x=as.matrix(d_pcr[, !colnames(d_pcr) %in% "y"]),y=d_pcr$y,penalty.factor)$lambda.opt 
    
    ###############
    model_LASSO.ADA=glmnet::glmnet(x=as.matrix(d_pcr[, !colnames(d_pcr) %in% "y"]),y=d_pcr$y,penalty.factor=penalty.factor, lambda = lambda.opt.ADA)
    
    
    pred <- predict(model_LASSO.ADA, newx = as.matrix(d_test), s =lambda.opt.ADA)
    
  }else if(regularization == "adaptive_lasso_lasso"){
    ## Step 1 - Ridge regression to initialize the variable-specific penalty factors
    model_lasso <- caret::train(y ~ . ,
                                data = d_pcr,
                                method = "glmnet",
                                family = "gaussian",
                                trControl = rolling_window_control,
                                tuneGrid = data.frame(alpha = 1,    # Lasso
                                                      lambda = c(0,
                                                                 0.001, 0.003, 0.005, 0.007, 
                                                                 0.01, 0.03, 0.05, 0.07,  
                                                                 0.1, 0.3, 0.5, 0.7)),
                                metric='RMSE')
    
    ## Define penalty factors as 1/|ridge_estimates|
    lasso_estimates <- coef(model_lasso$finalModel, s = model_lasso$bestTune$lambda)[-1]
    tau <- 1
    penalty.factor <- 1/abs(lasso_estimates)^tau
    penalty.factor[penalty.factor == Inf] <- 999999999 
    
    ## Step 2 - LASSO with variable-specific penalty factors
    model <- caret::train(y ~ . ,
                          data = d_pcr,
                          method = "glmnet",
                          penalty.factor = penalty.factor,
                          family = "gaussian",
                          trControl = rolling_window_control,
                          tuneGrid = data.frame(alpha = 1,    # Lasso
                                                lambda = c(0,
                                                           0.001, 0.003, 0.005, 0.007, 
                                                           0.01, 0.03, 0.05, 0.07,  
                                                           0.1, 0.3, 0.5, 0.7)),
                          metric='RMSE')
    
    ## Get predictions
    pred <- predict(object = model$finalModel,
                    newx = as.matrix(d_test),
                    s = model$bestTune$lambda)
    
  }    
  
  ## Output prediction
  pred
}





