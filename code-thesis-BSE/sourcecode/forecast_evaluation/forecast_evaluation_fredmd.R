#' Run sliding window forecast evaluation on FRED-MD data
#'
#' @param forecaster Function that takes the data as argument and outputs the predictions. 
#' It must have three arguments: d (the input data), target (the name of the target variable), 
#' horizon (the forecast horizon)
#' @param horizon Forecast horizon
#' @param fredmd A tibble containing the FRED-MD data. The first column must contain the dates.
#' @param target String reporting the name of the target variable in the data. 
#' If NULL, the forecasts for all the variables are computed (applicable for multivariate-target
#' methods like the Dynamic Factor Model)
#' @param window_size Size of the train set in each iteration of the rolling window evaluation
#' @param scale Logical. If true (default) the data are scaled before the forecaster is run.
#' The predictions are reported in the original scale.
#' @param pseudo Logical. If true (default) a pseudo real-time evaluation is performed.
#' If FALSE (not implemented yet), the argument data must be a list of all the vintages 
#' of the data.
#' @param load_components Logical (default is FALSE). Useful for the TV-PCR algorithm, 
#' that can take long time to compute the principal components
#' 
#'
#' @return A list of tibbles: forecasts, ground_truth, forecast_error
#' @export
#'
#' @examples
#' 
forecast_evaluation_fredmd <- function(forecaster, ...,
                                       horizon = 1,
                                       fredmd = NULL,
                                       target = NULL,
                                       window_size = 672,
                                       # scale = TRUE,
                                       pseudo = TRUE,
                                       num_cores = 1,
                                       verbose = TRUE,
                                       load_components = FALSE,
                                       impute = TRUE) {
  
  ## Checks and warnings
  if(is.null(fredmd) & pseudo){
    stop("A dataset 'fredmd' must be passed for pseudo-real-time evaluation")
  }
  if(load_components & !(deparse(substitute(forecaster)) %in% c("time_varying_pcr_penalized", "time_varying_pcr", "tvpcr"))){
    stop("load_components = TRUE is allowed only with TV-PCR forecasters")
  }
  if(load_components & length(target) != 1){
    stop("The target must be of length 1 if load_components = TRUE")
  }
  
  # 0) Initialization
  # Remove last row if all the variables are missing
  if(pseudo){
    if(sum(!is.na(fredmd[nrow(fredmd),-1])) == 0){
      fredmd <- fredmd[-nrow(fredmd), ]
    }
  }
  
  # 1) Get the list of preprocessed data
  if(!pseudo){
    d_list <- readRDS("data/fredmd_vintages/evaluationlist_windowsize487_imputeTRUE_pseudoFALSE.RDS.RDS")
    d_list <- lapply(d_list, function(d){d[-nrow(d), ]})
    fredmd <- d_list[[1]]
  } else {
    d_list <- get_data_list(fredmd, window_size, impute = impute)#, ...)
  }
  
  
  ## If target is NULL, all the columns are considered in the test set (useful for dynamic factor models)
  predict_all_variables <- FALSE
  if(is.null(target)){
    target <- names(fredmd)[-1]
    predict_all_variables <- TRUE
  }
  dates_name <- names(fredmd)[1]
  test_size  <- length(d_list) - horizon

  ## Train list
  train_list <- d_list[1:test_size]
  ## Test list
  test_list  <- d_list[(1+horizon):length(d_list)]
  test_list  <- lapply(test_list, function(d) d[nrow(d),] |> dplyr::select(all_of(c(dates_name, target))))
  
  ## Load the components if requested
  if(load_components){
    if(pseudo){
      pc_list <- get_tvpc_list(target = target, fredmd = fredmd, window_size = window_size, 
                               k = 500) # maximum number stored in our exercise. Building this dataset required
      # 12 hours on a machine with Intel i7-11370H using 7 cores. 
      
      pc_list <- pc_list[1:test_size]
    } else {
      pc_list <- readRDS(file_name <- paste0("data/fredmd_vintages/componentlist400_", target,
                                             "_windowsize", window_size, "_pseudoFALSE.RDS"))
      pc_list <- pc_list[1:test_size]
    }
    ## Obtain the new training set: target variable + components
    train_list <- mapply(function(d_train, pc_train){d_train |> 
        dplyr::select(1, all_of(target)) |> 
        bind_cols(as_tibble(pc_train))},
        train_list, pc_list, SIMPLIFY = FALSE)
  }
  
  # 2) Model selection ----
  ## not implemented yet. Here goes the code for tuning the model hyperparameters
  ## using the train_list
  ## (This has actually been implemented within the forecasters)
  
  
  # 3) Forecast ----
  require(lubridate)
  
  ## Define the algorithm
  algorithm <- function(d, ...){
    force(d)
    x <- as.matrix(d[,-1])
    month <- mdy(d[nrow(d), 1]) %m+% months(horizon)
    res <- tibble(dates_name = month)
    
    ## If target = NULL, then predict all the variables (for factor models)
    if(predict_all_variables){
      pred <- forecaster(x, target = NULL, horizon = horizon, ...)
      res <- res |> bind_cols(pred)
    } else {
      ## otherwise, forecast each target variable separately
      for(j in 1:length(target)){
        pred <- forecaster(x, target = target[j], horizon = horizon, ...)
        res <- res |> bind_cols(pred)
      }
      names(res) <- c(dates_name, target)
    }
    
    res
  }
  
  
  ## Run the evaluation
  if(num_cores == 1){
    forecast_list <- pbapply::pblapply(train_list, algorithm, ...)
  } else {
    if(verbose){
      message("\nInitialization - Sending local space to parallel workers")
    }
    ## Run parallel execution of the forecasting algorithm
    environment(algorithm) <- environment(forecaster)  ## Not very elegant, but it fixes the misalignment of the environments...
    cl <- parallel::makeCluster(num_cores)
    parallel::clusterEvalQ(cl, library(lubridate))
    parallel::clusterEvalQ(cl, library(tidyverse))
    parallel::clusterExport(cl, 
                            varlist = ls(),
                            envir = environment())
    
    if(verbose){
      message("\nRunning forecast evaluation")
    }
    
    forecast_list <- pbapply::pblapply(train_list, algorithm, ..., cl=cl)
    parallel::stopCluster(cl)
  }
  ## Reshape the results and compute the forecast errors
  forecasts <- forecast_list |> bind_rows()
  ground_truth <- test_list |> bind_rows()
  forecast_errors <- tibble(forecasts[,1], forecasts[,-1] - ground_truth[,-1])
  
  
  ## Output
  return(list(forecasts       = forecasts,
              ground_truth    = ground_truth,
              forecast_errors = forecast_errors,
              parameters      = list(horizon     = horizon,
                                     window_size = window_size,
                                     pseudo      = pseudo)
  ))  
}