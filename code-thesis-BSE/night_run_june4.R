library(tidyverse)
library(lubridate)
library(gridExtra)
library(kableExtra)
preprocessing_scripts_env <- new.env()    
file.sources = list.files(paste0(getwd(), "/sourcecode"),
                          pattern=".R$", full.names=TRUE, 
                          ignore.case=TRUE, recursive = TRUE)
sapply(file.sources, source, preprocessing_scripts_env)
attach(preprocessing_scripts_env, name="sourced_scripts")
target_variables <- c("UNRATE", "CPIAUCSL", "INDPRO", "S&P 500")

## Setup evaluation
pseudo <- FALSE      # pseudo real time
window_size <- 487   # size of the rolling window

## Set number of cores for parallel execution
num_cores <- parallel::detectCores() - 1

for(horizon in c(1,3)){
  ## String summarising evaluation setup
  setup_name <- paste0("_horizon", horizon, "_windowsize", window_size, "_pseudo", as.character(pseudo))
  ## Collect setup in a simple function
  run_evaluation <- function(m, ...){
    res <- forecast_evaluation_fredmd(forecaster = m, 
                                      ...,
                                      horizon = horizon,
                                      fredmd = NULL,
                                      target = target_variables,
                                      window_size = window_size,
                                      pseudo = pseudo,
                                      num_cores = num_cores,
                                      verbose = TRUE,
                                      impute = TRUE)
    
    ## Save results to disk
    filename <- paste0("results/", deparse(substitute(m)), ..., setup_name)
    saveRDS(object = res, file = filename)
    
    ## Output results
    res
  }
  
  ## compile
  run_evaluation <- compiler::cmpfun(run_evaluation)
  
  ## Run variable by variable to load variable-specific training data and components
  tvpcr_lasso_res <- list()
  for(i in 1:length(target_variables)){
    cat("\n", target_variables[i], " (number ", i, " of ", length(target_variables), ")\n", sep = "")
    tvpcr_lasso_res[[i]] <- forecast_evaluation_fredmd(forecaster = tvpcr, 
                                                       n_components = 400,
                                                       horizon = horizon,
                                                       tuning = NULL,
                                                       regularization = "lasso",
                                                       fredmd = NULL,
                                                       target = target_variables[i],
                                                       window_size = window_size,
                                                       pseudo = pseudo,
                                                       num_cores = num_cores,
                                                       verbose = TRUE,
                                                       load_components = TRUE)
  }
  
  ## Collect results
  tvpcr_lasso_res <- collect_results(tvpcr_lasso_res)
  saveRDS(tvpcr_lasso_res, paste0("results/tvpcr_lasso", setup_name))
  
  
}