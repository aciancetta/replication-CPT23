collect_results <- function(result_list){
  ## Collect the results from many variables to a single list of results
  res <- list(forecasts       = result_list[[1]]$forecasts[,1] |> bind_cols(lapply(1:length(result_list), function(i){result_list[[i]]$forecasts[,-1]}) |> bind_cols()), 
              ground_truth    = result_list[[1]]$ground_truth[,1] |> bind_cols(lapply(1:length(result_list), function(i){result_list[[i]]$ground_truth[,-1]}) |> bind_cols()), 
              forecast_errors = result_list[[1]]$forecast_errors[,1] |> bind_cols(lapply(1:length(result_list), function(i){result_list[[i]]$forecast_errors[,-1]}) |> bind_cols()), 
              parameters      = result_list[[1]]$parameters)
  ## Output
  res
}

