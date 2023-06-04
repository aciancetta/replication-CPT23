get_rmse_table_covid <- function(evaluation_results, 
                           evaluation_benchmark = NULL, 
                           column_name = "Model") {
  
  ## Initialization
  horizon <- evaluation_results$parameters$horizon
  window_size <- evaluation_results$parameters$window_size
  pseudo <- evaluation_results$parameters$pseudo
  filename_benchmark <- paste0("results/pcr_reg_horizon", horizon,
                               "_windowsize", window_size,
                               "_pseudo", as.character(pseudo))
  
  ## Load benchmark if not provided
  if(is.null(evaluation_benchmark)){
    if(file.exists(filename_benchmark)){
      message("Loading benchmark results from ", filename_benchmark)
      
      evaluation_benchmark <- readRDS(filename_benchmark)
      evaluation_benchmark=get_covid_data(evaluation_benchmark,date='2020-01-01')
      
    } else {
      stop("evaluation_benchmark is not provided and is not available in the 'results' folder")
    }
  }
                               
  ## Get relative RMSE
  evaluation_results <- get_covid_data(evaluation_results, date='2020-01-01')
  
  rmse_results <- evaluation_results$forecast_errors |> 
    select(-1) |> 
    summarise(across(everything(), function(x) sqrt(mean(x^2))))
  rmse_benchmark <- evaluation_benchmark$forecast_errors |> 
    select(-1) |> 
    summarise(across(everything(), function(x) sqrt(mean(x^2))))

  relative_rmse <- rmse_results/rmse_benchmark
    
  
  ## Diebold-Mariano test
  
  dmtest_pvalue <- c()
  ## Perform the test on the forecast errors for each variable against the benchmark
  for(j in 2:ncol(evaluation_results$forecast_errors)){
    test <- forecast::dm.test(e1 = pull(evaluation_benchmark$forecast_errors[,j]),
                              e2 = pull(evaluation_results$forecast_errors[,j]),
                              alternative = "greater",
                              h = horizon,
                              power = 2) # For alternative="greater", 
                                         # the alternative hypothesis is that 
                                         # method 2 is more accurate than method 1.
    dmtest_pvalue[j-1] <- test$p.value
    names(dmtest_pvalue)[j-1] <- names(evaluation_results$forecast_errors)[j]
  }
  ## Collect the results in a tibble
  dmtest_results <- tibble(Variable = names(dmtest_pvalue),
                           p_value  = dmtest_pvalue)
  
  
  ## Add significance markers to the relative rmse
  relative_rmse_star <- relative_rmse |> 
    pivot_longer(everything(), names_to = "Variable", values_to = column_name) %>%
    left_join(dmtest_results, by = "Variable") |> 
    mutate(test = case_when(p_value < 0.001 ~ "***",
                            p_value < 0.01 ~ "**",
                            p_value < 0.05 ~ "*",
                            p_value < 0.1 ~ ".",
                            p_value >= 0.1 ~ "")) |> 
    mutate(across(where(is.double), ~format(round(., 4), nsmall = 4))) |> 
    unite(!!column_name, all_of(column_name), test, sep = " ") |> 
    select(-p_value)
  
  if(nrow(relative_rmse_star) != length(relative_rmse)){
    warning("Some variables are lost after adding the Diebold-Mariano test")
  }
  
      
  relative_rmse_star
}