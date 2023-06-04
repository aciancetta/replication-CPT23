get_data_list <- function(fredmd, window_size, autoload = T, autosave = T, verbose = T,
                          impute = TRUE ){
  
  # Check if list is already available. If yes, load and output the list and stop execution
  file_name <- paste0("data/evaluationlist_windowsize", window_size, ".RDS")
  if(!impute){
    file_name <- paste0("data/evaluationlist_windowsize", window_size, 
                        "_impute", impute,".RDS")
  }
  if(file.exists(file_name) & autoload){
    d_list <- readRDS(file_name)
    if(verbose){
      message("\nData evaluation list successfully loaded from ", file_name)
    }
    return(d_list)
  }
  # Else:
  
  # Initialization. -1 is to account for the tcodes in the first row
  test_size <- (nrow(fredmd)-1) - window_size
  
  if(verbose){
    message("\nCreating the list of data for window_size=", window_size, 
            ". This will take about", 
            ifelse(impute, "2 minutes", "20 seconds"))
  }
  # 1) Get a list of subsets of the data ----
  ## e.g. for window_size = 600 and if fredmd has 1+769 rows (the first one being the tcodes), 
  ## we get a list of 170 datasets:
  ## 1=tcodes, 2, 3, ..., 601                                     ( D_1 )
  ## 1=tcodes, 3, 4, ..., 602                                     ( D_2 )
  ## 1=tcodes, 4, 5, ..., 603                                     ( D_3 )
  ## ... 
  ## 1=tcodes, 1+i, 1+(i+1), 1+(i+2), ..., i + window_size        ( D_i )
  ## ---
  ## 1=tcodes, 170, 171, ..., 769                                 ( D_{169} )
  ## 1=tcodes, 171, 172, ..., 770                                 ( D_{170} )
  
  ## The idea of the evaluation is as follows. At each step i:
  ##  - train on D_i
  ##  - forecast_i = y_{t+horizon} | D_i
  ##  - test against D_{i+horizon}:  forecast_i - {last row of D_{i+horizon}}
  
  ## Notice that the test size in this case is 170 - horizon.
  ## A main advantage of allowing for different test sizes at different horizons
  ## is that the training size can stay fixed. As a consequence, once the size of 
  ## the window is chosen, we need to compute the list of the datasets only once 
  ## for all forecast horizons
    
  indexes <- list()
  for(i in 1:(test_size+1)){
    indexes[[i]] <- 1 + i:(i+window_size-1)
  }
  ## check indexes
  # lapply(indexes[c(1,2,length(indexes)-1, length(indexes))], range)
  
  fredmd_list <- lapply(1:(test_size+1), 
                        function(i){
                          fredmd[c(1, indexes[[i]]),] # c(1, ...) to keep the tcodes (first row)
                        })
  
  # 2) Preprocess the FRED-MD data ----
  d_list <- lapply(fredmd_list, preprocess_fredmd, verbose = FALSE, impute = impute)
  
  if(autosave){
    # 3) Save the result
    saveRDS(d_list, file = file_name)
    if(verbose){
      message("\nData evaluation list successfully saved as ", file_name)
    }
  }
  # Output
  return(d_list)
}
