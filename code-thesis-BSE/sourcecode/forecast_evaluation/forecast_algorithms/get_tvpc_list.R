get_tvpc_list <- function(target, fredmd, window_size, 
                        k = 100, num_cores = 1,
                        autoload = T, autosave = T, verbose = T){
  
  
  # Check if list is already available. If yes, load and output the list and stop execution
  file_name <- paste0("data/componentlist", k, "_", target,"_windowsize", window_size, ".RDS")
  if(file.exists(file_name) & autoload){
    pc_list <- readRDS(file_name)
    if(verbose){
      message("\nPrincipal component list successfully loaded from ", file_name)
    }
    return(pc_list)
  }
  
  ## Get the list of datasets
  d_list <- get_data_list(fredmd = fredmd, 
                          window_size = window_size, 
                          autoload = autoload, 
                          autosave = autosave, 
                          verbose = verbose)
  
  ## Get time varying principal components
  ## compile function for faster execution
  get_tvpc_cmp <- compiler::cmpfun(get_tvpc)
  if(num_cores == 1){
    pc_list <- pbapply::pblapply(d_list, get_tvpc_cmp, target, k)
  } else {
    cl <- parallel::makeCluster(num_cores)
    pc_list <- pbapply::pblapply(d_list, get_tvpc_cmp, target, k, cl = cl)
    parallel::stopCluster(cl)
  }
  

  if(autosave){
    # 3) Save the result
    saveRDS(pc_list, file = file_name)
    if(verbose){
      message("\nPrincipal component list successfully saved as ", file_name)
    }
  }
  
  return(pc_list)
    
}


