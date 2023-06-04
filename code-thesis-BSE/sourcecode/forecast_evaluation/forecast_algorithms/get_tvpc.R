get_tvpc <- function(d_tibble, target, k){
  
    ## Convert into matrix
    d <- as.matrix(d_tibble[,-1])
    ## Get target index
    target_idx <- which(colnames(d) == target)
    
    ## Scale the data
    d_scaled <- scale(d)
    target_mean <- attr(d_scaled, "scaled:center")[target_idx]
    target_sd <- attr(d_scaled, "scaled:scale")[target_idx]
    
    ## get the target
    y <- d_scaled[,target_idx]    
    ## get the principal components of the other variables
    x <- d_scaled[,-target_idx]
    
    ## Create time varying matrix by repeating t_max times the original matrix
    ## and setting to zero the columns j > i*n_var (i = row number)
    t_max <- nrow(x)
    n_var <- ncol(x)
    x_tv <- matrix(rep(x, t_max), t_max, t_max*n_var)
    x_tv[col(x_tv)>(row(x_tv)*n_var)] <- 0
    
    ## Get principal components
    eigen_list <- RSpectra::svds(x_tv, k=k)
    f <-eigen_list$u
    
    ## Save mean and std. dev. of the target as attributes of f
    attr(f, "scaled:center") <- target_mean
    attr(f, "scaled:scale") <- target_sd
    
    ## Output 
    return(f)
}
