#' Impute missing values in a panel of time series
#'
#' @description Imputation of missing values follows the procedure described in Mc Cracken and Ng (2015).
#' It is based on the EM algorithm in Stock and Watson (2002):
#' - Scale data
#' - Initialize missing values to zero (i.e. the column mean)
#' - Extract the first `r` principal components
#' - Compute the reconstruction of the dataset based on the `r` principal components
#' - Impute the missing values with their reconstructed values
#' - Repeat PCA and imputation until convergence of the principal components
#' - Back-scale the data and output the result
#'
#'
#' @param d A tibble of time series having the dates in the first column
#' @param r An integer. Number of principal components to use in imputation
#' @param thresh A double defining the threshold for convergence of the algorithm
#'
#' @return A tibble where missing values in `d` have been imputed
#' @export
#'
#' @examples
#' # em_impute(d)
#'
em_impute <- function(d, r = 8, thresh = 0.01, verbose = TRUE) {
  
  # 1) Initialization
  n_var <- ncol(d)-1
  t_max <- nrow(d)
  
  ## 1.a) Scale the data
  d_scaled <- scale(d[,-1])
  means_d <- attr(d_scaled, "scaled:center")
  sds_d   <- attr(d_scaled, "scaled:scale")
  
  ## 1.b) Initialize the missing values to the column mean
  na_idx <- which(is.na(d_scaled))
  d_imputed <- d_scaled
  d_imputed[na_idx] <- 0
  
  ## 1.c) Initialize the matrix of principal components
  pc <- matrix(rep(0, t_max*r), t_max, r)

  # 2) while not converged:
  converged <- FALSE
  while(!converged){
    ## 2.a) Extract the principal components
    pc_old <- pc
    pca_fit <- stats::prcomp(d_imputed, rank. = r)
    loadings <- pca_fit$rotation
    pc <- pca_fit$x
    
    ## 2.b) Impute the missing values based on the reconstruction
    reconstruction <- pc %*% t(loadings)
    d_imputed[na_idx] <- reconstruction[na_idx]
    
    ## 2.c) Check convergence
    distance <- norm(pc - pc_old, type = "F")
    if(verbose){
      message("\rDistance is: ", round(distance/n_var, 4), appendLF = FALSE)
    }
    if(distance/n_var < thresh){
      converged <- TRUE
    }
  }
  
  d_imputed <- d_imputed * rep(sds_d, rep(nrow(d_imputed),length(sds_d))) + rep(means_d, rep(nrow(d_imputed),length(sds_d)))
  
  # Output
  d_imputed_tibble <- d %>%
    tibble::as_tibble() %>%
    dplyr::select(1) %>%
    dplyr::bind_cols(d_imputed)

  d_imputed_tibble
}
