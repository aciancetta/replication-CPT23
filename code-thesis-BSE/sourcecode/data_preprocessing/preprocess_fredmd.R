preprocess_fredmd <- function(fredmd, r = 8, thresh = 0.01, verbose = TRUE, 
                              impute = TRUE){
  ## extract data and transformation codes
  d <- as_tibble(fredmd[-1,])
  tcodes <- unlist(fredmd[1,-1])
  
  ## execute the preprocessing pipeline
  d_stationary <- transform_fred(d, tcode = tcodes, verbose = verbose)
  d_outlier <- outlier_to_missing(d_stationary)
  if(impute){
  # d_imputed <- extract_and_fill(d_outlier, r = 8)$imputed
    d_imputed <- em_impute(d_outlier, r, thresh, verbose)
  } else {
    d_imputed <- d_outlier[rowSums(!is.na(d_outlier[ , -1])) > 0, ]
  }
  
  ## output
  return(d_imputed)
}