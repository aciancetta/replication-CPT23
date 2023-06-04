outlier_to_missing <- function(d, n_iqr = 10){
  d |> 
    mutate(across(-1, function(x){ifelse(detect_outlier(x, n_iqr = n_iqr), NA, x)}))
}

