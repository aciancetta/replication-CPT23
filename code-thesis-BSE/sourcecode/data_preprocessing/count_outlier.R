detect_outlier <- function(x, n_iqr = 6){
  ## Outlier = observations deviating more than n_iqr inter-quartile ranges
  ## from the mean of the series
  m   <- mean(x, na.rm = T)
  iqr <- IQR(x, na.rm = T)
  thresh <- m + n_iqr*iqr
  abs(x) > thresh
}

count_outlier <- function(d, n_iqr = 8){
  d[,-1] |> 
    mutate(across(everything(), detect_outlier, n_iqr)) |> 
    summarise(across(everything(), sum, na.rm = T)) |> 
    pivot_longer(everything(), values_to = "n_missing") |> 
    # mutate(name = fct_relevel(name, var_names)) |> 
    filter(n_missing > 0) |> 
    ggplot(aes(x = name, y = n_missing)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "Number of outliers") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, size = 7))
}