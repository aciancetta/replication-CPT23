## Remark: this function requires initialization of the Python environment
## with reticulate and to load the D2FM.py script and all the necessary libraries.
## See forecast_evaluation.Rmd - D2FM for an example.
d2fm <- function(d, target=NULL, horizon = 1, 
                 epochs = 100,
                 latent_size = 6,
                 hidden_layer_sizes = c(32,16, 8), 
                 decoder_type = "symmetric", 
                 hidden_size_gru = 20, 
                 num_layers_gru = 2, 
                 seq_length = 12,
                 device = torch$device(ifelse(torch$cuda$is_available(), "cuda:0", "cpu"))
                 ) {
  
  ## Scale the data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")
  
  ## Store target indexes
  target_idx <- which(colnames(d) == target)
  
  ## Initialize objects
  x <- reticulate::np_array(d_scaled)
  n_features <- ncol(d_scaled)
  batch_size <- nrow(d_scaled) ## small sample --> no need for mini-batches
  
  ## Initialize the model
  if(typeof(hidden_layer_sizes) != "environment"){
    hidden_layer_sizes <- as.integer(hidden_layer_sizes)
  }
  model <- D2FM(
    n_features = n_features, 
    latent_size = as.integer(latent_size), 
    hidden_layer_sizes = hidden_layer_sizes, 
    decoder_type = decoder_type, 
    hidden_size_gru = as.integer(hidden_size_gru), 
    num_layers_gru = as.integer(num_layers_gru), 
    seq_length = as.integer(seq_length),
    device = device,
    load_params=TRUE) 
  
  ## fit the model
  sys$stdout$flush() # for enabling console printing
  model$fit(x, batch_size = batch_size, epochs = as.integer(epochs), autosave=TRUE)
  
  ## Get forecast (still scaled)
  scaled_forecast <- model$predict_next(horizon = as.integer(horizon))
  
  ## Back-scale to original mean and standard deviation
  y_hat <- scaled_forecast * t(d_sd) + t(d_mean)
  
  ## Output the forecasts
  y_hat
}

  


