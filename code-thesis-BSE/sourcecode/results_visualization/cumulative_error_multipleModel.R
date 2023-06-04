cumSumPlot_multi <- function(models, model_names) {
  plot_data <- data.frame()
  
  for (i in 1:length(models)) {
    model <- models[[i]]
    model_name <- model_names[i]
    
    standardized_data <- scale(model$forecast_error[, -1])
    
    cumulative_errors <- rowSums(standardized_data^2)
    cumulative_sum <- cumsum(cumulative_errors)
    
    model_data <- data.frame(date = model$forecast_error$sasdate, cumulative_sum)
    model_data$date <- as.Date(model_data$date)
    model_data$model_name <- model_name
    
    # Append the data to plot_data
    plot_data <- rbind(plot_data, model_data)
    
  }
  
  # Plot using ggplot
  ggplot(plot_data, aes(x = date, y = cumulative_sum, color = model_name)) +
    geom_line(aes(group = model_name)) +  # Add 'group' aesthetic to separate lines
    labs(x = "Date", y = "Cumulative Sum", color = "Model",
         title = "Cumulative Sum of Errors per Model") +
    theme_minimal()
}