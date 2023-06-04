cumSumPlot_single<-function(model,model_name){
  #scale(model$forecast_errors[,-1])
  cumulative_errors <- apply(scale(model$forecast_errors[,-1])^2, 2, cumsum)
  
  # Create a new dataframe for plotting
  plot_data <- data.frame(date = model$forecast_errors$sasdate, cumulative_errors)
  
  # Convert date column to Date type
  plot_data$date <- as.Date(plot_data$date)
  
  # Reshape the data to long format for ggplot
  plot_data_long <- tidyr::pivot_longer(plot_data, -date, names_to = "Column", values_to = "Cumulative_Sum")
  
  # Plot using ggplot
  ggplot(plot_data_long, aes(x = date, y = Cumulative_Sum, color = Column)) +
    geom_line() +
    labs(x = "Date", y = "Cumulative Sum", color = "Target Variable", title = paste("Cumulative Sum of Forecasted Errors Per Target Variable of", model_name)) +
    theme_minimal()
}