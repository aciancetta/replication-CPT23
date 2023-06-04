get_covid_data <- function(model, date='2020-01-01') {
  covid_date=as.Date(date)
  model$forecast_errors=model$forecast_errors[model$forecast_errors$sasdate<covid_date,]
  model$forecasts=model$forecasts[model$forecasts$sasdate<covid_date,]
  model$ground_truth$sasdate <- as.Date(model$ground_truth$sasdate, format = "%m/%d/%Y")
  model$ground_truth=model$ground_truth[model$ground_truth$sasdate<covid_date,]
  return(model)
}