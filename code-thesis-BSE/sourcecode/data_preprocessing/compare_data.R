#' Compare series from different datasets
#'
#' @param ... Dataframes to compare
#' @description Utility function for comparing a selection of variables from two different datasets. 
#' The variables must have the same name in the two dataset. The first dataset must contain the dates in the first column.
#' Typical use is to compare the data at different steps of the preprocessing.
#'
# compare_data <- function(d1, d2,   
#                          variable_list = c("INDPRO","UNRATE", "S.P.500"),
#                          ribbon_labs = c("With outliers", "Without outliers")
# ) {
#   # Plot selected series from two different versions of fredmd
#   plot_list <- list()
#   d1 <- as.data.frame(d1)
#   d2 <- as.data.frame(d2)
#   for(i in 1:length(variable_list)){
#     plot_list[[i]] <- plot_ts(x = tibble(d1 = d1[,variable_list[i]], 
#                                          d2 = d2[,variable_list[i]]),
#                               date = d1[,1], 
#                               scales = "fixed",
#                               labeller = labeller(variable = c(d1 = ribbon_labs[1], 
#                                                                d2 = ribbon_labs[2]))) +
#                       labs(title = paste("\n", variable_list[i], "\n"))
#   }
#   
#   do.call("grid.arrange", c(plot_list, nrow=ceiling(length(variable_list)/2)))
# }

compare_data <- function(..., 
                         variable_list = c("INDPRO","UNRATE", "S.P.500"),
                         ribbon_labs = c("With outliers", "Without outliers")) {
  
  input_data <- list(...)
  
  # Plot selected series from different versions of data
  plot_list <- list()
  for (i in 1:length(variable_list)) {
    data <- do.call(cbind, lapply(input_data, function(d) as.data.frame(d[, variable_list[i]])))
    names(data) <- paste0("d", 1:length(input_data))
    labs_vec <-  setNames(ribbon_labs, paste0("d", 1:length(input_data)))
    
    plot_list[[i]] <- plot_ts(x = as_tibble(data),
                              date = input_data[[1]][, 1],
                              scales = "free", 
                              labeller = labeller(variable = labs_vec)) + 
      labs(title = paste("\n", variable_list[i], "\n"))
  }
  
  do.call("grid.arrange", c(plot_list, nrow = ceiling(length(variable_list)/2)))
}



