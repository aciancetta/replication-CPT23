library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

cumulative_error_plot <- function(models, model_names, target_v) {
  
  models <- lapply(models, function(model) model$forecast_errors)
  models <- lapply(models, function(model) model[c(c('sasdate'),target_v)])
  
  target_variables <- colnames(models[[1]])[-1]
  
  model_pre_covid <- lapply(models, function(model) model[model$sasdate<'2020-01-01',])
  model_after_covid <- lapply(models, function(model) model[model$sasdate>='2020-01-01',])
  
  scaling_params_pre <- map_dfr(target_variables, ~ {
    target_variable <- .x
    values <- unlist(map(model_pre_covid, ~ .x[[target_variable]]))
    data.frame(
      variable = target_variable,
      mean = mean(values, na.rm = TRUE),
      sd = sd(values, na.rm = TRUE)
    )
  })
  
  scaling_params_after <- map_dfr(target_variables, ~ {
    target_variable <- .x
    values <- unlist(map(model_after_covid, ~ .x[[target_variable]]))
    data.frame(
      variable = target_variable,
      mean = mean(values, na.rm = TRUE),
      sd = sd(values, na.rm = TRUE)
    )
  })

  models <- lapply(models, function(df) {
    df[df$sasdate<'2020-01-01', -1] <- scale(df[df$sasdate<'2020-01-01', -1], center = scaling_params_pre$mean, scale = scaling_params_pre$sd)
    return(df)
  })
  
  models <- lapply(models, function(df) {
    df[df$sasdate>='2020-01-01', -1] <- scale(df[df$sasdate>='2020-01-01', -1], center = scaling_params_after$mean, scale = scaling_params_after$sd)
    return(df)
  })
  
  models <- lapply(models, function(df) {
    #df[, -1] <- abs(df[, -1])
    df[, -1] <- df[, -1]^2
    return(df)
  })
  
  process_model <- function(df, model_name) {
    df %>%
      rowwise() %>%
      mutate(mean_error = mean(c_across(-sasdate), na.rm = TRUE)) %>%
      ungroup() %>% 
      mutate(cumulative_error = cumsum(mean_error)) %>%
      mutate(model = model_name)
  }
  
  models_processed <- mapply(process_model, models, model_names, SIMPLIFY = FALSE)
  
  all_models_processed <- bind_rows(models_processed)
  
  #max_cumulative_error <- max(all_models_processed$cumulative_error, na.rm = TRUE)
  max_cumulative_error <-max(all_models_processed[all_models_processed$model=='pcr_res',]$cumulative_error)
  
  
  all_models_processed <- all_models_processed %>%
    mutate(scaled_cumulative_error = cumulative_error / max_cumulative_error)
  ##########
  
  #########
  recessions.df = data.frame(
    Peak = as.Date(c('2020-02-01')),
    Trough = as.Date(c('2020-04-01')),
    ymin = -Inf,
    ymax = +Inf
  )
  
  
  ##########
  
  ## Setup automatically the colors, linetypes and texts in the plot
  ## based on the best linear and non-linear models
  
  all_models_processed <- all_models_processed |> 
    mutate(
      type = case_when(
        model == "pcr_res" ~ "benchmark",
        model %in% c("lasso", "pcr_lasso", "dfm_res") ~ "linear",
        model %in% c("diffusionmap_res", "pcrquadratic_res", "kernelquadratic_res", 
                     "kernelgauss_res", "tvpcr50_res", "tvpcr_lasso_res", 
                     "tvpcr_ada_lasso_lasso_res", "tvpcr_ridge_res", "rtsne_res", 
                     "lle_res", "isomap_res", "d2fm_res") ~ "nonlinear"))
  
  all_models_processed <- all_models_processed |> 
    slice_max(sasdate) |> group_by(type) |> slice_min(cumulative_error) |> 
    mutate(is_best = TRUE) |> 
    select(model, is_best) |> 
    right_join(all_models_processed) |> 
    mutate(is_best = if_else(is.na(is_best), FALSE, TRUE)) |> 
    mutate(setup = paste0(type, as.character(is_best)),
           alpha = if_else(is_best, 0.9, 0.4))
  
  
 ## Colors
   color_list <- c(
    "benchmarkTRUE" = "darkgrey",
    "linearTRUE" = "black",
    "nonlinearTRUE" = "black",
    "linearFALSE" = "lightgrey",
    "nonlinearFALSE" = "lightgrey")
  
  ## linetypes 
  linetype_list <- c(
    "benchmarkTRUE" = "solid",
    "linearTRUE" = "solid",
    "nonlinearTRUE" = "dashed",
    "linearFALSE" = "solid",
    "nonlinearFALSE" = "dashed")
  
  ## legend text
  model_list <- all_models_processed |> 
    group_by(type) |>
    filter(is_best) |> 
    slice_max(sasdate) |> 
    ungroup() |> 
    mutate(model = case_when(
      model == "pcr_res"                   ~ "PCR",
      model == "lasso"                     ~ "LASSO",
      model == "pcr_lasso"                 ~ "LASSO-PCR",
      model == "diffusionmap_res"          ~ "Diffusion map",
      model == "pcrquadratic_res"          ~ "Squared PCR",
      model == "kernelquadratic_res"       ~ "Quadratic KPCR",
      model == "kernelgauss_res"           ~ "Gaussian KPCR",
      model == "tvpcr50_res"               ~ "TV-PCR 50 components",
      model == "tvpcr_lasso_res"           ~ "LASSO TV-PCR",
      model == "tvpcr_ada_lasso_lasso_res" ~ "AdaLASSO TV-PCR",
      model == "tvpcr_ridge_res"           ~ "Ridge TV-PCR",
      model == "rtsne_res"                 ~ "t-SNE",
      model == "lle_res"                   ~ "LLE",
      model == "isomap_res"                ~ "ISOMAP",
      model == "dfm_res"                   ~ "DFM",
      model == "d2fm_res"                  ~ "D2FM"
    )) |> 
    mutate(legend_name = case_when(
      type == "benchmark"   ~ paste(model, "(benchmark)"),
      type == "linear"   ~ paste(model, "(best linear)"),
      type == "nonlinear"   ~ paste(model, "(best non-linear)"),
    )) |> 
    select(setup, legend_name) |> 
    deframe() |> 
    c("linearFALSE" = "Other linear models", "nonlinearFALSE" = "Other non-linear models") 
  
  
  ## plot  
  all_models_processed |> 
    ggplot(aes(x = sasdate, y = scaled_cumulative_error, 
               group = model,
               color = setup, linetype = setup, alpha = alpha)) +
    geom_line(size = 0.8) +
    annotate(geom = "rect", 
             xmin = as.Date('2020-01-01'), xmax = as.Date('2020-04-01'), 
             ymin = -Inf, ymax = Inf,
             fill = "lightgrey", colour = NA, alpha = 0.5) +
    scale_color_manual(labels = model_list, values = color_list) +
    scale_linetype_manual(labels = model_list, values = linetype_list) +
    scale_alpha_identity() +
    scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0,1.1)) +
    labs(x = "", 
         y = "Cumulative squared errors scaled",
         color = "",
         linetype = "",
         title = paste(target_v)) +
    theme_bw() +
    theme(#panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, margin = margin(b = 5)),
          legend.key.size =  unit(1, "cm"),
          legend.text = element_text(size = 10),
          legend.position = "bottom",
          legend.margin = margin(t = -20),
          legend.spacing.y = unit(-0.5, "cm"),
          axis.title.y = element_text(margin = margin(r = 5))) +
    guides(color = guide_legend(nrow=3,byrow=TRUE),
           linetype = guide_legend(nrow=3, byrow=TRUE))


}