plot_ts <- function(x, date = 1:nrow(x), title = "", n_row = 3, dir = "v", scales = "free", ...){
  x <- data.frame(x)
  date <- if(is(date, "tbl")) pull(date) else date
  data <- tibble(date = date, x) %>% 
    pivot_longer(-date, names_to = "variable", values_to = "value") %>% 
    mutate(factor = fct_relevel(variable, names(x)),
           date = lubridate::mdy(date))
  
  plot_ts <- data %>% 
    ggplot(aes(x = date, y = value)) + 
    geom_line() +
    facet_wrap(vars(variable), nrow = n_row, dir = dir, scales = scales, ...) +
    labs(title = title,
         y = "", x= "") +
    theme_minimal() +
    theme(strip.background = element_rect(fill = "#DDDDE3", color = "white"),
          legend.title = element_text(size = 11), 
          legend.text = element_text(size = 11),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 11),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 11))
  
  plot_ts
}
