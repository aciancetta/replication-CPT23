count_missing <- function(d){
  d |> 
    summarise(across(everything(), function(x){sum(is.na(x))})) |> 
    pivot_longer(everything(), values_to = "n_missing") |> 
    mutate(name = fct_relevel(name, var_names)) |> 
    filter(n_missing > 0) |> 
    ggplot(aes(x = name, y = n_missing)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "Number of missing observations") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, size = 7))
}
