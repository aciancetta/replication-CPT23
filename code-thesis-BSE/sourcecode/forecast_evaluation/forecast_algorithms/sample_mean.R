sample_mean <- function(d, target, horizon = 1) {
  
  target_idx <- which(colnames(d) == target)
  y_hat <- mean(as.matrix(d)[,target_idx])
  y_hat#*0.9
}



