## Function that specifies number of required factors (r) and imputes 
## missing values following Stock and Watson (2002)

extract_and_fill <- function(d, r){
  Z0 <- as.matrix(d[,-1])
  Xs <- scale(Z0, center = TRUE, scale = TRUE)
  mu <- colMeans(Z0, na.rm = T)
  st_dev <- apply(as.matrix(Z0), 2, sd, na.rm = T)
  Xs[is.na(Xs)] <- 0
  X <- Xs
  t <- nrow(X)
  n <- ncol(X)
  converged <- F
  counter <- 1
  lhat <- matrix(0, n, r) 
  fhat <- matrix(0, t, r)
  while(!converged){
    eigenvv <- eigen((t(X)%*%X)/t)
    lhat <- eigenvv$vectors[,1:r]
    fhat <- (X%*%lhat)/n
    Znew <- (fhat%*%t(lhat)) %*% diag(st_dev) + mu[col(Xs)]
    Z <- Z0
    Z[is.na(Z0)] <- Znew[is.na(Z0)]
    Xold <- X
    X <- scale(Z, center = TRUE, scale = TRUE)
    mu <- colMeans(Z, na.rm = T)
    st_dev <- apply(as.matrix(Z), 2, sd, na.rm = T)
    converged <- norm(Xold - X, type = "2") < 1e-08
    if(converged == T & counter == 1){
      message("No missing values, only extracting factors")
    } else {
      message("Iteration ", counter, ". The norm is ", norm(Xold - X))
    }
    counter <- counter+1
  }
  eigenvv <- eigen((t(X)%*%X)/t)
  lhat <- eigenvv$vectors[,1:r]*sqrt(n) 
  fhat <- (X%*%lhat)/n
  Znew <- (fhat%*%t(lhat)) %*% diag(st_dev) + mu[col(Xs)]
  Z <- Z0
  Z[is.na(Z0)] <- Znew[is.na(Z0)] #this is the original dataset where missing data have been imputed
  eigenvalues <- eigenvv$values
  fhat <- as.data.frame(fhat)
  names(fhat) <- paste0("F",1:r)
  list(imputed = d[,1] |> bind_cols(as_tibble(Z)), 
       scaled = X, 
       factors = fhat, 
       loadings = lhat, 
       eigenvalues = eigenvalues)
} 
