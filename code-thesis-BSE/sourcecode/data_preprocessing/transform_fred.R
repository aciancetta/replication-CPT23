transform_fred <- function(data, tcode = tcode, verbose = TRUE){

  lag1      <- function(x){x <- c(NA, x[1:length(x)-1]);x}
  diff      <- function(x){x <- x - lag1(x);x}
  diff2     <- function(x){x <- diff(diff(x));x}
  logd      <- function(x){x <- log(x);x}
  logdiff   <- function(x){x <- log(x) - log(lag1(x));x}
  logdiff2  <- function(x){x <- diff(logdiff(x));x}
  diffp     <- function(x){x <- diff(x/lag1(x) - 1);x}
  
  tfuns <- c(identity, diff, diff2, logd, logdiff, logdiff2, diffp)
  tfuns_labels <- c("Identity", "Diff", "Diff2", "log", "logDiff", "logDiff2", "diffp")
  transformed <- as.data.frame(data)
  
  for(j in 2:ncol(transformed)){
    trcode <- as.integer(tcode[j-1])
    transformed[,j] <- tfuns[[trcode]](transformed[,j])
    
    if(verbose){
      message("Column ", j, ", ", names(data)[j], ". Transformation ", trcode, " (", tfuns_labels[trcode], ")")
    }
  }
  
  as_tibble(transformed)
  
}
