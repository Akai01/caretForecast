lag_maker <- function(y, max_lag) {
  if("ts" %notin% class(y)){
    stop("y must be a 'ts' object")
  }

  max_lag1 <- round(max_lag)
  if(max_lag1!=max_lag){
    message(paste("'max_lag' should not be a fractional number.",
                  "'max_lag' rounde to", max_lag1, sep = " "))
  }
  length_y <- length(y)
  n_col <- max_lag1 + 1
  dta <- apply(array(seq(from = 1, to = n_col, by = 1)), 1, FUN = function(i){
    y[(max_lag1 + 2 - i):(length_y + 1 - i)]
  }
  )
  colnames(dta) <- c("y", paste0("y_lag", seq(from= 1, to = max_lag1, by = 1)))

  dta <- dta[,-1]

  return(dta)
}
