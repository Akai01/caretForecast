lag_maker <- function(y, maxlag) {
  if("ts" %notin% class(y)){
    stop("y must be a 'ts' object")
  }

  max_lag <- round(maxlag)
  if(max_lag!=maxlag){
    message(paste("'maxlag' should not be a fractional number.",
                  "'maxlag' rounde to", max_lag, sep = " "))
  }
  length_y <- length(y)
  n_col <- max_lag + 1
  dta <- apply(array(seq(from = 1, to = n_col, by = 1)), 1, FUN = function(i){
    y[(max_lag + 2 - i):(length_y + 1 - i)]
  }
  )
  colnames(dta) <- c("y", paste0("y_lag", seq(from= 1, to = max_lag, by = 1)))

  dta <- dta[,-1]

  return(dta)
}
