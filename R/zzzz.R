
lag_maker <- function(x, maxlag, keeporig = TRUE){
  if(!is.vector(x) & !is.ts(x)){
    stop("x must be a vector or time series")
  }
  x <- as.vector(x)
  n <- length(x)
  z <- matrix(0, nrow = (n - maxlag), ncol = maxlag + 1)
  for(i in 1:ncol(z)){
    z[ , i] <- x[(maxlag + 2 - i):(n + 1 - i)]
  }
  col_name <- "x"
  colnames(z) <- c(col_name, paste0(col_name, "_lag", 1:maxlag))
  if(!keeporig){
    z <- z[ ,-1]
  }
  return(z)
}


forward <- function(x, y, model, xregpred, i, object, freq, fxh){
  newrow <- c(
    y[length(y)],
    x[nrow(x), 1:(object$maxlag - 1)])
  if(object$maxlag == 1){
    newrow = newrow[-1]
  }
  if(object$seasonal == TRUE & freq > 1)
  {
    newrow <- c(newrow, fxh[i, ])
  }
  if(!is.null(xregpred)){
    newrow <- c(newrow, xregpred)
  }
  newrow <- matrix(newrow, nrow = 1)
  colnames(newrow) <- colnames(x)
  pred <- predict(model, newdata = newrow)
  return(list(
    x = rbind(x, newrow),
    y = c(y, pred)
  ))
}
