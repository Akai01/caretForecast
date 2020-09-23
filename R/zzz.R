
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
  varname <- "x"
  colnames(z) <- c(varname, paste0(varname, "_lag", 1:maxlag))
  if(!keeporig){
    z <- z[ ,-1]
  }
  return(z)
}


lag_maker_forecast <- function(x, maxlag){
  if(!is.matrix(x)){
    stop("X needs to be a matrix")
  }

  if(is.null(colnames(x))){
    colnames(x) <- paste0("Var", 1:ncol(x))
  }
  n <- nrow(x)
  M <- matrix(0, nrow = (n - maxlag), ncol = (maxlag + 1) * ncol(x))
  for(i in 1:ncol(x)){
    M[ , 1:(maxlag + 1) + (i - 1) * (maxlag + 1)] <- lag_maker(x[ ,i], maxlag = maxlag)
  }
  thenames <- character()
  for(i in 1:ncol(x)){
    thenames <- c(thenames, paste0(colnames(x)[i], "_lag", 0:maxlag))
  }
  colnames(M) <- thenames

  return(M)
}
