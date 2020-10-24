pred_func <- function(i, x, y, newxreg, object, freq, fourier_h){

  newxreg_in <- newxreg[i,]

  new_data <- c(y[length(y)], x[nrow(x), 1:(object$max_lag - 1)])
  if(object$max_lag == 1){
    new_data = new_data[-1]
  }
  if(object$seasonal == TRUE & freq > 1)
  {
    new_data <- c(new_data, fourier_h[i, ])
  }
  if(!is.null(newxreg_in)){
    new_data <- c(new_data, newxreg_in)
  }
  new_data <- matrix(new_data, nrow = 1)
  colnames(new_data) <- colnames(x)
  pred <- predict(object$model, newdata = new_data)
  return(list(
    "x" = rbind(x, new_data),
    "y" = c(y, pred)
  ))
}

forecast_loop <- function(object, xreg, h){

  x <- object$x
  y <- object$y_modified
  freq <- stats::frequency(object$y_modified)

  if(object$seasonal == TRUE & freq > 1)
  {
    fourier_h <- forecast::fourier(object$y_modified, K = object$K, h = h)
  }


  for(i in 1:h){
    fc_x <- pred_func(i, x = x, y = y,
                      newxreg = xreg,
                      object = object, freq = freq, fourier_h = fourier_h)
    x <- fc_x$x
    y <- fc_x$y
  }

  y <- ts(y[-(1:length(object$y_modified))],
          frequency = freq,
          start = max(time(object$y)) + 1 / freq)
  x <- x[-(1:nrow(object$x)),]

  return(list(
    "x" = x,
    "y" = y
  ))

}
