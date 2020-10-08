fc_forward <- function(object, xreg, freq, fourier_h, h){

  forward <- function(x, y, model, newxreg_in, i, object, freq, fourier_h){
    newrow <- c(y[length(y)], x[nrow(x), 1:(object$maxlag - 1)])
    if(object$maxlag == 1){
      newrow = newrow[-1]
    }
    if(object$seasonal == TRUE & freq > 1)
    {
      newrow <- c(newrow, fourier_h[i, ])
    }
    if(!is.null(newxreg_in)){
      newrow <- c(newrow, newxreg_in)
    }
    newrow <- matrix(newrow, nrow = 1)
    colnames(newrow) <- colnames(x)
    pred <- predict(model, newdata = newrow)
    return(list(
      x = rbind(x, newrow),
      y = c(y, pred)
    ))
  }

  x <- object$x
  y <- object$y2
  for(i in 1:h){
    fc_x <- forward(x = x, y = y, model = object$model, newxreg_in = xreg[i, ], i = i,
                    object = object, freq = freq, fourier_h = fourier_h)
    x <- fc_x$x
    y <- fc_x$y
  }

  y <- ts(y[-(1:length(object$y2))],
          frequency = freq,
          start = max(time(object$y)) + 1 / freq)
  x <- x[-(1:nrow(object$x)),]

  return(list(
    "x" = x,
    "y" = y
  ))

}
