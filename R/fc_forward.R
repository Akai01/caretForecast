pred_model <- function(object, xreg, freq, fourier_h, h){

  pred_func <- function(x, y, model, newxreg_in, i, object, freq, fourier_h){
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
    pred <- predict(model, newdata = new_data)
    return(list(
      x = rbind(x, new_data),
      y = c(y, pred)
    ))
  }

  x <- object$x
  y <- object$y2
  for(i in 1:h){
    fc_x <- pred_func(x = x, y = y, model = object$model,
                    newxreg_in = xreg[i, ], i = i,
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
