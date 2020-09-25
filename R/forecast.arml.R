#' @import forecast
#' @title Forecasting an ARml object
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' @param object A list class of ARml
#' @param h forecast horizon
#' @param xreg Optionally, a numerical vector or matrix of future external regressors
#' @param level Confidence level for prediction intervals.
#' @param ... Other arguments pased to forecast()
#' @return A list class of forecast containing the following elemets
#' \item{x}{The input time series}
#' \item{method}{The name of the forecasting method as a character string}
#' \item{mean}{Point forecasts as a time series}
#' \item{lower}{Lower limits for prediction intervals}
#' \item{upper}{Upper limits for prediction intervals}
#' \item{level}{The confidence values associated with the prediction intervals}
#' \item{model}{A list containing information about the fitted model}
#' \item{newx}{A matrix containing regressors}
#' @author Res Akay
#' @note See \code{\link[forecast]{n_rowetar}} and \code{forecastxgb}
#' @examples
#' \dontrun{
#' library(MLforecast)
#' library(forecast)
#' train <- window(AirPassengers, end = c(1959, 12))
#'
#' test <- window(AirPassengers, start = c(1960, 1))
#'
#' ARml(train, caret_method = "svmLinear2", maxlag = 12, trend_method = "none",
#'  pre_process = "center") -> fit
#'
#' forecast(fit, h = length(test), level = NULL, PI = T) -> fc
#' fc_plot(fc)+ autolayer(test)
#' accuracy(fc, test)
#' }
#'
#'
#'@export

forecast.ARml <- function(object,
                          h = frequency(object$y),
                          xreg = NULL, level = c(80, 95), ...){
  if(!is.null(xreg)){
    if(is.null(object$ncolxreg)){
      stop("xreg will be ignored. ARml model trained without xreg")
    }

    if(ncol(xreg) != object$ncolxreg){
      stop("Number of variables in xreg does not mach what you provide to ARml")
    }


    h <- nrow(xreg)
    n_row <- nrow(xreg)
    xreg1 <- xreg[(n_row - h + 1):n_row, ]
  }

  if(is.null(h)){
    h <- ifelse(frequency(object$y) > 1, 2 * frequency(object$y), 10)
  }
  if(is.null(xreg)){
    xreg1 <- NULL
  }

  freq <- frequency(object$y)

  lambda <- object$lambda
  BoxCox_biasadj <- object$BoxCox_biasadj
  BoxCox_fvar <- object$BoxCox_fvar

  htime <- time(ts(rep(0, h), frequency = freq,
                   start = max(time(object$y)) + 1 / freq))
  if(object$seasonal == TRUE & freq > 1)
  {
  fxh <- fourier(object$y2, K = object$K, h = h)
}

  x <- object$x
  y <- object$y2


  for(i in 1:h){
    tmp <- forward(x, y, model = object$model, xregpred = xreg1[i, ], i = i,
                   object = object, freq = freq, fxh = fxh)
    x <- tmp$x
    y <- tmp$y
  }

  y <- ts(y[-(1:length(object$y2))],
          frequency = freq,
          start = max(time(object$y)) + 1 / freq)
if(!is.null(lambda)){
 y <- forecast::InvBoxCox(y, lambda = lambda, biasadj = BoxCox_biasadj,
                     fvar = BoxCox_fvar)
}
  output <- list(
    x = object$y,
    mean = y,
    lower = ts(data.frame("95%" = c(y - 1.96 * sd(y)),
                          "80%" = c(y - 1.28 * sd(y))),  frequency = frequency(object$y),
               start = start(y)),
    upper = ts( data.frame("95%" = c(y + 1.96 * sd(y)),
                           "80%" = c(y + 1.28 * sd(y))), frequency =  frequency(object$y),
                start = start(y)),
    fitted = object$fitted,
    level = level,
    newx = x,
    method = object$method,
    model = object$model
  )
  class(output) <- c("forecast", "forecastARml")
  return(output)

}