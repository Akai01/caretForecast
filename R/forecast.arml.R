#' @import forecast
#' @title Forecasting an ARml object
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' @param object A list class of ARml
#' @param h forecast horizon
#' @param xreg Optionally, a numerical vector or matrix of future external
#' regressors
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
#' \item{newxreg}{A matrix containing regressors}
#' @author Resul Akay
#' @note See \code{\link[forecast]{nnetar}}
#' @examples
#' \dontrun{
#'library(forecast)
#'
#'library(caretForecast)
#'
#'train_data <- window(AirPassengers, end = c(1959, 12))
#'
#'test <- window(AirPassengers, start = c(1960, 1))
#'
#'ARml(train_data, caret_method = "svmLinear2", max_lag = 12) -> fit
#'
#'forecast(fit, h = length(test), level = NULL, PI = T) -> fc
#'
#'fc_plot(fc)+ autolayer(test)
#'
#'accuracy(fc, test)
#' }
#'
#'
#'@export

forecast.ARml <- function(object,
                          h = frequency(object$y),
                          xreg = NULL, level = c(80, 95), ...){

  if(!is.null(object$xreg_fit)){
  ncolxreg <- ncol(object$xreg_fit)
  }

  if(is.null(xreg)){
    if(!is.null(object$xreg_fit)){
      stop("No regressors provided")
    }
  }

  if(!is.null(xreg)){
    if(is.null(object$xreg_fit)){
      stop("No regressors provided to fitted model")
    }

    if(ncol(xreg) != ncolxreg){
      stop("Number of regressors does not match to fitted model")
    }

    h <- nrow(xreg)
    newxreg1 <- xreg
  }

  if(is.null(h)){
    h <- ifelse(frequency(object$y) > 1, 2 * frequency(object$y), 10)
  }
  if(is.null(xreg)){
    newxreg1 <- NULL
  }

  lambda <- object$lambda
  BoxCox_biasadj <- object$BoxCox_biasadj
  BoxCox_fvar <- object$BoxCox_fvar

  fc_x <- forecast_loop(object = object, xreg = newxreg1, h = h)
    x <- fc_x$x
    y <- fc_x$y


if(!is.null(lambda)){
 y <- forecast::InvBoxCox(y, lambda = lambda, biasadj = BoxCox_biasadj,
                     fvar = BoxCox_fvar)
}
  output <- list(
    x = object$y,
    mean = y,
    lower = ts(data.frame("95%" = c(y - 1.96 * sd(y)),
                          "80%" = c(y - 1.28 * sd(y))),
               frequency = frequency(object$y),
               start = start(y)),
    upper = ts( data.frame("95%" = c(y + 1.96 * sd(y)),
                           "80%" = c(y + 1.28 * sd(y))),
                frequency =  frequency(object$y),
                start = start(y)),
    fitted = object$fitted,
    level = level,
    newxreg = x,
    method = object$method,
    model = object$model
  )
  class(output) <- c("forecast", "forecastARml")
  return(output)

}
