#' @importFrom generics forecast
#' @export
generics::forecast

#' @title Forecasting using ARml model
#'
#' @param object An object of class "ARml", the result of a call to ARml.
#' @param h forecast horizon
#' @param xreg Optionally, a numerical vector or matrix of future external
#' regressors
#' @param level Confidence level for prediction intervals.
#'
#' @param ... Ignored
#' @return A list class of forecast containing the following elemets
#' * x : The input time series
#' * method : The name of the forecasting method as a character string
#' * mean : Point forecasts as a time series
#' * lower : Lower limits for prediction intervals
#' * upper : Upper limits for prediction intervals
#' * level : The confidence values associated with the prediction intervals
#' * model : A list containing information about the fitted model
#' * newxreg : A matrix containing regressors
#' @author Resul Akay
#'
#' @examples
#'
#'library(caretForecast)
#'
#'train_data <- window(AirPassengers, end = c(1959, 12))
#'
#'test <- window(AirPassengers, start = c(1960, 1))
#'
#'ARml(train_data, caret_method = "lm", max_lag = 12) -> fit
#'
#'forecast(fit, h = length(test), level = c(80,95)) -> fc
#'
#'autoplot(fc)+ autolayer(test)
#'
#'accuracy(fc, test)
#' @importFrom stats residuals tsp tsp<-
#' @export
forecast.ARml <- function(object,
                          h = frequency(object$y),
                          xreg = NULL,
                          level = c(80, 95),
                          ...) {

  if (!is.null(object$xreg_fit)) {
    ncolxreg <- ncol(object$xreg_fit)
  }

  if (is.null(xreg)) {
    if (!is.null(object$xreg_fit)) {
      stop("No regressors provided")
    }
  }

  if (!is.null(xreg)) {
    if (is.null(object$xreg_fit)) {
      stop("No regressors provided to fitted model")
    }

    if (ncol(xreg) != ncolxreg) {
      stop("Number of regressors does not match to fitted model")
    }

    h <- nrow(xreg)
    newxreg1 <- xreg
  }

  if (is.null(h)) {
    h <- ifelse(frequency(object$y) > 1, 2 * frequency(object$y), 10)
  }

  if (is.null(xreg)) {
    newxreg1 <- NULL
  }

  lambda <- object$lambda
  BoxCox_biasadj <- object$BoxCox_biasadj
  BoxCox_fvar <- object$BoxCox_fvar

  fc_x <- forecast_loop(object = object, xreg = newxreg1, h = h)
  x <- fc_x$x
  y <- fc_x$y

  if (!is.null(lambda)) {
    y <- forecast::InvBoxCox(y, lambda = lambda, biasadj = BoxCox_biasadj,
                             fvar = BoxCox_fvar)
  }

  res <- tryCatch({
    residuals(object)
  }, error = function(err){
    out <- NULL
    return(out)
  })

  if(is.null(res)){
    lower <- NULL
    upper <- NULL
  } else {
    intervals <- tryCatch({
      conformal_intervals(residuals = res, y_hat = y, level = level)
    }, error =  function(err) {
      NULL
    })
    if(is.null(intervals)){
      lower <- NULL
      upper <- NULL
      warning("I could not derive conformal prediction intervals")
    } else {
      lower <- intervals[["lower"]]
      upper<- intervals[["upper"]]
    }
  }

  output <- list(
    x = object$y,
    mean = y,
    lower = lower,
    upper = upper,
    fitted = object$fitted,
    level = level,
    newxreg = x,
    method = object$method,
    model = object$model
  )
  class(output) <- c("forecast", "forecastARml")
  return(output)
}
