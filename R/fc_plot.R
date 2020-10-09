#' @import forecast
#' @title Plot a forecast or forecastARml object
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' @param object a forecast object
#' @param ... other arguments passed to \code{\link[forecast]{autoplot}}
#' @author Resul Akay
#'
#' @examples
#' \dontrun{
#' library(forecast)
#' library(caretForecast)
#' train <- window(AirPassengers, end = c(1959, 12))
#'
#' test <- window(AirPassengers, start = c(1960, 1))
#'
#' ARml(train, caret_method = "svmLinear2", max_lag = 12, trend_method = "none",
#'  pre_process = "center") -> fit
#'
#' forecast(fit, h = length(test), level = NULL, PI = T) -> fc
#' fc_plot(fc)+ autolayer(test)
#' accuracy(fc, test)
#' }
#'
#' @export
fc_plot <- function(object, ...){

forecast::autoplot(object, ...)
}

