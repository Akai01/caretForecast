#' @title Variable importance for forecasting model.
#'
#' @param object A list class of ARml or forecast object derived from ARml
#' @param plot Boolean, if TRUE, variable importance will be ploted.
#' @return A list class of "varImp.train". See \code{\link[caret]{varImp}} or a
#' "trellis" plot.
#' @author Resul Akay
#' @examples
#'
#' train <- window(AirPassengers, end = c(1959, 12))
#'
#' test <- window(AirPassengers, start = c(1960, 1))
#'
#' ARml(train, caret_method = "lm", max_lag = 12, trend_method = "none",
#'  pre_process = "center") -> fit
#'
#' forecast(fit, h = length(test), level = c(80,95), PI = TRUE) -> fc
#'
#' autoplot(fc)+ autolayer(test)
#'
#' accuracy(fc, test)
#'
#' get_var_imp(fc, plot = TRUE)
#'
#'
#' @export

get_var_imp <- function(object, plot = TRUE) {
  if ("forecastARml" %notin% class(object)) {
    stop("object must be an forecastARml or ARml object")
  }
  if (plot) {
    return(plot(varImp(object$model)))
  }
  if (!plot) {
    return(varImp(object$model))
  }
}
