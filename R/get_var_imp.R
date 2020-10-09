#' @import caret
#' @title Calculation of variable importance for forecasting model.
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' @param object A list class of ARml or forecast object derived from ARml
#' @param plot Boolean, if TRUE, variable importance will be ploted.
#' @return A list class of "varImp.train". See \code{\link[caret]{varImp}}
#' @author Resul Akay
#' @examples
#' \dontrun{
#' library(MLforecast)
#' library(forecast)
#'
#' train <- window(AirPassengers, end = c(1959, 12))
#'
#' test <- window(AirPassengers, start = c(1960, 1))
#'
#' ARml(train, caret_method = "svmLinear2", max_lag = 12, trend_method = "none",
#'  pre_process = "center") -> fit
#'
#' forecast(fit, h = length(test), level = NULL, PI = T) -> fc
#'
#' plot(fc)+ autolayer(test)
#'
#' accuracy(fc, test)
#'
#' get_var_imp(fc, plot = T)
#' }
#'

#'
#' @export

get_var_imp <- function(object, plot = T){

  if(!is(object, c("ARml","forecastARml"))){
    stop("object must be an forecastARml or ARml object")
  }


  if(plot){
    return(plot(varImp(object$model)))
  }

  if(!plot){
    return(varImp(object$model))
  }

}
