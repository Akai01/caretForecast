#' @import forecast
#' @title Plot a forecast or forecastARml object
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' @param object a forecast object
#' @param ... other arguments passed to \code{\link[forecast]{autoplot}}
#' @author Res Akay
#'
#' @export
fc_plot <- function(object, ...){

forecast::autoplot(object, ...)
}

