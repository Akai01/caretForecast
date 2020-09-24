#' @import forecast
#' @title Plot a ts object
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' @param object a ts object
#' @param ... other arguments pased \code{autoplot}
#' @author Resul Akay
#' @examples
#' \dontrun{
#' ts_plot(AirPassengers)
#' }
#'
#'
#' @export
ts_plot <- function(object, ...){

  forecast::autoplot(object, ...)
}
