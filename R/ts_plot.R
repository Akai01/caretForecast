#' @import forecast
#' @title Plot a ts object
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' @param object a ts object
#' @param ... other arguments
#' @author Res Akay
#'
#'
#' @export
ts_plot <- function(object, ...){

  forecast::autoplot(object, ...)
}
