#' Fit a conformal regressor.
#'
#' @param residuals Model residuals.
#' @param sigmas A vector of difficulty estimates
#' @author Resul Akay
#' @return A conformalRegressor object
#'
#' @references
#' Boström, H., 2022. crepes: a Python Package for Generating Conformal
#' Regressors and Predictive Systems. In Conformal and Probabilistic Prediction
#' and Applications. PMLR, 179.
#' \url{https://copa-conference.com/papers/COPA2022_paper_11.pdf}
#'
#' @export
conformalRegressor <- function(residuals, sigmas=NULL) {
  abs_residuals <- abs(residuals)
  if(is.null(sigmas)){
    normalized <- FALSE
    alphas <- rev(sort(abs_residuals))
  } else {
    normalized <- TRUE
    alphas = rev(sort(abs_residuals/sigmas))
  }
  out <- list(
    alphas = alphas,
    normalized = normalized
  )
  out <- structure(out, class = "conformalRegressor")
  return(out)
}
