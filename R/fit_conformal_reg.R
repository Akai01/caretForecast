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

#' Fit a horizon-specific conformal regressor for time series forecasting.
#'
#' This function creates a conformal regressor that accounts for increasing
#' uncertainty at longer forecast horizons. It uses separate nonconformity
#' score distributions for each horizon h=1,2,3,..., resulting in prediction
#' intervals that naturally widen as the forecast horizon increases
#' (trumpet-shaped intervals).
#'
#' @param horizon_errors A named list where each element contains sorted
#'   absolute errors for that horizon. Names should be "h1", "h2", etc.
#'   This is typically produced by \code{calibrate_horizon_scores()}.
#' @author Resul Akay
#' @return A conformalRegressorByHorizon object containing:
#'   \item{alphas_by_horizon}{List of sorted nonconformity scores for each horizon}
#'   \item{max_horizon}{Maximum calibrated horizon}
#'   \item{n_samples}{Number of calibration samples per horizon}
#'
#' @references
#' Boström, H., 2022. crepes: a Python Package for Generating Conformal
#' Regressors and Predictive Systems. In Conformal and Probabilistic Prediction
#' and Applications. PMLR, 179.
#' \url{https://copa-conference.com/papers/COPA2022_paper_11.pdf}
#'
#' Stankeviciute, K., Alaa, A. M., & van der Schaar, M., 2021.
#' Conformal Time-series Forecasting. NeurIPS 2021.
#'
#' @export
conformalRegressorByHorizon <- function(horizon_errors) {

  if (!is.list(horizon_errors)) {
    stop("horizon_errors must be a list of error vectors indexed by horizon")
  }

  max_horizon <- length(horizon_errors)

  if (max_horizon < 1) {
    stop("horizon_errors must contain at least one horizon")
  }

  # Ensure errors are sorted in descending order (largest first) for quantile lookup
  alphas_by_horizon <- lapply(horizon_errors, function(x) rev(sort(x)))

  n_samples <- sapply(alphas_by_horizon, length)

  out <- list(
    alphas_by_horizon = alphas_by_horizon,
    max_horizon = max_horizon,
    n_samples = n_samples
  )

  out <- structure(out, class = "conformalRegressorByHorizon")
  return(out)
}
