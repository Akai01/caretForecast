predict_default <- function(object, y_hat, sigmas, confidence, y_min, y_max){

  if(length(confidence) >1){
    confidence <- confidence[1]
    warning("Only first element of confidence considered")
  }

  if(!all(c(confidence <=1 & confidence>=0))){
    stop("confidence must be in the interval '0<=confidence<=1' ")
  }

  intervals <- matrix(nrow = length(y_hat), ncol = 2)
  alpha_index <- ceiling((1-confidence) * (length(object[["alphas"]]) + 1))
  if(alpha_index > 0 && alpha_index <= length(object[["alphas"]])) {
    alpha <- object[["alphas"]][alpha_index]
    if(object[["normalized"]]){
      intervals[,1] <- y_hat-alpha*sigmas
      intervals[,2] <- y_hat+alpha*sigmas
    } else {
      intervals[,1] <- y_hat-alpha
      intervals[,2] <- y_hat+alpha
    }
  } else {
    intervals[,1] <- -Inf
    intervals[,2] <- Inf
  }
  if(y_min > - Inf) {
    intervals[intervals<y_min] = y_min
  }
  if(y_max < Inf){
    intervals[intervals>y_max] = y_max
  }
  return(intervals)
}

#' Predict a conformalRegressor
#' @param object A conformalRegressor object
#' @param y_hat Predicted values
#' @param sigmas Difficulty estimates
#' @param confidence Confidence level
#' @param y_min The minimum value to include in prediction intervals
#' @param y_max The maximum value to include in prediction intervals
#' @param ... Ignored
#' @author Resul Akay
#' @return Prediction intervals
#' @importFrom stats predict
#' @importFrom dplyr bind_cols
#' @export
predict.conformalRegressor <- function(object, y_hat = NULL, sigmas = NULL,
                                       confidence = 0.95, y_min = - Inf,
                                       y_max = Inf, ...){
  if(!is.null(y_hat)){
    if(!is.numeric(y_hat)){
      stop("y_hat must be a numeric vector")
    }
  }

  if(!is.null(sigmas)){
    if(!is.numeric(sigmas)){
      stop("sigmas must be a numeric vector")
    }
  }

  if(length(y_min)>1){
    warning("Only the first element of y_min considered")
    y_min <- y_min[1]
  }

  if(!is.numeric(y_min)){
    stop("y_min must be a numeric vector")
  }

  if(length(y_max)>1){
    warning("Only the first element of y_max considered")
    y_max <- y_max[1]
  }

  if(!is.numeric(y_max)){
    stop("y_max must be a numeric vector")
  }

  intervals <- sapply(X = confidence,
                      FUN = function(x, .object, .y_hat, .sigmas, .y_min,
                                     .y_max) {

                        pred <- predict_default(object = .object,
                                                y_hat = .y_hat,
                                                sigmas = .sigmas,
                                                confidence = x,
                                                y_min = .y_min,
                                                y_max = .y_max)
                        pred <- as.data.frame(pred)
                        colnames(pred) <- paste0(c("lower_", "upper_"), x*100)
                        return(pred)
                      },
                      .object = object,
                      .y_hat = y_hat,
                      .sigmas = sigmas,
                      .y_min = y_min,
                      .y_max = y_max,
                      simplify = FALSE
  )
  intervals <- dplyr::bind_cols(intervals)
  return(intervals)
}

#' Predict intervals from a horizon-specific conformal regressor
#'
#' This function generates prediction intervals that account for increasing
#' uncertainty at longer forecast horizons. Each horizon h uses its own
#' calibrated nonconformity score distribution, resulting in trumpet-shaped
#' prediction intervals.
#'
#' @param object A conformalRegressorByHorizon object
#' @param y_hat Predicted values (one per horizon)
#' @param confidence Confidence level(s) between 0 and 1 (e.g., 0.95 for 95\%)
#' @param y_min The minimum value to include in prediction intervals
#' @param y_max The maximum value to include in prediction intervals
#' @param ... Ignored
#' @author Resul Akay
#' @return A data frame with lower and upper bounds for each confidence level
#' @importFrom stats predict
#' @importFrom dplyr bind_cols
#' @export
predict.conformalRegressorByHorizon <- function(object, y_hat = NULL,
                                                 confidence = 0.95,
                                                 y_min = -Inf,
                                                 y_max = Inf, ...) {

  if (is.null(y_hat)) {
    stop("y_hat must be provided")
  }

  if (!is.numeric(y_hat)) {
    stop("y_hat must be a numeric vector")
  }

  h <- length(y_hat)

  if (h > object$max_horizon) {
    warning(sprintf(
      "Forecast horizon (%d) exceeds calibrated horizon (%d). Using max calibrated horizon for h > %d.",
      h, object$max_horizon, object$max_horizon
    ))
  }

  if (length(y_min) > 1) {
    warning("Only the first element of y_min considered")
    y_min <- y_min[1]
  }

  if (length(y_max) > 1) {
    warning("Only the first element of y_max considered")
    y_max <- y_max[1]
  }

  # Generate intervals for each confidence level
  intervals_list <- lapply(confidence, function(conf) {

    if (conf < 0 || conf > 1) {
      stop("confidence must be in the interval [0, 1]")
    }

    lower <- numeric(h)
    upper <- numeric(h)

    for (i in 1:h) {
      # Use the appropriate horizon's alphas, or the max available
      horizon_idx <- min(i, object$max_horizon)
      alphas <- object$alphas_by_horizon[[horizon_idx]]
      n_alphas <- length(alphas)

      # Calculate alpha index for this confidence level
      alpha_index <- ceiling((1 - conf) * (n_alphas + 1))

      if (alpha_index > 0 && alpha_index <= n_alphas) {
        alpha <- alphas[alpha_index]
        lower[i] <- y_hat[i] - alpha
        upper[i] <- y_hat[i] + alpha
      } else {
        # Not enough calibration samples for this confidence level
        lower[i] <- -Inf
        upper[i] <- Inf
      }
    }

    # Apply bounds
    if (y_min > -Inf) {
      lower[lower < y_min] <- y_min
    }
    if (y_max < Inf) {
      upper[upper > y_max] <- y_max
    }

    result <- data.frame(lower = lower, upper = upper)
    colnames(result) <- paste0(c("lower_", "upper_"), conf * 100)
    return(result)
  })

  intervals <- dplyr::bind_cols(intervals_list)
  return(intervals)
}
