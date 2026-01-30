#' Autoregressive forecasting using various Machine Learning models.
#'
#' @importFrom methods is
#' @importFrom stats frequency is.ts predict sd start time ts
#' @importFrom forecast is.constant na.interp BoxCox.lambda BoxCox InvBoxCox
#' @importFrom caret train trainControl
#' @param y  A univariate time series object.
#' @param xreg Optional. A numerical vector or matrix of external regressors,
#' which must have the same number of rows as y.
#'  (It should not be a data frame.).
#' @param max_lag Maximum value of lag.
#' @param caret_method A string specifying which classification or
#' regression model to use.
#' Possible values are found using names(getModelInfo()).
#' A list of functions can also be passed for a custom model function.
#' See \url{https://topepo.github.io/caret/} for details.
#' @param metric A string that specifies what summary metric will be used to
#' select the optimal model. See \code{?caret::train}.
#' @param pre_process A string vector that defines a pre-processing of the
#' predictor data.
#' Current possibilities are "BoxCox", "YeoJohnson", "expoTrans", "center",
#' "scale", "range",
#' "knnImpute", "bagImpute", "medianImpute", "pca", "ica" and "spatialSign".
#' The default is no pre-processing.
#' See preProcess and trainControl on the procedures and how to adjust them.
#'  Pre-processing code is only designed to work when x is a simple matrix or
#'   data frame.
#' @param cv Logical, if \code{cv = TRUE} model selection will be done via
#' cross-validation. If \code{cv = FALSE} user need to provide a specific model
#' via \code{tune_grid} argument.
#' @param cv_horizon The number of consecutive values in test set sample.
#' @param initial_window The initial number of consecutive values in each
#' training set sample.
#' @param fixed_window Logical, if FALSE, all training samples start at 1.
#' @param verbose A logical for printing a training log.
#' @param seasonal Boolean. If \code{seasonal = TRUE} the fourier terms will be
#'  used for modeling seasonality.
#' @param K Maximum order(s) of Fourier terms
#' @param tune_grid A data frame with possible tuning values.
#' The columns are named the same as the tuning parameters.
#' Use getModelInfo to get a list of tuning parameters for each model or
#' see \url{https://topepo.github.io/caret/available-models.html}.
#' (NOTE: If given, this argument must be named.)
#' @param lambda BoxCox transformation parameter. If \code{lambda = NULL}
#' If \code{lambda = "auto"}, then the transformation parameter lambda is chosen
#'  using \code{\link[forecast]{BoxCox.lambda}}.
#' @param BoxCox_method \code{\link[forecast]{BoxCox.lambda}} argument.
#' Choose method to be used in calculating lambda.
#' @param BoxCox_lower \code{\link[forecast]{BoxCox.lambda}} argument.
#' Lower limit for possible lambda values.
#' @param BoxCox_upper \code{\link[forecast]{BoxCox.lambda}} argument.
#' Upper limit for possible lambda values.
#' @param BoxCox_biasadj \code{\link[forecast]{InvBoxCox}} argument.
#' Use adjusted back-transformed mean for Box-Cox transformations.
#' If transformed data is used to produce forecasts and fitted values,
#' a regular back transformation will result in median forecasts.
#' If biasadj is TRUE, an adjustment will be made to produce mean
#' forecasts and fitted values.
#' @param BoxCox_fvar \code{\link[forecast]{InvBoxCox}} argument.
#' Optional parameter required if biasadj=TRUE.
#' Can either be the forecast variance, or a list containing the interval level,
#'  and the corresponding upper and lower intervals.
#' @param allow_parallel If a parallel backend is loaded and available,
#' should the function use it?
#' @param calibrate Logical. If TRUE, performs rolling-origin calibration to
#' compute horizon-specific conformal prediction intervals. This produces
#' properly calibrated intervals that widen with forecast horizon (trumpet shape).
#' Default is TRUE.
#' @param calibration_horizon Maximum forecast horizon for calibration.
#' If NULL (default), uses \code{2 * frequency(y)} for seasonal data or 10
#' for non-seasonal data.
#' @param n_cal_windows Number of rolling windows for calibration.
#' If NULL (default), automatically determined based on data length (max 50).
#' @param ... Ignored.
#' @return A list class of forecast containing the following elemets
#' * x : The input time series
#' * method : The name of the forecasting method as a character string
#' * mean : Point forecasts as a time series
#' * lower : Lower limits for prediction intervals
#' * upper : Upper limits for prediction intervals
#' * level : The confidence values associated with the prediction intervals
#' * model : A list containing information about the fitted model
#' * newx : A matrix containing regressors
#' * calibration : Horizon-specific conformal calibration scores (if calibrate=TRUE)
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
#'forecast(fit, h = length(test)) -> fc
#'
#'autoplot(fc) + autolayer(test)
#'
#'accuracy(fc, test)
#'
#' @export

ARml <- function(y,
                 max_lag = 5,
                 xreg = NULL,
                 caret_method = "cubist",
                 metric = "RMSE",
                 pre_process = NULL,
                 cv = TRUE,
                 cv_horizon = 4,
                 initial_window = NULL,
                 fixed_window = FALSE,
                 verbose = TRUE,
                 seasonal = TRUE,
                 K =  frequency(y) / 2,
                 tune_grid = NULL,
                 lambda = NULL,
                 BoxCox_method = c("guerrero", "loglik"),
                 BoxCox_lower = -1,
                 BoxCox_upper = 2,
                 BoxCox_biasadj = FALSE,
                 BoxCox_fvar = NULL,
                 allow_parallel = FALSE,
                 calibrate = TRUE,
                 calibration_horizon = NULL,
                 n_cal_windows = NULL,
                 ...) {

  if ("ts" %notin% class(y)) {
    stop("y must be a univariate time series")
  }
  freq <- stats::frequency(y)
  length_y <- length(y)

  if ((length_y - freq - round(freq / 4)) < max_lag) {
    if(length_y > 3){
      max_lag <- 3
    } else {
      max_lag <- 1
    }
    if(max_lag >= length_y - max_lag - 2){
      max_lag <- 1
    }

    warning(paste("Input data is too short. setting max_lag =  ", max_lag))
  }

  if (length_y < 3) {
    stop("Not enough data to fit a model")
  }
  constant_data <- is.constant(na.interp(y))
  if (constant_data) {
    warning("Constant data, setting max_lag = 1, seasonal = FALSE, lambda = NULL,
            pre_process = NULL")
    pre_process <- NULL
    lambda <- NULL
    max_lag <- 1
  }

  if (!is.null(xreg)) {
    constant_xreg <- any(apply(as.matrix(xreg), 2,
                               function(x) {
                                 is.constant(na.interp(x))
                                 }
                               )
                         )
    if (constant_xreg) {
      warning("Constant xreg column, setting pre_process=NULL")
      pre_process <- NULL
    }
  }

  if (max_lag <= 0) {
    warning("max_lag increased to 1. max_lag must be max_lag >= 1")
    max_lag <- 1
  }


  if (max_lag != round(max_lag)) {
    max_lag <- round(max_lag)
    message(paste("max_lag must be an integer, max_lag rounded to", max_lag))
  }

  if (!is.null(xreg)) {
    if ("matrix" %notin% class(xreg)) {
      xreg <- as.matrix(xreg)
    }
  }

  if (!is.null(xreg))
  {
    ncolxreg <- ncol(xreg)
  }

  if (is.null(lambda)) {
    modified_y <- y
  }
  if (!is.null(lambda)) {
    if (lambda == "auto") {
      lambda  <- forecast::BoxCox.lambda(y,
                                         method = BoxCox_method,
                                         lower = BoxCox_lower,
                                         upper = BoxCox_upper)
      modified_y <- forecast::BoxCox(y, lambda)
    } else if (is.numeric(lambda)) {
      modified_y <- forecast::BoxCox(y, lambda)
    }
  }

  modified_y_2 <- ts(modified_y[-seq_len(max_lag)],
                     start = time(modified_y)[max_lag + 1],
                     frequency = freq)

  if(length_y - max_lag < freq + 1) {
    seasonal <- FALSE
  }

  if (seasonal == TRUE & freq > 1)
  {
    if (K == freq / 2) {
      ncolx <- max_lag + K * 2 - 1
    } else {
      ncolx <- max_lag + K * 2
    }
  }
  if (seasonal == FALSE | freq == 1)
  {
    ncolx <- max_lag
  }

  x <- matrix(0, nrow = c(length_y - max_lag), ncol = ncolx)

  x[, seq_len(max_lag)] <- lag_maker(modified_y, max_lag)

  if (seasonal == TRUE & freq > 1)
  {
    fourier_s <- fourier(modified_y_2, K = K)
    x[, (max_lag + 1):ncolx] <- fourier_s
    colnames(x) <- c(paste0("lag", 1:max_lag), colnames(fourier_s))
  }

  if (seasonal == FALSE | freq == 1)
  {
    colnames(x) <- c(paste0("lag", 1:max_lag))
  }

  xreg_original <- NULL
  if (!is.null(xreg)) {
    xreg_original <- xreg
    col_xreg <- ncol(xreg)
    name_xreg <- colnames(xreg)
    xreg <- xreg[-seq_len(max_lag),]
    if (col_xreg == 1) {
      xreg <- as.matrix(matrix(xreg, ncol = 1))
      colnames(xreg)[1] <- name_xreg[1]
      rm(name_xreg, col_xreg)
    }
    x <- cbind(x, xreg)
  }

  training_method <- "timeslice"

  if (!cv) {
    training_method <- "none"
    if (is.null(tune_grid)) {
      stop("Only one model should be specified in tune_grid with no resampling")
    }
  }

  initial_window_setted <- FALSE
  if(is.null(initial_window)){
    initial_window <- length_y - max_lag -  cv_horizon * 2
    message("initial_window = NULL. Setting initial_window = ", initial_window)
    initial_window_setted <- TRUE
  }
  if(initial_window<1 | initial_window >= nrow(x)){
    initial_window <- length_y - max_lag - 1
    cv_horizon <- 1
    if(initial_window_setted){
      warning("Resetting initial_window = ", initial_window, " cv_horizon = 1")
    } else {
      warning("Setting initial_window = ", initial_window, " cv_horizon = 1")
    }
  }

  model <- caret::train(
    x = x,
    y = as.numeric(modified_y_2),
    method = caret_method,
    preProcess = pre_process,
    weights = NULL,
    metric =  metric,
    trControl = caret::trainControl(
      method = training_method,
      initialWindow = initial_window,
      horizon = cv_horizon,
      fixedWindow = fixed_window,
      verboseIter = verbose,
      allowParallel = allow_parallel,
      returnData = TRUE,
      returnResamp = "final",
      savePredictions = "final"
    ),
    tuneGrid = tune_grid
  )

  fitted <- ts(c(rep(NA, max_lag),
                 predict(model, newdata = x)),
               frequency = freq,
               start = min(time(modified_y)))

  if (!is.null(lambda)) {
    fitted <- forecast::InvBoxCox(fitted,
                                  lambda = lambda,
                                  biasadj = BoxCox_biasadj,
                                  fvar = BoxCox_fvar)
  }

  K_m <- 0

  if (seasonal == TRUE & freq > 1)
  {
    K_m <- K
  }

  method <- paste0("ARml(", max_lag, paste(", ", K_m), ")")

  output <- list(
    y =  y,
    y_modified = modified_y_2,
    x = x,
    model = model,
    fitted = fitted,
    max_lag = max_lag,
    lambda = lambda,
    seasonal = seasonal,
    BoxCox_biasadj = BoxCox_biasadj,
    BoxCox_fvar = BoxCox_fvar,
    method = paste0("caret method ", caret_method, " with ", method)
  )

  if (seasonal == TRUE & freq > 1)
  {
    output$fourier_s <- fourier_s
    output$K <- K
  }
  output$xreg_fit <- NULL
  if (!is.null(xreg)) {
    output$xreg_fit <- xreg
  }

  # Store additional info needed for calibration
  output$caret_method <- caret_method
  output$pre_process <- pre_process

  # Perform horizon-specific calibration for conformal prediction intervals
  output$calibration <- NULL
  if (calibrate) {
    # Set default calibration horizon
    if (is.null(calibration_horizon)) {
      calibration_horizon <- ifelse(freq > 1, 2 * freq, 10)
    }

    # Get the best tuning parameters from the model
    best_tune <- model$bestTune

    if (verbose) {
      message("Performing horizon-specific calibration for conformal prediction intervals...")
    }

    # Need to reconstruct the full modified_y for calibration
    # (before it was truncated by max_lag)
    if (is.null(lambda)) {
      full_modified_y <- y
    } else {
      full_modified_y <- forecast::BoxCox(y, lambda)
    }

    # xreg_original was saved before truncation at line 252

    cal_scores <- tryCatch({
      calibrate_horizon_scores(
        y = y,
        y_modified = full_modified_y,
        max_lag = max_lag,
        caret_method = caret_method,
        seasonal = seasonal,
        K = if (seasonal && freq > 1) K else NULL,
        lambda = lambda,
        pre_process = pre_process,
        tune_grid = best_tune,
        xreg = xreg_original,
        calibration_horizon = calibration_horizon,
        n_windows = n_cal_windows,
        verbose = verbose
      )
    }, error = function(e) {
      if (verbose) {
        warning(paste("Calibration failed:", e$message,
                      "\nFalling back to in-sample residuals."))
      }
      NULL
    })

    if (!is.null(cal_scores)) {
      output$calibration <- conformalRegressorByHorizon(cal_scores)
      output$calibration_horizon <- calibration_horizon
    }
  }

  class(output) <- "ARml"
  return(output)
}
