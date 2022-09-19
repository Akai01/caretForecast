lag_maker_fable <- function(y, lags) {
  if ("ts" %notin% class(y)) {
    stop("y must be a 'ts' object")
  }
  lags <- as.integer(lags)
  y <- c(y)
  out <- matrix(nrow = length(y), ncol = length(lags))
  for (i in seq_len(length(lags))) {
    out[, i] <- dplyr::lag(y, lags[i])
  }
  out <- as.matrix(na.omit(out))
  colnames(out) <- paste0("y_lag", lags)
  return(out)
}

prepare_data_fable <- function(y,
                               p,
                               P = 1,
                               xreg = NULL){
  if ("ts" %notin% class(y)) {
    stop("y must be a univariate time series")
  }
  length_y <- length(y)
  freq <- stats::frequency(y)
  if(missing(p)){
    p <- 1
  }
  while (!length_y >= freq * P + 2) {
    P <- P - 1
  }
  if(P > 0){
    lags <- sort(unique(c(1:p, freq * (1:P))))
  } else {
    lags <- 1:p
  }
  xlags <- lag_maker(y, lags = lags)
  modified_y <- ts(y[-seq_len(max(lags))], start = time(y)[max(lags) + 1], frequency = freq)
  if (!is.null(xreg)) {
    xlags <- cbind(xlags, xreg)
  }
  out <- structure(list("x" = xlags, "y" = modified_y,  "frequency" = freq,
                        "lags" = lags, "p" = p, "P" = P, "max_lag" = max(lags)),
                   class = "caret_ts_data")
  return(out)
}

`%notin%` <- Negate(`%in%`)

#' Forecast a CARET model
#' @param object The time series model used to produce the forecasts
#' @param new_data A tsibble containing future information used to forecast.
#' @param specials Model specials, passed by fabletools::forecast.mdl_df().
#' @param ... Additional arguments for forecast model methods.
#'
#' @importFrom stats predict residuals
#' @importFrom fabletools forecast
#' @importFrom distributional dist_normal
#'
#' @export
forecast.CARET <- function(object,
                          h = NULL,
                          xreg = NULL,
                          level = c(80, 95),
                          PI = FALSE,
                          num_bs = 1000,
                          ...) {
  if(is.null(h)){
    h <- ifelse(frequency(object[["y"]]) > 1, 2 * frequency(object[["y"]]), 6)
  }

  if(is.numeric(h)){
    h <- as.integer(h)
  } else {
    stop("h must be an integer")
  }

  if (is.null(xreg)) {
    if (object[["xreg"]]) {
      stop("No regressors provided")
    }
  }

  if (!is.null(xreg)) {
    if (object[["xreg"]]) {
      warning("No regressors provided to fitted model. xreg will be ignored")
      xreg <- NULL
    }

    if(nrow(xreg) != h){
      h <- nrow(xreg)
    }
  }

  if (PI) {
    if (is.null(level)) {
      warning("level was not provided. Prediction intervals will not be returned")
      PI <- FALSE
    }
  }

  fcast <- numeric(h)
  xx <- object$x
  xxreg <- xreg
  lags <- object$lags
  maxlag <- max(lags)
  flag <- rev(tail(xx, n = maxlag))

  for (i in 1:h) {
    newdata <- c(flag[lags], xxreg[i, ])
    if (any(is.na(newdata))) {
      stop("I can't forecast when there are missing values near the end of the series.")
    }
    newdata <- data.frame(matrix(newdata, nrow = 1))
    colnames(newdata) <- colnames(xx)
    fcast[i] <- predict(object[["model"]], newdata = newdata)
    flag <- c(fcast[i], flag[-maxlag])
  }
  return(fcast)
  output <- list(
    x = object$y,
    mean = fcast,
    lower = NULL,
    upper = NULL,
    fitted = object$fitted,
    level = level,
    newxreg = x,
    method = object$method,
    model = object$model
  )

  class(output) <- c("forecast", "forecastARml")
  return(output)
}

train_caret_fable <- function(y,
                              p,
                              P = 1,
                              xreg = NULL,
                              caret_method = "cubist",
                              tune_grid = NULL,
                              metric = "RMSE",
                              pre_process = NULL,
                              cv = TRUE,
                              cv_horizon = 4,
                              initial_window = NULL, #length(y) - max_lag -  cv_horizon * 2,
                              fixed_window = FALSE,
                              verbose = TRUE,
                              allow_parallel = FALSE,
                              ...) {

  object_data <- prepare_data(y = y, p = p, P = P, xreg = xreg)
  training_method <- "timeslice"

  if (!cv) {
    training_method <- "none"
    if (is.null(tune_grid)) {
      stop("Only one model should be specified in tune_grid with no resampling")
    }
  }
  if(is.null(initial_window)){
    initial_window <- length(object_data[["y"]]) - object_data[["max_lag"]] -  cv_horizon * 2
  }
  model <- caret::train(
    x = object_data[["x"]],
    y = c(object_data[["y"]]),
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
    tuneGrid = tune_grid,
    ...
  )
  fitted_y <- predict(model, newdata = object_data[["x"]])
  fitted_y <- c(rep(NA, object_data[["max_lag"]]), fitted_y)
  fitted_y <- ts(fitted_y,
                 frequency = object_data[["frequency"]],
                 start = min(time(object_data[["y"]])))
  method <- paste0("ARml(", object_data[["p"]], paste(", ", object_data[["P"]]), ")")

  output <- list(
    y =  y,
    xreg = !is.null(xreg),
    y_modified = object_data[["y"]],
    x = object_data[["x"]],
    model = model,
    fitted = fitted_y,
    max_lag = object_data[["max_lag"]],
    method = paste0("CARET ", caret_method, " with ", method),
    lags = object_data[["lags"]]
  )
  class(output) <- "ARml"
  return(output)
}
